# ============================================================================
# MODULE P5_01: SCENARIO DEFINITION & SPATIAL ALLOCATION
# ============================================================================
# PURPOSE: Define restoration/management scenarios spatially using actual 
#          site predictions from Parts 1-4
#
# APPROACH:
#   1. Load spatial carbon stock predictions (baseline condition)
#   2. Define management zones (spatial units for restoration)
#   3. Assign restoration actions to each zone
#   4. Calculate baseline carbon stocks per zone
#   5. Export scenario structure for trajectory modeling
#
# INPUTS:
#   - outputs/predictions/rf/carbon_stock_rf_*.tif (from Module 05)
#     OR outputs/predictions/kriging/carbon_stock_*.tif (from Module 04)
#   - management_zones.shp (optional - user-defined zones)
#   - restoration_actions.csv (defines what actions are possible)
#
# OUTPUTS:
#   - scenarios/SCENARIO_NAME/baseline_stocks_total.tif
#   - scenarios/SCENARIO_NAME/baseline_stocks_by_depth.tif (multi-band)
#   - scenarios/SCENARIO_NAME/management_zones.shp
#   - scenarios/SCENARIO_NAME/zone_summaries.csv
#   - scenarios/SCENARIO_NAME/scenario_definition.yaml
#
# WORKFLOW:
#   This module sets up the "playing field" for scenario modeling.
#   It takes your actual spatial predictions and divides the landscape
#   into management units, then calculates current carbon stocks per unit.
#   
#   Next module (P5_02) will project future stocks under different actions.
#
# ============================================================================

# Clear environment
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(yaml)
  library(ggplot2)
  
  # Try to load exactextractr, install if missing
  if (!require(exactextractr, quietly = TRUE)) {
    cat("Installing exactextractr package...\n")
    install.packages("exactextractr")
    library(exactextractr)
  }
})

# Load configuration
if (file.exists("blue_carbon_config.R")) {
  source("blue_carbon_config.R")
} else {
  stop("Configuration file not found.")
}

# Load Part 5 scenario configuration
if (file.exists("P5_scenario_config.R")) {
  source("P5_scenario_config.R")
  cat("Loaded Part 5 scenario configuration\n")
} else {
  stop("P5_scenario_config.R not found. Please create this file first.")
}

# ============================================================================
# SETUP LOGGING
# ============================================================================

log_file <- file.path("logs", paste0("scenario_definition_", Sys.Date(), ".log"))
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_entry <- sprintf("%s %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE P5_01: SCENARIO DEFINITION & SPATIAL ALLOCATION ===")
log_message(sprintf("Project: %s", PROJECT_NAME))

# Use settings from P5_scenario_config.R
SCENARIO_NAME <- BASELINE_SCENARIO_NAME

log_message(sprintf("Baseline method: %s", BASELINE_METHOD))
log_message(sprintf("Scenario name: %s", SCENARIO_NAME))
log_message(sprintf("Zone definition: %s", ZONE_DEFINITION_METHOD))

# ============================================================================
# CREATE OUTPUT DIRECTORIES
# ============================================================================

scenario_dir <- file.path("scenarios", SCENARIO_NAME)
dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scenario_dir, "rasters"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scenario_dir, "vectors"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scenario_dir, "tables"), recursive = TRUE, showWarnings = FALSE)

log_message(sprintf("Output directory: %s", scenario_dir))

# ============================================================================
# STEP 1: LOAD BASELINE CARBON STOCK PREDICTIONS
# ============================================================================

log_message("\n=== STEP 1: Loading Baseline Carbon Stocks ===")

# Determine which prediction files to load
if (BASELINE_METHOD == "rf") {
  pred_dir <- "outputs/predictions/rf"
  pattern <- "^carbon_stock_rf_.*\\.tif$"
} else if (BASELINE_METHOD == "kriging") {
  pred_dir <- "outputs/predictions/kriging"
  pattern <- "^carbon_stock_.*\\.tif$"
} else {
  stop("BASELINE_METHOD must be either 'rf' or 'kriging'")
}

# Find all depth rasters
depth_files <- list.files(pred_dir, pattern = pattern, full.names = TRUE)

if (length(depth_files) == 0) {
  stop(sprintf("No prediction files found in %s with pattern '%s'.\nPlease run Module 04 (Kriging) or Module 05 (Random Forest) first.",
               pred_dir, pattern))
}

log_message(sprintf("Found %d depth layers:", length(depth_files)))

# Load all depths
depth_rasters <- list()
depth_labels <- c()

for (file in depth_files) {
  # Extract depth from filename
  depth_cm <- as.numeric(gsub(".*_(\\d+)cm\\.tif$", "\\1", basename(file)))
  
  r <- rast(file)
  depth_rasters[[as.character(depth_cm)]] <- r
  depth_labels <- c(depth_labels, sprintf("%d cm", depth_cm))
  
  log_message(sprintf("  - %d cm: %s", depth_cm, basename(file)))
}

# Calculate total carbon stock (sum across depths)
log_message("\nCalculating total carbon stock (0-100 cm)...")

# Stack all depths
depth_stack <- rast(depth_rasters)
names(depth_stack) <- paste0("depth_", names(depth_rasters), "cm")

# Sum to get total (all in kg/m²)
total_stock <- sum(depth_stack, na.rm = TRUE)
names(total_stock) <- "total_carbon_stock_kg_m2"

# Convert to Mg/ha for easier interpretation
total_stock_Mg_ha <- total_stock * 10
names(total_stock_Mg_ha) <- "total_carbon_stock_Mg_ha"

# Get summary statistics
total_stats <- global(total_stock_Mg_ha, c("min", "max", "mean", "sd"), na.rm = TRUE)
log_message(sprintf("Total carbon stock (0-100 cm):"))
log_message(sprintf("  Mean: %.1f Mg C/ha", total_stats[1, "mean"]))
log_message(sprintf("  Range: %.1f - %.1f Mg C/ha", total_stats[1, "min"], total_stats[1, "max"]))
log_message(sprintf("  SD: %.1f Mg C/ha", total_stats[1, "sd"]))

# Save baseline rasters
log_message("\nSaving baseline rasters...")

writeRaster(total_stock_Mg_ha, 
            file.path(scenario_dir, "rasters", "baseline_stocks_total.tif"),
            overwrite = TRUE)
log_message("  Saved: baseline_stocks_total.tif")

writeRaster(depth_stack * 10,  # Convert to Mg/ha
            file.path(scenario_dir, "rasters", "baseline_stocks_by_depth.tif"),
            overwrite = TRUE)
log_message("  Saved: baseline_stocks_by_depth.tif (multi-band)")

# ============================================================================
# STEP 2: DEFINE MANAGEMENT ZONES
# ============================================================================

log_message("\n=== STEP 2: Defining Management Zones ===")

zones_sf <- NULL

if (ZONE_DEFINITION_METHOD == "auto_stratum") {
  
  log_message("Using ecosystem strata as management zones...")
  
  # Look for stratum rasters from Module 05
  stratum_dir <- "data_processed"
  stratum_file <- file.path(stratum_dir, "strata_raster.tif")
  
  if (!file.exists(stratum_file)) {
    # Try alternate location
    stratum_file <- file.path("outputs/predictions/rf", "strata_mask.tif")
  }
  
  if (file.exists(stratum_file)) {
    log_message(sprintf("  Loading: %s", stratum_file))
    strata_rast <- rast(stratum_file)
    
    # Resample to match carbon stock raster if needed
    if (!compareGeom(strata_rast, total_stock_Mg_ha, stopOnError = FALSE)) {
      log_message("  Resampling strata to match carbon raster...")
      strata_rast <- resample(strata_rast, total_stock_Mg_ha, method = "near")
    }
    
    # Convert to polygons
    log_message("  Converting strata to polygons...")
    zones_sf <- as.polygons(strata_rast) %>% st_as_sf()
    
    # Add stratum names if available
    if (exists("VALID_STRATA")) {
      if (ncol(zones_sf) > 1 && is.numeric(zones_sf[[1]])) {
        stratum_codes <- zones_sf[[1]]
        zones_sf$stratum <- VALID_STRATA[stratum_codes]
      }
    }
    
  } else {
    log_message("  No stratum raster found, falling back to single zone", "WARNING")
    ZONE_DEFINITION_METHOD <- "single_zone"
  }
}

if (ZONE_DEFINITION_METHOD == "auto_grid") {
  
  log_message(sprintf("Creating regular grid (cell size = %d m)...", GRID_CELL_SIZE))
  
  # Create grid
  ext_bbox <- ext(total_stock_Mg_ha)
  grid_rast <- rast(total_stock_Mg_ha)
  res(grid_rast) <- GRID_CELL_SIZE
  
  # Assign unique IDs to each cell
  values(grid_rast) <- 1:ncell(grid_rast)
  
  # Mask to data extent
  grid_rast <- mask(grid_rast, total_stock_Mg_ha)
  
  # Convert to polygons
  zones_sf <- as.polygons(grid_rast) %>% st_as_sf()
  zones_sf$zone_id <- paste0("GRID_", zones_sf[[1]])
  
  log_message(sprintf("  Created %d grid cells", nrow(zones_sf)))
}

if (ZONE_DEFINITION_METHOD == "shapefile") {
  
  log_message(sprintf("Loading shapefile: %s", ZONE_SHAPEFILE_PATH))
  
  if (!file.exists(ZONE_SHAPEFILE_PATH)) {
    stop(sprintf("Shapefile not found: %s", ZONE_SHAPEFILE_PATH))
  }
  
  zones_sf <- st_read(ZONE_SHAPEFILE_PATH, quiet = TRUE)
  
  # Reproject to match raster if needed
  if (st_crs(zones_sf) != st_crs(total_stock_Mg_ha)) {
    log_message("  Reprojecting zones to match raster CRS...")
    zones_sf <- st_transform(zones_sf, crs = crs(total_stock_Mg_ha))
  }
  
  log_message(sprintf("  Loaded %d zones", nrow(zones_sf)))
}

if (ZONE_DEFINITION_METHOD == "single_zone") {
  
  log_message("Creating single management zone for entire site...")
  
  # Get extent of raster
  ext_bbox <- ext(total_stock_Mg_ha)
  
  # Create polygon directly from extent without using st_bbox
  # This avoids the NA check issues
  bbox_coords <- matrix(c(
    ext_bbox[1], ext_bbox[3],  # xmin, ymin
    ext_bbox[2], ext_bbox[3],  # xmax, ymin
    ext_bbox[2], ext_bbox[4],  # xmax, ymax
    ext_bbox[1], ext_bbox[4],  # xmin, ymax
    ext_bbox[1], ext_bbox[3]   # close polygon
  ), ncol = 2, byrow = TRUE)
  
  # Create polygon
  poly <- st_polygon(list(bbox_coords))
  
  # Convert to sf and set CRS
  zones_sf <- st_sf(geometry = st_sfc(poly))
  st_crs(zones_sf) <- st_crs(crs(total_stock_Mg_ha, proj = TRUE))
  
  # Add attributes
  zones_sf$zone_id <- "SITE_ALL"
  zones_sf$zone_name <- "Entire Site"
  
  log_message("  Created single zone for entire site")
}

# Clean up zone attributes
if (!"zone_id" %in% names(zones_sf)) {
  zones_sf$zone_id <- paste0("ZONE_", 1:nrow(zones_sf))
}

if (!"zone_name" %in% names(zones_sf)) {
  zones_sf$zone_name <- zones_sf$zone_id
}

# Calculate zone areas
zones_sf$area_m2 <- st_area(zones_sf) %>% as.numeric()
zones_sf$area_ha <- zones_sf$area_m2 / 10000

log_message(sprintf("\nZone statistics:"))
log_message(sprintf("  Total zones: %d", nrow(zones_sf)))
log_message(sprintf("  Total area: %.1f ha", sum(zones_sf$area_ha)))
log_message(sprintf("  Mean zone size: %.2f ha", mean(zones_sf$area_ha)))
log_message(sprintf("  Range: %.2f - %.2f ha", min(zones_sf$area_ha), max(zones_sf$area_ha)))

# Filter small zones
if (any(zones_sf$area_ha < MIN_ZONE_AREA_HA)) {
  n_small <- sum(zones_sf$area_ha < MIN_ZONE_AREA_HA)
  log_message(sprintf("\nRemoving %d zones smaller than %.2f ha...", n_small, MIN_ZONE_AREA_HA))
  zones_sf <- zones_sf %>% filter(area_ha >= MIN_ZONE_AREA_HA)
  log_message(sprintf("  Retained %d zones", nrow(zones_sf)))
}

# ============================================================================
# STEP 3: EXTRACT BASELINE CARBON STOCKS PER ZONE
# ============================================================================

log_message("\n=== STEP 3: Extracting Carbon Stocks per Zone ===")

# Extract total stocks
log_message("Extracting total carbon stocks (0-100 cm)...")

# Try exact_extract first, fallback to terra::extract if it fails
tryCatch({
  zone_stats_total <- exact_extract(total_stock_Mg_ha, zones_sf, 'mean', 
                                    progress = FALSE)
  zones_sf$baseline_carbon_mean_Mg_ha <- zone_stats_total
  
  zone_stats_sd <- exact_extract(total_stock_Mg_ha, zones_sf, 'stdev', 
                                 progress = FALSE)
  zones_sf$baseline_carbon_sd_Mg_ha <- zone_stats_sd
  
  log_message("  Using exact_extract for zonal statistics")
  
}, error = function(e) {
  
  log_message("  exact_extract failed, using terra::extract instead...", "WARNING")
  
  # Fallback to terra::extract
  zone_stats <- terra::extract(total_stock_Mg_ha, vect(zones_sf), 
                               fun = mean, na.rm = TRUE)
  zones_sf$baseline_carbon_mean_Mg_ha <- zone_stats[, 2]
  
  zone_stats_sd <- terra::extract(total_stock_Mg_ha, vect(zones_sf), 
                                  fun = sd, na.rm = TRUE)
  zones_sf$baseline_carbon_sd_Mg_ha <- zone_stats_sd[, 2]
})

# Calculate total carbon per zone
zones_sf$baseline_carbon_total_Mg <- zones_sf$baseline_carbon_mean_Mg_ha * zones_sf$area_ha

# Extract by depth interval (for VM0033 compliance)
log_message("Extracting carbon by VM0033 depth intervals...")

# VM0033 requires 4 intervals: 0-15, 15-30, 30-50, 50-100 cm
# Match our depths to these intervals
vm0033_intervals <- data.frame(
  interval_name = c("0-15cm", "15-30cm", "30-50cm", "50-100cm"),
  depth_start = c(0, 15, 30, 50),
  depth_end = c(15, 30, 50, 100)
)

# Find which depth rasters contribute to each interval
for (i in 1:nrow(vm0033_intervals)) {
  interval_name <- vm0033_intervals$interval_name[i]
  start_depth <- vm0033_intervals$depth_start[i]
  end_depth <- vm0033_intervals$depth_end[i]
  
  log_message(sprintf("  Processing interval: %s", interval_name))
  
  # Find depth layers in this interval
  depth_values <- as.numeric(names(depth_rasters))
  relevant_depths <- depth_values[depth_values >= start_depth & depth_values < end_depth]
  
  if (length(relevant_depths) > 0) {
    # Sum relevant depths
    interval_rast <- sum(rast(depth_rasters[as.character(relevant_depths)]), na.rm = TRUE) * 10
    
    # Extract per zone with fallback
    tryCatch({
      interval_stats <- exact_extract(interval_rast, zones_sf, 'mean', progress = FALSE)
    }, error = function(e) {
      interval_stats <- terra::extract(interval_rast, vect(zones_sf), 
                                       fun = mean, na.rm = TRUE)[, 2]
    })
    
    zones_sf[[paste0("baseline_", gsub("-", "_", interval_name), "_Mg_ha")]] <- interval_stats
  }
}

# Display summary
log_message("\nZone carbon stock summary:")
for (i in 1:min(5, nrow(zones_sf))) {
  log_message(sprintf("  %s: %.1f ± %.1f Mg C/ha (%.1f Mg total)",
                      zones_sf$zone_name[i],
                      zones_sf$baseline_carbon_mean_Mg_ha[i],
                      zones_sf$baseline_carbon_sd_Mg_ha[i],
                      zones_sf$baseline_carbon_total_Mg[i]))
}
if (nrow(zones_sf) > 5) {
  log_message(sprintf("  ... and %d more zones", nrow(zones_sf) - 5))
}

# ============================================================================
# STEP 4: ASSIGN DEFAULT RESTORATION ACTIONS
# ============================================================================

log_message("\n=== STEP 4: Setting Up Restoration Actions ===")

# Check if restoration actions file exists
actions_file <- "restoration_actions.csv"

if (file.exists(actions_file)) {
  log_message(sprintf("Loading restoration actions from: %s", actions_file))
  restoration_actions <- read_csv(actions_file, show_col_types = FALSE)
} else {
  log_message("No restoration_actions.csv found, creating template...")
  
  # Create template
  restoration_actions <- data.frame(
    action_id = c("NO_ACTION", "RESTORE_HYDRO", "PLANT_NATIVE", "REMOVE_INVASIVE", "THIN_FOREST"),
    action_name = c(
      "No Action (Baseline)",
      "Restore Hydrology",
      "Plant Native Vegetation",
      "Remove Invasive Species",
      "Thin Forest Canopy"
    ),
    description = c(
      "Maintain current condition (baseline scenario)",
      "Remove dikes, reconnect tidal flow, restore natural hydrology",
      "Plant native salt marsh or seagrass species",
      "Remove invasive species (e.g., Spartina, Phragmites)",
      "Thin forest canopy to reduce shading"
    ),
    applies_to_ecosystem = c(
      "all",
      "salt_marsh,tidal_flat",
      "salt_marsh,seagrass",
      "salt_marsh",
      "forested_wetland"
    ),
    cost_per_ha = c(0, 15000, 8000, 5000, 3000),
    years_to_effect = c(0, 2, 3, 1, 1),
    notes = c(
      "Baseline - no intervention",
      "Major engineering - dike removal, channel restoration",
      "Medium intensity - includes site prep and planting",
      "Low intensity - mechanical or herbicide treatment",
      "Low intensity - selective thinning only"
    )
  )
  
  write_csv(restoration_actions, actions_file)
  log_message(sprintf("  Created template: %s", actions_file))
  log_message("  Edit this file to define your restoration options")
}

log_message(sprintf("\nAvailable actions (%d):", nrow(restoration_actions)))
for (i in 1:nrow(restoration_actions)) {
  log_message(sprintf("  %s: %s ($%.0f/ha)",
                      restoration_actions$action_id[i],
                      restoration_actions$action_name[i],
                      restoration_actions$cost_per_ha[i]))
}

# Assign default action (NO_ACTION) to all zones
zones_sf$action_id <- "NO_ACTION"
zones_sf$action_name <- "No Action (Baseline)"

# ============================================================================
# STEP 5: SAVE OUTPUTS
# ============================================================================

log_message("\n=== STEP 5: Saving Scenario Definition ===")

# Prepare zones for saving - shorten field names for shapefile compatibility
zones_save <- zones_sf

# Shorten field names to 10 characters (shapefile limit)
names(zones_save)[names(zones_save) == "baseline_carbon_mean_Mg_ha"] <- "base_C_avg"
names(zones_save)[names(zones_save) == "baseline_carbon_sd_Mg_ha"] <- "base_C_sd"
names(zones_save)[names(zones_save) == "baseline_carbon_total_Mg"] <- "base_C_tot"
names(zones_save)[names(zones_save) == "baseline_0_15cm_Mg_ha"] <- "base_0_15"
names(zones_save)[names(zones_save) == "baseline_15_30cm_Mg_ha"] <- "base_15_30"
names(zones_save)[names(zones_save) == "baseline_30_50cm_Mg_ha"] <- "base_30_50"
names(zones_save)[names(zones_save) == "baseline_50_100cm_Mg_ha"] <- "base_50_100"

# Save as GeoPackage (no field name limits) - preferred format
zones_gpkg <- file.path(scenario_dir, "vectors", "management_zones.gpkg")
st_write(zones_save, zones_gpkg, delete_dsn = TRUE, quiet = TRUE)
log_message(sprintf("Saved: %s", basename(zones_gpkg)))

# Also save as shapefile for compatibility
zones_shp <- file.path(scenario_dir, "vectors", "management_zones.shp")
st_write(zones_save, zones_shp, delete_dsn = TRUE, quiet = TRUE)
log_message(sprintf("Saved: %s (field names shortened for compatibility)", basename(zones_shp)))

# Save zone summaries table
zone_summary <- zones_sf %>%
  st_drop_geometry() %>%
  select(zone_id, zone_name, area_ha, 
         baseline_carbon_mean_Mg_ha, baseline_carbon_sd_Mg_ha,
         baseline_carbon_total_Mg,
         starts_with("baseline_0_15"),
         starts_with("baseline_15_30"),
         starts_with("baseline_30_50"),
         starts_with("baseline_50_100"),
         action_id, action_name)

write_csv(zone_summary, file.path(scenario_dir, "tables", "zone_summaries.csv"))
log_message("Saved: zone_summaries.csv")

# Create scenario definition YAML
scenario_def <- list(
  scenario_name = SCENARIO_NAME,
  created_date = as.character(Sys.Date()),
  project_name = PROJECT_NAME,
  baseline_method = BASELINE_METHOD,
  zone_definition_method = ZONE_DEFINITION_METHOD,
  n_zones = nrow(zones_sf),
  total_area_ha = sum(zones_sf$area_ha),
  total_baseline_carbon_Mg = sum(zones_sf$baseline_carbon_total_Mg),
  mean_baseline_carbon_Mg_ha = mean(zones_sf$baseline_carbon_mean_Mg_ha),
  depth_intervals = vm0033_intervals$interval_name,
  files = list(
    baseline_raster = "rasters/baseline_stocks_total.tif",
    depth_rasters = "rasters/baseline_stocks_by_depth.tif",
    management_zones_gpkg = "vectors/management_zones.gpkg",
    management_zones_shp = "vectors/management_zones.shp",
    zone_summaries = "tables/zone_summaries.csv"
  ),
  notes = "GeoPackage (.gpkg) has full field names. Shapefile (.shp) has shortened names due to 10-char limit."
)

write_yaml(scenario_def, file.path(scenario_dir, "scenario_definition.yaml"))
log_message("Saved: scenario_definition.yaml")

# ============================================================================
# STEP 6: CREATE VISUALIZATION
# ============================================================================

log_message("\n=== STEP 6: Creating Visualization ===")

# Create summary plot
png(file.path(scenario_dir, "baseline_carbon_map.png"),
    width = 10, height = 8, units = "in", res = 300)

par(mfrow = c(1, 1), mar = c(4, 4, 3, 6))

plot(total_stock_Mg_ha,
     main = paste("Baseline Carbon Stocks -", SCENARIO_NAME),
     col = hcl.colors(100, "YlOrRd", rev = TRUE),
     axes = TRUE,
     legend = TRUE,
     mar = c(4, 4, 3, 6))

# Overlay management zones
plot(st_geometry(zones_sf), add = TRUE, border = "black", lwd = 2)

# Add zone labels for first few zones
if (nrow(zones_sf) <= 10) {
  centroids <- st_centroid(zones_sf)
  coords <- st_coordinates(centroids)
  text(coords[, "X"], coords[, "Y"], labels = zones_sf$zone_id,
       cex = 0.7, font = 2, col = "black")
}

dev.off()
log_message("Saved: baseline_carbon_map.png")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("SCENARIO DEFINITION COMPLETE\n")
cat("========================================\n\n")

cat(sprintf("Scenario: %s\n", SCENARIO_NAME))
cat(sprintf("Output directory: %s\n\n", scenario_dir))

cat("Baseline Carbon Stocks:\n")
cat(sprintf("  Total area: %.1f ha\n", sum(zones_sf$area_ha)))
cat(sprintf("  Total carbon: %.1f Mg C (%.1f tonnes CO2e)\n",
            sum(zones_sf$baseline_carbon_total_Mg),
            sum(zones_sf$baseline_carbon_total_Mg) * 44/12))
cat(sprintf("  Mean density: %.1f ± %.1f Mg C/ha\n",
            mean(zones_sf$baseline_carbon_mean_Mg_ha),
            sd(zones_sf$baseline_carbon_mean_Mg_ha)))
cat(sprintf("  Range: %.1f - %.1f Mg C/ha\n\n",
            min(zones_sf$baseline_carbon_mean_Mg_ha),
            max(zones_sf$baseline_carbon_mean_Mg_ha)))

cat("Management Zones:\n")
cat(sprintf("  Number of zones: %d\n", nrow(zones_sf)))
cat(sprintf("  Zone definition method: %s\n", ZONE_DEFINITION_METHOD))
cat(sprintf("  Mean zone size: %.2f ha\n\n", mean(zones_sf$area_ha)))

cat("Output Files:\n")
cat("  📊 zone_summaries.csv - Baseline carbon per zone\n")
cat("  🗺️  management_zones.gpkg - Spatial units (GeoPackage, full names)\n")
cat("  🗺️  management_zones.shp - Spatial units (Shapefile, shortened names)\n")
cat("  🎨 baseline_carbon_map.png - Visualization\n")
cat("  🗂️  scenario_definition.yaml - Metadata\n")
cat("  📁 rasters/ - Baseline carbon stock rasters\n\n")

cat("Next Steps:\n")
cat("  1. Review zone_summaries.csv to understand baseline conditions\n")
cat("  2. Edit P5_scenario_config.R to set baseline and project actions\n")
cat("  3. Run P5_02_trajectory_modeling.R to model scenarios\n")
cat("  4. Run P5_03_vm0033_crediting.R to calculate credits\n\n")

cat("Customization Options:\n")
cat("  - Edit P5_scenario_config.R BASELINE_ACTION for baseline scenario\n")
cat("  - Edit P5_scenario_config.R UNIFORM_ACTION for project scenario\n")
cat("  - Use ZONE_DEFINITION_METHOD='shapefile' for custom zones\n\n")

log_message("=== MODULE P5_01 COMPLETE ===")