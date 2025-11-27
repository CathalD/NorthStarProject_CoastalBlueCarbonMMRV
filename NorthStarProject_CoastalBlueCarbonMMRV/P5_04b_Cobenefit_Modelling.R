# ============================================================================
# MODULE P5_04b: CO-BENEFITS MODELING & SPATIAL ANALYSIS
# ============================================================================
# PURPOSE: Quantify and map ecosystem co-benefits from restoration scenarios
#
# CO-BENEFITS ANALYZED:
#   1. BIODIVERSITY
#      - Habitat suitability for key species
#      - Structural complexity (vegetation diversity)
#      - Connectivity to reference sites
#
#   2. FLOOD PROTECTION
#      - Wave attenuation capacity
#      - Storm surge buffer
#      - Sediment stabilization
#
#   3. WATER QUALITY
#      - Nitrogen removal capacity
#      - Phosphorus removal capacity
#      - Sediment filtration
#
#   4. ADDITIONAL BENEFITS
#      - Recreation/access
#      - Cultural/aesthetic value
#      - Fish nursery habitat
#
# METHODS:
#   - Spatial overlay analysis
#   - Distance-based scoring
#   - Ecosystem service modeling
#   - Species distribution models (if data available)
#
# OUTPUTS:
#   - scenarios/cobenefits/biodiversity_score.tif
#   - scenarios/cobenefits/flood_protection_score.tif
#   - scenarios/cobenefits/water_quality_score.tif
#   - scenarios/cobenefits/cobenefit_summary_by_zone.csv
#   - scenarios/cobenefits/cobenefit_maps.png
#
# ============================================================================

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
})

# Load configuration
source("blue_carbon_config.R")
source("P5_scenario_config.R")

# Setup logging
log_file <- file.path("logs", paste0("cobenefit_modeling_", Sys.Date(), ".log"))
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_entry <- sprintf("%s %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

cat("\n========================================\n")
cat("CO-BENEFITS SPATIAL MODELING\n")
cat("========================================\n\n")

log_message("=== MODULE P5_04b: CO-BENEFITS ANALYSIS ===")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Output directory
cobenefit_dir <- "scenarios/cobenefits"
dir.create(cobenefit_dir, recursive = TRUE, showWarnings = FALSE)

# Co-benefit modeling parameters
BIODIVERSITY_PARAMS <- list(
  # Distance to reference sites (m)
  reference_buffer = 500,
  # Vegetation complexity weights
  weight_native_veg = 0.4,
  weight_structural_diversity = 0.3,
  weight_connectivity = 0.3
)

FLOOD_PROTECTION_PARAMS <- list(
  # Wave attenuation factors
  marsh_roughness = 0.15,  # Manning's n for salt marsh
  wave_reduction_per_m = 0.02,  # Wave height reduction per meter
  # Elevation importance
  optimal_elevation_range = c(-0.5, 0.5),  # meters relative to MHW
  elevation_weight = 0.5
)

WATER_QUALITY_PARAMS <- list(
  # Nutrient removal rates (g/m2/yr)
  nitrogen_removal_rate = 15,  # g N/m2/yr for salt marsh
  phosphorus_removal_rate = 2,  # g P/m2/yr
  # Sediment trapping efficiency
  sediment_trap_efficiency = 0.7  # 70% of suspended sediment
)

# ============================================================================
# LOAD SPATIAL DATA
# ============================================================================

log_message("\n=== STEP 1: Loading Spatial Data ===")

# Load baseline scenario structure
baseline_dir <- file.path("scenarios", BASELINE_SCENARIO_NAME)
if (!dir.exists(baseline_dir)) {
  stop("Baseline scenario not found. Please run P5_01 first.")
}

# Load management zones
zones_path <- file.path(baseline_dir, "vectors", "management_zones.gpkg")
if (!file.exists(zones_path)) {
  zones_path <- file.path(baseline_dir, "vectors", "management_zones.shp")
}

zones_sf <- st_read(zones_path, quiet = TRUE)
log_message(sprintf("Loaded %d management zones", nrow(zones_sf)))

# Load baseline carbon raster (for template)
baseline_raster_path <- file.path(baseline_dir, "rasters", "baseline_stocks_total.tif")
if (!file.exists(baseline_raster_path)) {
  stop("Baseline raster not found. Please run P5_01 first.")
}

template_raster <- rast(baseline_raster_path)
log_message("Loaded template raster")

# Load restoration actions
actions <- read_csv("restoration_actions.csv", show_col_types = FALSE)

# ============================================================================
# STEP 2: BIODIVERSITY CO-BENEFIT MODELING
# ============================================================================

log_message("\n=== STEP 2: Modeling Biodiversity Co-Benefits ===")

cat("\n========================================\n")
cat("BIODIVERSITY ANALYSIS\n")
cat("========================================\n\n")

# Initialize biodiversity score raster
biodiversity_score <- template_raster * 0
names(biodiversity_score) <- "biodiversity_score"

# Check if reference sites exist
reference_sites_path <- "data_raw/reference_sites.shp"
has_reference_sites <- file.exists(reference_sites_path)

if (has_reference_sites) {
  log_message("Loading reference sites for connectivity analysis")
  reference_sites <- st_read(reference_sites_path, quiet = TRUE)
  
  # Calculate distance to nearest reference site
  zones_sf$dist_to_reference <- st_distance(zones_sf, reference_sites) %>%
    apply(1, min)
  
  # Score: inverse distance (closer = better)
  max_dist <- BIODIVERSITY_PARAMS$reference_buffer
  zones_sf$connectivity_score <- pmax(0, 1 - (zones_sf$dist_to_reference / max_dist))
  
} else {
  log_message("No reference sites found - using default connectivity score", "WARNING")
  zones_sf$connectivity_score <- 0.5  # Neutral score
}

# Assign biodiversity potential based on restoration action
zones_sf$biodiversity_potential <- 0

for (i in 1:nrow(zones_sf)) {
  
  # Get action(s) for this zone
  action_id <- if ("action_id" %in% names(zones_sf)) {
    zones_sf$action_id[i]
  } else {
    "NO_ACTION"
  }
  
  # Parse multiple actions
  action_ids <- trimws(strsplit(as.character(action_id), "[,;|]")[[1]])
  
  # Score biodiversity potential based on action type
  biodiv_scores <- sapply(action_ids, function(aid) {
    if (aid == "NO_ACTION") {
      return(0.2)  # Baseline
    } else if (grepl("PLANT_NATIVE|PLANT_SEAGRASS", aid)) {
      return(0.9)  # High biodiversity benefit
    } else if (grepl("REMOVE_INVASIVE|THIN_INVASIVE", aid)) {
      return(0.8)  # High benefit - removes competition
    } else if (grepl("RESTORE_HYDRO|RECONNECT", aid)) {
      return(0.7)  # Moderate-high - enables colonization
    } else if (grepl("CEASE_MOWING|GRAZE", aid)) {
      return(0.6)  # Moderate - allows natural succession
    } else if (grepl("SEDIMENT|FILL", aid)) {
      return(0.5)  # Moderate - habitat creation
    } else {
      return(0.4)  # Default moderate benefit
    }
  })
  
  # Take maximum if multiple actions
  zones_sf$biodiversity_potential[i] <- max(biodiv_scores)
}

# Calculate composite biodiversity score
zones_sf$biodiversity_score <- (
  zones_sf$biodiversity_potential * BIODIVERSITY_PARAMS$weight_native_veg +
    zones_sf$connectivity_score * BIODIVERSITY_PARAMS$weight_connectivity +
    0.5 * BIODIVERSITY_PARAMS$weight_structural_diversity  # Placeholder for structure
)

# Rasterize biodiversity score
biodiversity_raster <- rasterize(
  vect(zones_sf),
  template_raster,
  field = "biodiversity_score",
  fun = "max"
)
names(biodiversity_raster) <- "biodiversity_score"

# Save raster
writeRaster(biodiversity_raster, 
            file.path(cobenefit_dir, "biodiversity_score.tif"),
            overwrite = TRUE)
log_message("Saved: biodiversity_score.tif")

cat("Biodiversity Scores:\n")
cat(sprintf("  Mean: %.2f\n", mean(zones_sf$biodiversity_score, na.rm = TRUE)))
cat(sprintf("  Range: %.2f - %.2f\n\n", 
            min(zones_sf$biodiversity_score, na.rm = TRUE),
            max(zones_sf$biodiversity_score, na.rm = TRUE)))

# ============================================================================
# STEP 3: FLOOD PROTECTION CO-BENEFIT MODELING
# ============================================================================

log_message("\n=== STEP 3: Modeling Flood Protection Co-Benefits ===")

cat("\n========================================\n")
cat("FLOOD PROTECTION ANALYSIS\n")
cat("========================================\n\n")

# Check if elevation data exists
elevation_path <- "data_processed/elevation.tif"
has_elevation <- file.exists(elevation_path)

if (has_elevation) {
  log_message("Loading elevation data for flood protection analysis")
  elevation_rast <- rast(elevation_path)
  
  # Extract mean elevation per zone
  zones_sf$mean_elevation <- extract(elevation_rast, vect(zones_sf), fun = "mean", na.rm = TRUE)[,2]
  
  # Score based on optimal elevation for marsh
  optimal_range <- FLOOD_PROTECTION_PARAMS$optimal_elevation_range
  zones_sf$elevation_score <- ifelse(
    zones_sf$mean_elevation >= optimal_range[1] & zones_sf$mean_elevation <= optimal_range[2],
    1.0,  # Optimal elevation
    pmax(0, 1 - abs(zones_sf$mean_elevation - mean(optimal_range)) / 2)  # Decay with distance
  )
  
} else {
  log_message("No elevation data found - using default elevation score", "WARNING")
  zones_sf$elevation_score <- 0.6  # Neutral-positive score
}

# Score wave attenuation potential based on restoration action
zones_sf$wave_attenuation_potential <- 0

for (i in 1:nrow(zones_sf)) {
  
  action_id <- if ("action_id" %in% names(zones_sf)) {
    zones_sf$action_id[i]
  } else {
    "NO_ACTION"
  }
  
  action_ids <- trimws(strsplit(as.character(action_id), "[,;|]")[[1]])
  
  # Score based on vegetation structure and density
  wave_scores <- sapply(action_ids, function(aid) {
    if (aid == "NO_ACTION") {
      return(0.3)  # Some baseline protection
    } else if (grepl("PLANT_NATIVE|PLANT_SEAGRASS", aid)) {
      return(0.9)  # High wave attenuation from dense vegetation
    } else if (grepl("RESTORE_HYDRO|RECONNECT|SEDIMENT", aid)) {
      return(0.8)  # High - restores elevation and allows colonization
    } else if (grepl("CONTROL_EROSION|REMOVE_FILL", aid)) {
      return(0.7)  # Moderate-high - stabilizes substrate
    } else if (grepl("REMOVE_INVASIVE", aid)) {
      return(0.6)  # Moderate - depends on what replaces it
    } else {
      return(0.5)  # Default moderate benefit
    }
  })
  
  zones_sf$wave_attenuation_potential[i] <- max(wave_scores)
}

# Calculate composite flood protection score
zones_sf$flood_protection_score <- (
  zones_sf$wave_attenuation_potential * (1 - FLOOD_PROTECTION_PARAMS$elevation_weight) +
    zones_sf$elevation_score * FLOOD_PROTECTION_PARAMS$elevation_weight
)

# Rasterize flood protection score
flood_protection_raster <- rasterize(
  vect(zones_sf),
  template_raster,
  field = "flood_protection_score",
  fun = "max"
)
names(flood_protection_raster) <- "flood_protection_score"

writeRaster(flood_protection_raster,
            file.path(cobenefit_dir, "flood_protection_score.tif"),
            overwrite = TRUE)
log_message("Saved: flood_protection_score.tif")

cat("Flood Protection Scores:\n")
cat(sprintf("  Mean: %.2f\n", mean(zones_sf$flood_protection_score, na.rm = TRUE)))
cat(sprintf("  Range: %.2f - %.2f\n\n",
            min(zones_sf$flood_protection_score, na.rm = TRUE),
            max(zones_sf$flood_protection_score, na.rm = TRUE)))

# ============================================================================
# STEP 4: WATER QUALITY CO-BENEFIT MODELING
# ============================================================================

log_message("\n=== STEP 4: Modeling Water Quality Co-Benefits ===")

cat("\n========================================\n")
cat("WATER QUALITY ANALYSIS\n")
cat("========================================\n\n")

# Score nutrient removal potential based on restoration action and vegetation type
zones_sf$nutrient_removal_potential <- 0

for (i in 1:nrow(zones_sf)) {
  
  action_id <- if ("action_id" %in% names(zones_sf)) {
    zones_sf$action_id[i]
  } else {
    "NO_ACTION"
  }
  
  action_ids <- trimws(strsplit(as.character(action_id), "[,;|]")[[1]])
  
  # Score based on vegetation density and root structure
  nutrient_scores <- sapply(action_ids, function(aid) {
    if (aid == "NO_ACTION") {
      return(0.3)  # Some baseline filtration
    } else if (grepl("PLANT_NATIVE|PLANT_SEAGRASS", aid)) {
      return(0.95)  # Excellent - dense root systems for N/P uptake
    } else if (grepl("RESTORE_HYDRO|RECONNECT", aid)) {
      return(0.85)  # High - increases water residence time
    } else if (grepl("REMOVE_INVASIVE", aid)) {
      return(0.75)  # Moderate-high - depends on replacement vegetation
    } else if (grepl("SEDIMENT|NUTRIENT_MANAGE", aid)) {
      return(0.7)  # Moderate-high - physical and biological processes
    } else if (grepl("CEASE_MOWING|GRAZE", aid)) {
      return(0.65)  # Moderate - allows denser vegetation
    } else {
      return(0.5)  # Default moderate benefit
    }
  })
  
  zones_sf$nutrient_removal_potential[i] <- max(nutrient_scores)
}

# Calculate sediment trapping potential
# Higher for restoration actions that increase surface roughness
zones_sf$sediment_trap_potential <- zones_sf$nutrient_removal_potential * 0.9

# Calculate composite water quality score
zones_sf$water_quality_score <- (
  zones_sf$nutrient_removal_potential * 0.6 +
    zones_sf$sediment_trap_potential * 0.4
)

# Rasterize water quality score
water_quality_raster <- rasterize(
  vect(zones_sf),
  template_raster,
  field = "water_quality_score",
  fun = "max"
)
names(water_quality_raster) <- "water_quality_score"

writeRaster(water_quality_raster,
            file.path(cobenefit_dir, "water_quality_score.tif"),
            overwrite = TRUE)
log_message("Saved: water_quality_score.tif")

cat("Water Quality Scores:\n")
cat(sprintf("  Mean: %.2f\n", mean(zones_sf$water_quality_score, na.rm = TRUE)))
cat(sprintf("  Range: %.2f - %.2f\n\n",
            min(zones_sf$water_quality_score, na.rm = TRUE),
            max(zones_sf$water_quality_score, na.rm = TRUE)))

# ============================================================================
# STEP 5: QUANTIFY CO-BENEFIT PROVISION
# ============================================================================

log_message("\n=== STEP 5: Quantifying Co-Benefit Provision ===")

cat("\n========================================\n")
cat("CO-BENEFIT QUANTIFICATION\n")
cat("========================================\n\n")

# Calculate annual co-benefit provision

# BIODIVERSITY: Habitat area (ha)
zones_sf$biodiversity_ha <- zones_sf$area_ha * zones_sf$biodiversity_score

# FLOOD PROTECTION: Protected shoreline length (m) or area (ha)
zones_sf$flood_protection_ha <- zones_sf$area_ha * zones_sf$flood_protection_score

# WATER QUALITY: Nutrient removal (kg N/yr and kg P/yr)
zones_sf$nitrogen_removal_kg_yr <- zones_sf$area_ha * 10000 *  # m2
  zones_sf$water_quality_score * 
  WATER_QUALITY_PARAMS$nitrogen_removal_rate / 1000  # Convert g to kg

zones_sf$phosphorus_removal_kg_yr <- zones_sf$area_ha * 10000 *  # m2
  zones_sf$water_quality_score * 
  WATER_QUALITY_PARAMS$phosphorus_removal_rate / 1000  # Convert g to kg

# Summarize total provision
total_biodiversity_ha <- sum(zones_sf$biodiversity_ha, na.rm = TRUE)
total_flood_ha <- sum(zones_sf$flood_protection_ha, na.rm = TRUE)
total_n_removal_kg <- sum(zones_sf$nitrogen_removal_kg_yr, na.rm = TRUE)
total_p_removal_kg <- sum(zones_sf$phosphorus_removal_kg_yr, na.rm = TRUE)

cat("Annual Co-Benefit Provision:\n\n")
cat("BIODIVERSITY:\n")
cat(sprintf("  High-quality habitat: %.1f ha\n", total_biodiversity_ha))
cat(sprintf("  Mean habitat quality score: %.2f\n\n", 
            mean(zones_sf$biodiversity_score, na.rm = TRUE)))

cat("FLOOD PROTECTION:\n")
cat(sprintf("  Protected area: %.1f ha\n", total_flood_ha))
cat(sprintf("  Mean protection score: %.2f\n\n",
            mean(zones_sf$flood_protection_score, na.rm = TRUE)))

cat("WATER QUALITY:\n")
cat(sprintf("  Nitrogen removal: %.0f kg N/yr\n", total_n_removal_kg))
cat(sprintf("  Phosphorus removal: %.0f kg P/yr\n", total_p_removal_kg))
cat(sprintf("  Mean water quality score: %.2f\n\n",
            mean(zones_sf$water_quality_score, na.rm = TRUE)))

# ============================================================================
# STEP 6: SAVE ZONE-LEVEL RESULTS
# ============================================================================

log_message("\n=== STEP 6: Saving Zone-Level Co-Benefit Data ===")

# Extract key fields
cobenefit_summary <- zones_sf %>%
  st_drop_geometry() %>%
  select(
    zone_id,
    area_ha,
    biodiversity_score,
    flood_protection_score,
    water_quality_score,
    biodiversity_ha,
    flood_protection_ha,
    nitrogen_removal_kg_yr,
    phosphorus_removal_kg_yr
  )

# Calculate composite score using configured weights
cobenefit_summary <- cobenefit_summary %>%
  mutate(
    composite_score = 
      biodiversity_score * WEIGHT_BIODIVERSITY +
      flood_protection_score * WEIGHT_FLOOD_PROTECTION +
      water_quality_score * WEIGHT_WATER_QUALITY
  )

# Save
write_csv(cobenefit_summary, 
          file.path(cobenefit_dir, "cobenefit_summary_by_zone.csv"))
log_message("Saved: cobenefit_summary_by_zone.csv")

# Also add to zones shapefile
zones_sf_export <- zones_sf %>%
  select(zone_id, area_ha,
         biodiversity_score, flood_protection_score, water_quality_score,
         biodiversity_ha, flood_protection_ha,
         nitrogen_removal_kg_yr, phosphorus_removal_kg_yr)

st_write(zones_sf_export,
         file.path(cobenefit_dir, "zones_with_cobenefits.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)
log_message("Saved: zones_with_cobenefits.gpkg")

# ============================================================================
# STEP 7: CREATE VISUALIZATION
# ============================================================================

log_message("\n=== STEP 7: Creating Co-Benefit Maps ===")

# Create multi-panel map (if possible with simple plotting)
cat("\nCreating co-benefit maps...\n")

# Create simple plots showing score distributions
p1 <- ggplot(cobenefit_summary, aes(x = biodiversity_score)) +
  geom_histogram(bins = 20, fill = "forestgreen", alpha = 0.7) +
  labs(title = "Biodiversity Scores", x = "Score (0-1)", y = "Count") +
  theme_minimal()

p2 <- ggplot(cobenefit_summary, aes(x = flood_protection_score)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  labs(title = "Flood Protection Scores", x = "Score (0-1)", y = "Count") +
  theme_minimal()

p3 <- ggplot(cobenefit_summary, aes(x = water_quality_score)) +
  geom_histogram(bins = 20, fill = "cyan4", alpha = 0.7) +
  labs(title = "Water Quality Scores", x = "Score (0-1)", y = "Count") +
  theme_minimal()

# Scatterplot: biodiversity vs flood protection
p4 <- ggplot(cobenefit_summary, aes(x = biodiversity_score, y = flood_protection_score)) +
  geom_point(aes(size = area_ha), alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Biodiversity vs Flood Protection",
       x = "Biodiversity Score", y = "Flood Protection Score",
       size = "Area (ha)") +
  theme_minimal()

# Save plots
ggsave(file.path(cobenefit_dir, "biodiversity_distribution.png"),
       p1, width = 8, height = 6, dpi = 300)
ggsave(file.path(cobenefit_dir, "flood_protection_distribution.png"),
       p2, width = 8, height = 6, dpi = 300)
ggsave(file.path(cobenefit_dir, "water_quality_distribution.png"),
       p3, width = 8, height = 6, dpi = 300)
ggsave(file.path(cobenefit_dir, "biodiversity_vs_flood.png"),
       p4, width = 8, height = 6, dpi = 300)

log_message("Saved: cobenefit distribution plots")

# ============================================================================
# STEP 8: CREATE SUMMARY REPORT
# ============================================================================

log_message("\n=== STEP 8: Creating Summary Report ===")

# Create YAML summary
cobenefit_report <- list(
  analysis_date = as.character(Sys.Date()),
  total_area_ha = sum(zones_sf$area_ha),
  
  biodiversity = list(
    mean_score = mean(zones_sf$biodiversity_score, na.rm = TRUE),
    sd_score = sd(zones_sf$biodiversity_score, na.rm = TRUE),
    total_high_quality_ha = total_biodiversity_ha,
    parameters = BIODIVERSITY_PARAMS
  ),
  
  flood_protection = list(
    mean_score = mean(zones_sf$flood_protection_score, na.rm = TRUE),
    sd_score = sd(zones_sf$flood_protection_score, na.rm = TRUE),
    total_protected_ha = total_flood_ha,
    parameters = FLOOD_PROTECTION_PARAMS
  ),
  
  water_quality = list(
    mean_score = mean(zones_sf$water_quality_score, na.rm = TRUE),
    sd_score = sd(zones_sf$water_quality_score, na.rm = TRUE),
    nitrogen_removal_kg_yr = total_n_removal_kg,
    phosphorus_removal_kg_yr = total_p_removal_kg,
    parameters = WATER_QUALITY_PARAMS
  ),
  
  multi_criteria_weights = list(
    carbon = WEIGHT_CARBON,
    biodiversity = WEIGHT_BIODIVERSITY,
    flood_protection = WEIGHT_FLOOD_PROTECTION,
    water_quality = WEIGHT_WATER_QUALITY
  ),
  
  output_files = list(
    biodiversity_raster = "biodiversity_score.tif",
    flood_protection_raster = "flood_protection_score.tif",
    water_quality_raster = "water_quality_score.tif",
    zone_summary = "cobenefit_summary_by_zone.csv",
    zones_spatial = "zones_with_cobenefits.gpkg"
  )
)

write_yaml(cobenefit_report, file.path(cobenefit_dir, "cobenefit_analysis_report.yaml"))
log_message("Saved: cobenefit_analysis_report.yaml")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("CO-BENEFITS ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("Output files saved to:\n")
cat(sprintf("  %s/\n\n", cobenefit_dir))

cat("Spatial Outputs:\n")
cat("  🗺️  biodiversity_score.tif - Habitat quality index\n")
cat("  🗺️  flood_protection_score.tif - Wave attenuation capacity\n")
cat("  🗺️  water_quality_score.tif - Nutrient removal capacity\n")
cat("  🗺️  zones_with_cobenefits.gpkg - Vector data with all scores\n\n")

cat("Tabular Outputs:\n")
cat("  📊 cobenefit_summary_by_zone.csv - Zone-level scores\n")
cat("  📊 cobenefit_analysis_report.yaml - Full analysis summary\n\n")

cat("Plots:\n")
cat("  📈 biodiversity_distribution.png\n")
cat("  📈 flood_protection_distribution.png\n")
cat("  📈 water_quality_distribution.png\n")
cat("  📈 biodiversity_vs_flood.png\n\n")

cat("Integration with P5_04:\n")
cat("  These outputs are automatically detected by P5_04_scenario_optimization.R\n")
cat("  Co-benefit scores will be used instead of placeholder values\n\n")

cat("To use these results:\n")
cat("  1. Review cobenefit_summary_by_zone.csv for zone scores\n")
cat("  2. Load spatial layers in QGIS/ArcGIS for visualization\n")
cat("  3. Run P5_04_scenario_optimization.R to use in multi-criteria analysis\n\n")

cat("Data Improvements:\n")
cat("  For more accurate co-benefit scoring, provide:\n")
cat("    - Reference sites shapefile: data_raw/reference_sites.shp\n")
cat("    - Elevation raster: data_processed/elevation.tif\n")
cat("    - Species occurrence data for habitat suitability modeling\n")
cat("    - Water quality monitoring data for validation\n\n")

log_message("=== MODULE P5_04b COMPLETE ===")