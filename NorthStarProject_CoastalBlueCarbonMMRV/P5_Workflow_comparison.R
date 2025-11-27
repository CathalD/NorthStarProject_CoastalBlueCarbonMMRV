# ============================================================================
# MODULE 0X: COMPREHENSIVE WORKFLOW COMPARISON (FIXED)
# ============================================================================
# PURPOSE: Compare carbon stock estimates across all workflow components
# FIXED: 
#   1. Handles different raster extents by resampling to common grid
#   2. Corrected Bayesian data path (uses raster files, not RDS)
#   3. Corrected Transfer Learning data path (uses outputs/carbon_stocks/predictions/)
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

# Load configuration
if (file.exists("blue_carbon_config.R")) {
  source("blue_carbon_config.R")
} else {
  stop("Configuration file not found.")
}

# Create log file
log_file <- file.path("logs", paste0("workflow_comparison_", Sys.Date(), ".log"))

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== COMPREHENSIVE WORKFLOW COMPARISON ===")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(terra)
  library(sf)
  library(ggplot2)
  library(gridExtra)
})

# Create output directory
dir.create("outputs/comparisons", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# HELPER FUNCTION: MATCH RASTER EXTENTS
# ============================================================================

#' Resample all rasters to match a reference raster
#' @param raster_list List of SpatRaster objects
#' @param reference_raster SpatRaster to use as template (or NULL to auto-select)
#' @return List of resampled rasters with matching extent/resolution
match_raster_extents <- function(raster_list, reference_raster = NULL) {
  
  if (length(raster_list) == 0) return(raster_list)
  
  # If no reference provided, use the one with smallest extent (most conservative)
  if (is.null(reference_raster)) {
    extents <- lapply(raster_list, function(r) as.vector(ext(r)))
    areas <- sapply(extents, function(e) (e[2] - e[1]) * (e[4] - e[3]))
    reference_raster <- raster_list[[which.min(areas)]]
    log_message(sprintf("  Using '%s' as reference for extent matching (smallest extent)",
                        names(raster_list)[which.min(areas)]))
  }
  
  # Resample all rasters to match reference
  matched_list <- list()
  
  for (i in seq_along(raster_list)) {
    rast_name <- names(raster_list)[i]
    r <- raster_list[[i]]
    
    # Check if already matches
    if (all(ext(r) == ext(reference_raster)) && 
        all(res(r) == res(reference_raster)) &&
        same.crs(r, reference_raster)) {
      matched_list[[rast_name]] <- r
      log_message(sprintf("  '%s': Already matches reference", rast_name))
    } else {
      # Resample to match reference
      log_message(sprintf("  '%s': Resampling to match reference...", rast_name))
      
      # Reproject if needed
      if (!same.crs(r, reference_raster)) {
        r <- project(r, reference_raster, method = "bilinear")
      }
      
      # Resample to match resolution and extent
      r_matched <- resample(r, reference_raster, method = "bilinear")
      matched_list[[rast_name]] <- r_matched
      
      log_message(sprintf("  '%s': Resampled successfully", rast_name))
    }
  }
  
  return(matched_list)
}

# ============================================================================
# DETECT AVAILABLE WORKFLOW COMPONENTS
# ============================================================================

log_message("Detecting available workflow components...")

workflow_available <- list(
  raw_data = file.exists("data_processed/cores_clean_bluecarbon.rds"),
  harmonized = file.exists("data_processed/cores_harmonized_bluecarbon.rds"),
  kriging = length(list.files("outputs/predictions/kriging", 
                              pattern = "^carbon_stock_.*\\.tif$")) > 0,
  rf = length(list.files("outputs/predictions/rf", 
                         pattern = "^carbon_stock_rf_.*\\.tif$")) > 0,
  # FIXED: Bayesian now checks for raster files, not RDS
  bayesian = length(list.files("outputs/predictions/posterior",
                               pattern = "^carbon_stock_posterior_mean_.*\\.tif$")) > 0,
  # FIXED: Transfer learning checks correct directory
  transfer_learning = length(list.files("outputs/carbon_stocks/predictions", 
                                        pattern = "^depth_.*_predictions\\.tif$")) > 0
)

log_message("Component availability:")
for (component in names(workflow_available)) {
  status <- ifelse(workflow_available[[component]], "✓ Available", "✗ Not available")
  log_message(sprintf("  %s: %s", component, status))
}

# Check if we have enough to compare
n_available <- sum(unlist(workflow_available))
if (n_available < 2) {
  stop("Need at least 2 workflow components to compare. Run more modules first.")
}

log_message(sprintf("Found %d workflow components for comparison", n_available))

# ============================================================================
# LOAD RAW FIELD DATA (MODULE 01)
# ============================================================================

raw_summary <- NULL

if (workflow_available$raw_data) {
  log_message("Loading raw field data...")
  
  cores_raw <- readRDS("data_processed/cores_clean_bluecarbon.rds")
  
  # Calculate total carbon stock per core (sum across depth)
  raw_summary <- cores_raw %>%
    group_by(core_id, stratum) %>%
    summarise(
      total_stock_kg_m2 = sum(carbon_stock_kg_m2, na.rm = TRUE),
      max_depth_cm = max(depth_bottom_cm, na.rm = TRUE),
      n_samples = n(),
      .groups = "drop"
    ) %>%
    summarise(
      component = "Raw Field Data",
      mean_stock_kg_m2 = mean(total_stock_kg_m2, na.rm = TRUE),
      sd_stock_kg_m2 = sd(total_stock_kg_m2, na.rm = TRUE),
      min_stock_kg_m2 = min(total_stock_kg_m2, na.rm = TRUE),
      max_stock_kg_m2 = max(total_stock_kg_m2, na.rm = TRUE),
      mean_stock_Mg_ha = mean(total_stock_kg_m2, na.rm = TRUE) * 10,
      sd_stock_Mg_ha = sd(total_stock_kg_m2, na.rm = TRUE) * 10,
      n_cores = n_distinct(core_id),
      n_samples = sum(n_samples),
      .groups = "drop"
    )
  
  log_message(sprintf("  Raw data: %.1f ± %.1f Mg C/ha (n=%d cores)", 
                      raw_summary$mean_stock_Mg_ha,
                      raw_summary$sd_stock_Mg_ha,
                      raw_summary$n_cores))
}

# ============================================================================
# LOAD HARMONIZED DATA (MODULE 03)
# ============================================================================

harmonized_summary <- NULL

if (workflow_available$harmonized) {
  log_message("Loading harmonized data...")
  
  cores_harmonized <- readRDS("data_processed/cores_harmonized_bluecarbon.rds")
  
  # Calculate total stock per core (sum across VM0033 depths)
  harmonized_summary <- cores_harmonized %>%
    group_by(core_id, stratum) %>%
    summarise(
      total_stock_kg_m2 = sum(carbon_stock_kg_m2, na.rm = TRUE),
      n_depths = n(),
      .groups = "drop"
    ) %>%
    summarise(
      component = "Depth Harmonized",
      mean_stock_kg_m2 = mean(total_stock_kg_m2, na.rm = TRUE),
      sd_stock_kg_m2 = sd(total_stock_kg_m2, na.rm = TRUE),
      min_stock_kg_m2 = min(total_stock_kg_m2, na.rm = TRUE),
      max_stock_kg_m2 = max(total_stock_kg_m2, na.rm = TRUE),
      mean_stock_Mg_ha = mean(total_stock_kg_m2, na.rm = TRUE) * 10,
      sd_stock_Mg_ha = sd(total_stock_kg_m2, na.rm = TRUE) * 10,
      n_cores = n_distinct(core_id),
      n_samples = sum(n_depths),
      .groups = "drop"
    )
  
  log_message(sprintf("  Harmonized: %.1f ± %.1f Mg C/ha (n=%d cores)", 
                      harmonized_summary$mean_stock_Mg_ha,
                      harmonized_summary$sd_stock_Mg_ha,
                      harmonized_summary$n_cores))
}

# ============================================================================
# LOAD KRIGING PREDICTIONS (MODULE 04)
# ============================================================================

kriging_summary <- NULL
kriging_raster <- NULL

if (workflow_available$kriging) {
  log_message("Loading kriging predictions...")
  
  # Find all kriging stock rasters
  kriging_files <- list.files("outputs/predictions/kriging",
                              pattern = "^carbon_stock_.*\\.tif$",
                              full.names = TRUE)
  
  if (length(kriging_files) > 0) {
    # Load and sum all depth layers
    kriging_layers <- lapply(kriging_files, rast)
    
    # Group by depth first
    depths <- sapply(kriging_files, function(f) {
      as.numeric(gsub(".*_(\\d+)cm\\.tif$", "\\1", basename(f)))
    })
    
    unique_depths <- unique(depths)
    depth_rasters <- list()
    
    for (d in unique_depths) {
      depth_files <- kriging_files[depths == d]
      if (length(depth_files) == 1) {
        depth_rasters[[as.character(d)]] <- rast(depth_files)
      } else {
        # Mosaic multiple strata at same depth
        rasts <- lapply(depth_files, rast)
        depth_rasters[[as.character(d)]] <- do.call(mosaic, c(rasts, list(fun = "mean")))
      }
    }
    
    # Match extents before summing
    if (length(depth_rasters) > 1) {
      depth_rasters <- match_raster_extents(depth_rasters)
      kriging_raster <- Reduce(`+`, depth_rasters)
    } else {
      kriging_raster <- depth_rasters[[1]]
    }
    
    # Convert from kg/m² to Mg/ha
    kriging_raster <- kriging_raster * 10
    
    # Calculate statistics
    kriging_vals <- values(kriging_raster, mat = FALSE)
    kriging_vals <- kriging_vals[!is.na(kriging_vals)]
    
    kriging_summary <- data.frame(
      component = "Kriging",
      mean_stock_kg_m2 = mean(kriging_vals, na.rm = TRUE) / 10,
      sd_stock_kg_m2 = sd(kriging_vals, na.rm = TRUE) / 10,
      min_stock_kg_m2 = min(kriging_vals, na.rm = TRUE) / 10,
      max_stock_kg_m2 = max(kriging_vals, na.rm = TRUE) / 10,
      mean_stock_Mg_ha = mean(kriging_vals, na.rm = TRUE),
      sd_stock_Mg_ha = sd(kriging_vals, na.rm = TRUE),
      n_cores = NA,
      n_samples = length(kriging_vals)
    )
    
    log_message(sprintf("  Kriging: %.1f ± %.1f Mg C/ha (%d cells)", 
                        kriging_summary$mean_stock_Mg_ha,
                        kriging_summary$sd_stock_Mg_ha,
                        kriging_summary$n_samples))
  }
}

# ============================================================================
# LOAD RANDOM FOREST PREDICTIONS (MODULE 05)
# ============================================================================

rf_summary <- NULL
rf_raster <- NULL

if (workflow_available$rf) {
  log_message("Loading Random Forest predictions...")
  
  # Find all RF stock rasters
  rf_files <- list.files("outputs/predictions/rf",
                         pattern = "^carbon_stock_rf_.*\\.tif$",
                         full.names = TRUE)
  
  if (length(rf_files) > 0) {
    # Load all depth layers
    rf_layers <- lapply(rf_files, rast)
    names(rf_layers) <- basename(rf_files)
    
    # Match extents before summing
    if (length(rf_layers) > 1) {
      rf_layers <- match_raster_extents(rf_layers)
      rf_raster <- Reduce(`+`, rf_layers)
    } else {
      rf_raster <- rf_layers[[1]]
    }
    
    # Convert from kg/m² to Mg/ha
    rf_raster <- rf_raster * 10
    
    # Calculate statistics
    rf_vals <- values(rf_raster, mat = FALSE)
    rf_vals <- rf_vals[!is.na(rf_vals)]
    
    rf_summary <- data.frame(
      component = "Random Forest",
      mean_stock_kg_m2 = mean(rf_vals, na.rm = TRUE) / 10,
      sd_stock_kg_m2 = sd(rf_vals, na.rm = TRUE) / 10,
      min_stock_kg_m2 = min(rf_vals, na.rm = TRUE) / 10,
      max_stock_kg_m2 = max(rf_vals, na.rm = TRUE) / 10,
      mean_stock_Mg_ha = mean(rf_vals, na.rm = TRUE),
      sd_stock_Mg_ha = sd(rf_vals, na.rm = TRUE),
      n_cores = NA,
      n_samples = length(rf_vals)
    )
    
    log_message(sprintf("  Random Forest: %.1f ± %.1f Mg C/ha (%d cells)", 
                        rf_summary$mean_stock_Mg_ha,
                        rf_summary$sd_stock_Mg_ha,
                        rf_summary$n_samples))
  }
}

# ============================================================================
# LOAD BAYESIAN POSTERIOR (MODULE 06c) - FIXED
# ============================================================================

bayesian_summary <- NULL
bayesian_raster <- NULL

if (workflow_available$bayesian) {
  log_message("Loading Bayesian posterior estimates...")
  
  # FIXED: Bayesian module saves individual rasters, not a single RDS file
  # Look for posterior mean rasters in outputs/predictions/posterior/
  bayesian_files <- list.files("outputs/predictions/posterior",
                               pattern = "^carbon_stock_posterior_mean_.*\\.tif$",
                               full.names = TRUE)
  
  if (length(bayesian_files) > 0) {
    log_message(sprintf("  Found %d Bayesian posterior rasters", length(bayesian_files)))
    
    # Load all depth layers
    bayesian_layers <- lapply(bayesian_files, rast)
    names(bayesian_layers) <- basename(bayesian_files)
    
    # Match extents before summing
    if (length(bayesian_layers) > 1) {
      bayesian_layers <- match_raster_extents(bayesian_layers)
      bayesian_raster <- Reduce(`+`, bayesian_layers)
    } else {
      bayesian_raster <- bayesian_layers[[1]]
    }
    
    # Convert from kg/m² to Mg/ha
    bayesian_raster <- bayesian_raster * 10
    
    # Calculate statistics
    bayesian_vals <- values(bayesian_raster, mat = FALSE)
    bayesian_vals <- bayesian_vals[!is.na(bayesian_vals)]
    
    bayesian_summary <- data.frame(
      component = "Bayesian Posterior",
      mean_stock_kg_m2 = mean(bayesian_vals, na.rm = TRUE) / 10,
      sd_stock_kg_m2 = sd(bayesian_vals, na.rm = TRUE) / 10,
      min_stock_kg_m2 = min(bayesian_vals, na.rm = TRUE) / 10,
      max_stock_kg_m2 = max(bayesian_vals, na.rm = TRUE) / 10,
      mean_stock_Mg_ha = mean(bayesian_vals, na.rm = TRUE),
      sd_stock_Mg_ha = sd(bayesian_vals, na.rm = TRUE),
      n_cores = NA,
      n_samples = length(bayesian_vals)
    )
    
    log_message(sprintf("  Bayesian: %.1f ± %.1f Mg C/ha (%d cells)", 
                        bayesian_summary$mean_stock_Mg_ha,
                        bayesian_summary$sd_stock_Mg_ha,
                        bayesian_summary$n_samples))
  } else {
    log_message("  No Bayesian posterior rasters found", "WARNING")
  }
}

# ============================================================================
# LOAD TRANSFER LEARNING PREDICTIONS (MODULE 05 TL) - FIXED
# ============================================================================

tl_summary <- NULL
tl_raster <- NULL

if (workflow_available$transfer_learning) {
  log_message("Loading Transfer Learning predictions...")
  
  # FIXED: Transfer learning saves prediction rasters in outputs/carbon_stocks/predictions/
  # Each file is a 4-band raster: Global_Prior, Local_Only, Transfer_Final, Difference
  # We want band 3 (Transfer_Final) from each depth
  
  tl_files <- list.files("outputs/carbon_stocks/predictions",
                         pattern = "^depth_.*_predictions\\.tif$",
                         full.names = TRUE)
  
  if (length(tl_files) > 0) {
    log_message(sprintf("  Found %d Transfer Learning prediction files", length(tl_files)))
    
    # Extract Transfer_Final band (band 3) from each file
    tl_layers <- list()
    for (f in tl_files) {
      # Extract depth from filename (e.g., "depth_7.5_predictions.tif" -> 7.5)
      depth_str <- gsub(".*depth_([0-9.]+)_predictions\\.tif", "\\1", basename(f))
      
      # Load raster and extract band 3 (Transfer_Final)
      r <- rast(f)
      
      # Check if this is a multi-band raster
      if (nlyr(r) >= 3) {
        tl_layers[[depth_str]] <- r[[3]]  # Band 3 = Transfer_Final
        log_message(sprintf("    Loaded Transfer_Final for depth %s cm", depth_str))
      } else {
        log_message(sprintf("    WARNING: File %s has fewer than 3 bands, skipping", basename(f)), "WARNING")
      }
    }
    
    if (length(tl_layers) > 0) {
      # Match extents before summing
      if (length(tl_layers) > 1) {
        tl_layers <- match_raster_extents(tl_layers)
        tl_raster <- Reduce(`+`, tl_layers)
      } else {
        tl_raster <- tl_layers[[1]]
      }
      
      # Convert from kg/m² to Mg/ha
      tl_raster <- tl_raster * 10
      
      # Calculate statistics
      tl_vals <- values(tl_raster, mat = FALSE)
      tl_vals <- tl_vals[!is.na(tl_vals)]
      
      tl_summary <- data.frame(
        component = "Transfer Learning",
        mean_stock_kg_m2 = mean(tl_vals, na.rm = TRUE) / 10,
        sd_stock_kg_m2 = sd(tl_vals, na.rm = TRUE) / 10,
        min_stock_kg_m2 = min(tl_vals, na.rm = TRUE) / 10,
        max_stock_kg_m2 = max(tl_vals, na.rm = TRUE) / 10,
        mean_stock_Mg_ha = mean(tl_vals, na.rm = TRUE),
        sd_stock_Mg_ha = sd(tl_vals, na.rm = TRUE),
        n_cores = NA,
        n_samples = length(tl_vals)
      )
      
      log_message(sprintf("  Transfer Learning: %.1f ± %.1f Mg C/ha (%d cells)", 
                          tl_summary$mean_stock_Mg_ha,
                          tl_summary$sd_stock_Mg_ha,
                          tl_summary$n_samples))
    }
  } else {
    log_message("  No Transfer Learning prediction files found", "WARNING")
  }
}

# ============================================================================
# COMBINE ALL SUMMARIES
# ============================================================================

log_message("Creating comparison table...")

comparison_table <- bind_rows(
  raw_summary,
  harmonized_summary,
  kriging_summary,
  rf_summary,
  bayesian_summary,
  tl_summary
)

# Add relative difference from raw data (if available)
if (!is.null(raw_summary)) {
  comparison_table <- comparison_table %>%
    mutate(
      diff_from_raw_Mg_ha = mean_stock_Mg_ha - raw_summary$mean_stock_Mg_ha,
      pct_diff_from_raw = 100 * (mean_stock_Mg_ha - raw_summary$mean_stock_Mg_ha) / 
        raw_summary$mean_stock_Mg_ha
    )
}

# Save comparison table
write.csv(comparison_table, "outputs/comparisons/workflow_comparison_table.csv",
          row.names = FALSE)

log_message("Saved workflow_comparison_table.csv")

# Print to console
cat("\n========================================\n")
cat("WORKFLOW COMPARISON TABLE\n")
cat("========================================\n\n")
print(comparison_table, row.names = FALSE)

# ============================================================================
# CREATE COMPARISON VISUALIZATIONS
# ============================================================================

log_message("Creating comparison visualizations...")

# 1. Bar plot of mean stocks with error bars
p_comparison <- ggplot(comparison_table, 
                       aes(x = reorder(component, mean_stock_Mg_ha), 
                           y = mean_stock_Mg_ha)) +
  geom_col(aes(fill = component), alpha = 0.8, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_stock_Mg_ha - sd_stock_Mg_ha,
                    ymax = mean_stock_Mg_ha + sd_stock_Mg_ha),
                width = 0.3, linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f", mean_stock_Mg_ha)),
            vjust = -2, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Raw Field Data" = "#8D6E63",
    "Depth Harmonized" = "#5C6BC0",
    "Kriging" = "#42A5F5",
    "Random Forest" = "#66BB6A",
    "Bayesian Posterior" = "#AB47BC",
    "Transfer Learning" = "#FFA726"
  )) +
  labs(
    title = "Carbon Stock Comparison Across Workflow Components",
    subtitle = "Total carbon stock (0-100 cm depth)",
    x = "Workflow Component",
    y = "Carbon Stock (Mg C/ha)",
    caption = "Error bars show ± 1 SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/comparisons/workflow_comparison_barplot.png",
       p_comparison, width = 10, height = 6, dpi = 300)

log_message("Saved workflow_comparison_barplot.png")

# 2. Create map comparison (if spatial predictions available)
if (!is.null(kriging_raster) || !is.null(rf_raster) || 
    !is.null(bayesian_raster) || !is.null(tl_raster)) {
  
  log_message("Creating spatial comparison maps...")
  
  # Collect available rasters
  raster_list <- list()
  if (!is.null(kriging_raster)) raster_list$Kriging <- kriging_raster
  if (!is.null(rf_raster)) raster_list$`Random Forest` <- rf_raster
  if (!is.null(bayesian_raster)) raster_list$`Bayesian Posterior` <- bayesian_raster
  if (!is.null(tl_raster)) raster_list$`Transfer Learning` <- tl_raster
  
  # Match extents for all rasters
  log_message("Matching raster extents for spatial comparison...")
  raster_list <- match_raster_extents(raster_list)
  
  # Create multi-panel map
  n_maps <- length(raster_list)
  ncols <- min(2, n_maps)
  nrows <- ceiling(n_maps / ncols)
  
  png("outputs/comparisons/workflow_comparison_maps.png",
      width = 12 * ncols, height = 10 * nrows, units = "in", res = 300)
  
  par(mfrow = c(nrows, ncols), mar = c(4, 4, 3, 6))
  
  # Determine common color scale
  all_vals <- unlist(lapply(raster_list, function(r) values(r, mat = FALSE)))
  all_vals <- all_vals[!is.na(all_vals)]
  zlim <- c(min(all_vals), max(all_vals))
  
  # Plot each raster
  for (method_name in names(raster_list)) {
    r <- raster_list[[method_name]]
    
    plot(r,
         main = sprintf("%s\nMean: %.1f Mg C/ha", 
                        method_name,
                        mean(values(r, mat = FALSE), na.rm = TRUE)),
         col = hcl.colors(100, "YlOrRd", rev = TRUE),
         range = zlim,
         axes = TRUE,
         mar = c(4, 4, 3, 6),
         cex.main = 1.3,
         axis.args = list(cex.axis = 0.9))
  }
  
  dev.off()
  
  log_message("Saved workflow_comparison_maps.png")
  
  # 3. Create difference maps (if we have multiple spatial predictions)
  if (length(raster_list) >= 2) {
    
    log_message("Creating difference maps...")
    
    # Use first raster as reference
    reference_name <- names(raster_list)[1]
    reference_raster <- raster_list[[1]]
    
    # Calculate differences (rasters already have matching extents)
    diff_rasters <- list()
    for (i in 2:length(raster_list)) {
      method_name <- names(raster_list)[i]
      
      tryCatch({
        diff_rasters[[method_name]] <- raster_list[[i]] - reference_raster
      }, error = function(e) {
        log_message(sprintf("Could not calculate difference for %s: %s", 
                            method_name, e$message), "WARNING")
      })
    }
    
    if (length(diff_rasters) > 0) {
      
      n_diff_maps <- length(diff_rasters)
      ncols_diff <- min(2, n_diff_maps)
      nrows_diff <- ceiling(n_diff_maps / ncols_diff)
      
      png("outputs/comparisons/workflow_difference_maps.png",
          width = 12 * ncols_diff, height = 10 * nrows_diff, units = "in", res = 300)
      
      par(mfrow = c(nrows_diff, ncols_diff), mar = c(4, 4, 3, 6))
      
      # Determine common difference scale
      all_diffs <- unlist(lapply(diff_rasters, function(r) values(r, mat = FALSE)))
      all_diffs <- all_diffs[!is.na(all_diffs)]
      diff_lim <- max(abs(all_diffs))
      
      # Plot differences
      for (method_name in names(diff_rasters)) {
        d <- diff_rasters[[method_name]]
        
        plot(d,
             main = sprintf("%s - %s\nMean Diff: %.1f Mg C/ha", 
                            method_name,
                            reference_name,
                            mean(values(d, mat = FALSE), na.rm = TRUE)),
             col = hcl.colors(100, "RdBu", rev = TRUE),
             range = c(-diff_lim, diff_lim),
             axes = TRUE,
             mar = c(4, 4, 3, 6),
             cex.main = 1.3,
             axis.args = list(cex.axis = 0.9))
      }
      
      dev.off()
      
      log_message("Saved workflow_difference_maps.png")
    }
  }
}

# ============================================================================
# CREATE HTML COMPARISON REPORT
# ============================================================================

log_message("Creating HTML comparison report...")

# Build table HTML
table_html <- "<table style='width: 100%; border-collapse: collapse; margin: 20px 0;'>\n"
table_html <- paste0(table_html, "<thead>\n<tr style='background-color: #2E7D32; color: white;'>\n")
table_html <- paste0(table_html, "<th style='padding: 10px; text-align: left;'>Component</th>\n")
table_html <- paste0(table_html, "<th style='padding: 10px; text-align: right;'>Mean (Mg C/ha)</th>\n")
table_html <- paste0(table_html, "<th style='padding: 10px; text-align: right;'>SD (Mg C/ha)</th>\n")
table_html <- paste0(table_html, "<th style='padding: 10px; text-align: right;'>Range (Mg C/ha)</th>\n")
if ("diff_from_raw_Mg_ha" %in% names(comparison_table)) {
  table_html <- paste0(table_html, "<th style='padding: 10px; text-align: right;'>Diff from Raw</th>\n")
}
table_html <- paste0(table_html, "<th style='padding: 10px; text-align: right;'>N</th>\n")
table_html <- paste0(table_html, "</tr>\n</thead>\n<tbody>\n")

for (i in 1:nrow(comparison_table)) {
  table_html <- paste0(table_html, "<tr style='", 
                       ifelse(i %% 2 == 0, "background-color: #f2f2f2;", ""), "'>\n")
  table_html <- paste0(table_html, sprintf("<td style='padding: 8px;'>%s</td>\n", 
                                           comparison_table$component[i]))
  table_html <- paste0(table_html, sprintf("<td style='padding: 8px; text-align: right;'>%.1f</td>\n", 
                                           comparison_table$mean_stock_Mg_ha[i]))
  table_html <- paste0(table_html, sprintf("<td style='padding: 8px; text-align: right;'>%.1f</td>\n", 
                                           comparison_table$sd_stock_Mg_ha[i]))
  table_html <- paste0(table_html, sprintf("<td style='padding: 8px; text-align: right;'>%.1f - %.1f</td>\n", 
                                           comparison_table$min_stock_Mg_ha[i],
                                           comparison_table$max_stock_Mg_ha[i]))
  if ("diff_from_raw_Mg_ha" %in% names(comparison_table)) {
    diff_val <- comparison_table$diff_from_raw_Mg_ha[i]
    pct_val <- comparison_table$pct_diff_from_raw[i]
    if (!is.na(diff_val)) {
      color <- ifelse(abs(pct_val) < 10, "green", ifelse(abs(pct_val) < 20, "orange", "red"))
      table_html <- paste0(table_html, sprintf("<td style='padding: 8px; text-align: right; color: %s;'>%+.1f (%+.1f%%)</td>\n", 
                                               color, diff_val, pct_val))
    } else {
      table_html <- paste0(table_html, "<td style='padding: 8px; text-align: right;'>-</td>\n")
    }
  }
  n_val <- ifelse(!is.na(comparison_table$n_cores[i]),
                  sprintf("%d cores", comparison_table$n_cores[i]),
                  sprintf("%d cells", comparison_table$n_samples[i]))
  table_html <- paste0(table_html, sprintf("<td style='padding: 8px; text-align: right;'>%s</td>\n", n_val))
  table_html <- paste0(table_html, "</tr>\n")
}

table_html <- paste0(table_html, "</tbody>\n</table>\n")

# Image tags
img_html <- ""
if (file.exists("outputs/comparisons/workflow_comparison_barplot.png")) {
  img_html <- paste0(img_html, 
                     "<h2>Statistical Comparison</h2>\n",
                     "<img src='workflow_comparison_barplot.png' style='width: 100%; max-width: 1000px;'>\n")
}
if (file.exists("outputs/comparisons/workflow_comparison_maps.png")) {
  img_html <- paste0(img_html,
                     "<h2>Spatial Comparison</h2>\n",
                     "<img src='workflow_comparison_maps.png' style='width: 100%; max-width: 1200px;'>\n")
}
if (file.exists("outputs/comparisons/workflow_difference_maps.png")) {
  img_html <- paste0(img_html,
                     "<h2>Difference Maps</h2>\n",
                     "<p>Differences calculated relative to: <strong>",
                     ifelse(exists("reference_name"), reference_name, "First method"),
                     "</strong></p>\n",
                     "<img src='workflow_difference_maps.png' style='width: 100%; max-width: 1200px;'>\n")
}

html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
<title>Workflow Comparison Report</title>
<style>
body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
h1 { color: #2E7D32; border-bottom: 3px solid #2E7D32; padding-bottom: 10px; }
h2 { color: #1976D2; border-bottom: 2px solid #1976D2; padding-bottom: 5px; margin-top: 30px; }
table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
th { background-color: #2E7D32; color: white; padding: 10px; text-align: left; }
td { border: 1px solid #ddd; padding: 8px; }
tr:nth-child(even) { background-color: #f2f2f2; }
.highlight { background-color: #FFF9C4; padding: 15px; border-left: 4px solid #FBC02D; margin: 20px 0; }
img { display: block; margin: 20px auto; }
</style>
</head>
<body>

<h1>🔬 Comprehensive Workflow Comparison (FIXED)</h1>
<p><strong>Project:</strong> %s<br>
<strong>Generated:</strong> %s<br>
<strong>Components Compared:</strong> %d</p>

<div class="highlight">
<strong>Interpretation Guide:</strong><br>
- <strong>Raw Field Data:</strong> Direct measurements from cores (sum across sampled depths)<br>
- <strong>Depth Harmonized:</strong> Spline-interpolated to VM0033 standard depths<br>
- <strong>Kriging:</strong> Spatial interpolation (geostatistical)<br>
- <strong>Random Forest:</strong> Machine learning with environmental covariates<br>
- <strong>Bayesian Posterior:</strong> Combines prior + likelihood for uncertainty reduction<br>
- <strong>Transfer Learning:</strong> Global model applied to local site
</div>

<h2>Comparison Table</h2>
%s

<p><em>Color coding for differences: <span style="color: green;">Green</span> = <10%%, 
<span style="color: orange;">Orange</span> = 10-20%%, <span style="color: red;">Red</span> = >20%%</em></p>

%s

<h2>Key Findings</h2>
<ul>
<li>Mean carbon stock range: %.1f - %.1f Mg C/ha</li>
<li>Coefficient of variation across methods: %.1f%%</li>
<li>Maximum difference between methods: %.1f Mg C/ha (%.1f%%)</li>
</ul>

<h2>Recommendations</h2>
<p><strong>For Carbon Crediting:</strong></p>
<ul>
<li>Use the <strong>most conservative estimate</strong> (lowest mean) for crediting</li>
<li>Report uncertainty range across all methods</li>
<li>Prioritize methods with lower uncertainty (smaller SD)</li>
<li>Consider Bayesian posterior if available (best uncertainty quantification)</li>
</ul>

<h2>Technical Notes (FIXED)</h2>
<p><strong>Path corrections made:</strong></p>
<ul>
<li><strong>Bayesian data:</strong> Now loads from <code>outputs/predictions/posterior/carbon_stock_posterior_mean_*.tif</code> instead of non-existent RDS file</li>
<li><strong>Transfer Learning data:</strong> Now loads from <code>outputs/carbon_stocks/predictions/depth_*_predictions.tif</code> (band 3: Transfer_Final)</li>
</ul>

<hr>
<p><em>Generated by Blue Carbon MMRV Workflow | Comprehensive Comparison Module (FIXED)</em></p>

</body>
</html>
',
                        PROJECT_NAME,
                        Sys.Date(),
                        nrow(comparison_table),
                        table_html,
                        img_html,
                        min(comparison_table$mean_stock_Mg_ha),
                        max(comparison_table$mean_stock_Mg_ha),
                        100 * sd(comparison_table$mean_stock_Mg_ha) / mean(comparison_table$mean_stock_Mg_ha),
                        max(comparison_table$mean_stock_Mg_ha) - min(comparison_table$mean_stock_Mg_ha),
                        100 * (max(comparison_table$mean_stock_Mg_ha) - min(comparison_table$mean_stock_Mg_ha)) / 
                          mean(comparison_table$mean_stock_Mg_ha)
)

writeLines(html_content, "outputs/comparisons/workflow_comparison_report.html")

log_message("Saved workflow_comparison_report.html")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("WORKFLOW COMPARISON COMPLETE\n")
cat("========================================\n\n")

cat("Components Compared:\n")
for (i in 1:nrow(comparison_table)) {
  cat(sprintf("  %s: %.1f ± %.1f Mg C/ha\n",
              comparison_table$component[i],
              comparison_table$mean_stock_Mg_ha[i],
              comparison_table$sd_stock_Mg_ha[i]))
}

cat("\nOutputs:\n")
cat("  📊 Comparison Table: outputs/comparisons/workflow_comparison_table.csv\n")
cat("  📈 Bar Plot: outputs/comparisons/workflow_comparison_barplot.png\n")
if (file.exists("outputs/comparisons/workflow_comparison_maps.png")) {
  cat("  🗺️  Spatial Maps: outputs/comparisons/workflow_comparison_maps.png\n")
}
if (file.exists("outputs/comparisons/workflow_difference_maps.png")) {
  cat("  📏 Difference Maps: outputs/comparisons/workflow_difference_maps.png\n")
}
cat("  📄 HTML Report: outputs/comparisons/workflow_comparison_report.html\n")

cat("\nKey Statistics:\n")
cat(sprintf("  Carbon stock range: %.1f - %.1f Mg C/ha\n",
            min(comparison_table$mean_stock_Mg_ha),
            max(comparison_table$mean_stock_Mg_ha)))
cat(sprintf("  Mean across methods: %.1f Mg C/ha\n",
            mean(comparison_table$mean_stock_Mg_ha)))
cat(sprintf("  CV across methods: %.1f%%\n",
            100 * sd(comparison_table$mean_stock_Mg_ha) / mean(comparison_table$mean_stock_Mg_ha)))

cat("\nRecommendation:\n")
if (sd(comparison_table$mean_stock_Mg_ha) / mean(comparison_table$mean_stock_Mg_ha) < 0.15) {
  cat("  ✓ LOW DISAGREEMENT: All methods agree well (<15% CV)\n")
  cat("    → High confidence in estimates\n")
} else if (sd(comparison_table$mean_stock_Mg_ha) / mean(comparison_table$mean_stock_Mg_ha) < 0.25) {
  cat("  ⚠ MODERATE DISAGREEMENT: Methods show some variation (15-25% CV)\n")
  cat("    → Review spatial predictions and use conservative estimate\n")
} else {
  cat("  ⚠️ HIGH DISAGREEMENT: Methods differ substantially (>25% CV)\n")
  cat("    → Investigate causes: sample size, model assumptions, data quality\n")
  cat("    → Use most conservative estimate for crediting\n")
}

cat("\nPATH FIXES APPLIED:\n")
cat("  ✓ Bayesian: Now loads from outputs/predictions/posterior/ (raster files)\n")
cat("  ✓ Transfer Learning: Now loads from outputs/carbon_stocks/predictions/ (band 3)\n")

cat("\n")

log_message("=== WORKFLOW COMPARISON COMPLETE ===")
