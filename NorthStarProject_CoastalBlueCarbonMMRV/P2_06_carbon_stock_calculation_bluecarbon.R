# ============================================================================
# MODULE 06: BLUE CARBON STOCK AGGREGATION & VM0033 COMPLIANCE
# ============================================================================
# PURPOSE: Aggregate carbon stocks from Modules 04/05, calculate conservative
#          estimates, compare methods, and generate VM0033 compliance reports
# INPUTS:
#   - outputs/predictions/kriging/carbon_stock_*_*cm.tif (from Module 04, kg/m²)
#   - outputs/predictions/kriging/se_combined_*_*cm.tif (from Module 04)
#   - outputs/predictions/rf/carbon_stock_rf_*cm.tif (from Module 05, kg/m²)
#   - outputs/predictions/rf/se_combined_*cm.tif (from Module 05)
#   - data_processed/stratum_raster.tif (from Module 05, optional)
# OUTPUTS:
#   - outputs/carbon_stocks/carbon_stocks_by_stratum_kriging.csv
#   - outputs/carbon_stocks/carbon_stocks_by_stratum_rf.csv
#   - outputs/carbon_stocks/carbon_stocks_method_comparison.csv
#   - outputs/carbon_stocks/carbon_stocks_conservative_vm0033.csv
#   - outputs/carbon_stocks/maps/*.tif
#   - outputs/mmrv_reports/vm0033_verification_summary.html
# NOTE: Modules 04 & 05 now output carbon stocks directly in kg/m² (not SOC)
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

# Load configuration
if (file.exists("blue_carbon_config.R")) {
  source("blue_carbon_config.R")
} else {
  stop("Configuration file not found. Run 00b_setup_directories.R first.")
}

# Create log file
log_file <- file.path("logs", paste0("carbon_stocks_", Sys.Date(), ".log"))

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 06: CARBON STOCK CALCULATION & VM0033 COMPLIANCE ===")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(terra)
  library(sf)
  library(ggplot2)
})

# Create output directories
dir.create("outputs/carbon_stocks", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/carbon_stocks/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/mmrv_reports", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# CONFIGURATION
# ============================================================================

# VM0033 depth intervals are loaded from blue_carbon_config.R
# VM0033_DEPTH_INTERVALS data frame has: depth_top, depth_bottom, depth_midpoint, thickness_cm

# IPCC/VM0033 Confidence Level
CONFIDENCE_LEVEL <- 0.95  # 95% CI required by VM0033

# Conservative approach: use lower bound of CI for crediting
USE_CONSERVATIVE_ESTIMATES <- TRUE

# Methods to analyze ('kriging', 'rf', or c('kriging', 'rf') for both)
METHODS_TO_ANALYZE <- c("kriging", "rf")

log_message(sprintf("Configuration: Methods=%s, Conservative=%s",
                    paste(METHODS_TO_ANALYZE, collapse=", "), USE_CONSERVATIVE_ESTIMATES))
log_message(sprintf("VM0033 depth intervals: %d layers", nrow(VM0033_DEPTH_INTERVALS)))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Calculate conservative estimate (lower bound of CI)
#' @param mean_raster Mean carbon stock
#' @param se_raster Standard error of carbon stock
#' @param confidence Confidence level (default 0.95)
#' @return Conservative (lower bound) estimate
calculate_conservative_estimate <- function(mean_raster, se_raster, confidence = CONFIDENCE_LEVEL) {

  # Z-score for confidence level
  z_score <- qnorm((1 + confidence) / 2)

  # Lower bound of CI
  conservative <- mean_raster - (z_score * se_raster)

  # Ensure non-negative
  conservative <- max(conservative, 0)

  return(conservative)
}

#' Load carbon stock rasters for a given method
#' @param method 'kriging' or 'rf'
#' @return List with stock and se rasters by depth
#' @note Loads carbon stocks in kg/m² (from Module 03 harmonization via 04/05)
load_stock_rasters <- function(method) {

  stocks <- list()
  uncertainties <- list()

  if (method == "kriging") {
    stock_dir <- "outputs/predictions/kriging"
    # Updated patterns to match Module 04 output: carbon_stock_{stratum}_{depth}cm.tif
    stock_pattern <- "^carbon_stock_.*_[0-9]+cm\\.tif$"
    se_pattern <- "^se_combined_.*_[0-9]+cm\\.tif$"
  } else if (method == "rf") {
    stock_dir <- "outputs/predictions/rf"
    # Updated patterns to match Module 05 output: carbon_stock_rf_{depth}cm.tif
    stock_pattern <- "^carbon_stock_rf_[0-9]+cm\\.tif$"
    se_pattern <- "^se_combined_[0-9]+cm\\.tif$"
  } else {
    stop("method must be 'kriging' or 'rf'")
  }

  # Check directory exists
  if (!dir.exists(stock_dir)) {
    stop(sprintf("Stock directory not found: %s\nRun Module 04 (kriging) or Module 05 (RF) first.", stock_dir))
  }

  # Load stock rasters
  stock_files <- list.files(stock_dir, pattern = stock_pattern, full.names = TRUE)

  if (length(stock_files) == 0) {
    stop(sprintf("No stock files found in %s\nRun Module 04 or Module 05 first.", stock_dir))
  }

  log_message(sprintf("Loading %d stock rasters for %s method", length(stock_files), method))

  # Group files by depth (handle multiple strata per depth for kriging)
  # Extract depth from filenames:
  #   Kriging: carbon_stock_{stratum}_{depth}cm.tif
  #   RF: carbon_stock_rf_{depth}cm.tif
  # NOTE: Filenames use rounded depths (7.5→8, 22.5→22, 40→40, 75→75)
  # The actual VM0033 midpoints are 7.5, 22.5, 40, 75 cm
  depth_files <- data.frame(
    file = stock_files,
    depth = sapply(stock_files, function(f) {
      # Extract number before "cm" in filename (will be rounded)
      as.numeric(gsub(".*_(\\d+)cm\\.tif$", "\\1", basename(f)))
    })
  )

  for (d in unique(depth_files$depth)) {
    files_at_depth <- depth_files$file[depth_files$depth == d]

    if (length(files_at_depth) == 1) {
      # Single file for this depth
      stock_raster <- rast(files_at_depth)
      # Convert from kg/m² to Mg/ha for VM0033 reporting: 1 kg/m² = 10 Mg/ha
      stocks[[as.character(d)]] <- stock_raster * 10
      log_message(sprintf("  Loaded: stock at depth %.0f cm (converted kg/m² → Mg/ha)", d))
    } else {
      # Multiple files (e.g., different strata) - mosaic them
      log_message(sprintf("  Loading %d files for depth %.0f cm (mosaicking strata)",
                         length(files_at_depth), d))
      rasters <- lapply(files_at_depth, rast)
      stock_raster <- do.call(mosaic, c(rasters, list(fun = "mean")))
      # Convert from kg/m² to Mg/ha for VM0033 reporting: 1 kg/m² = 10 Mg/ha
      stocks[[as.character(d)]] <- stock_raster * 10
      log_message(sprintf("  Mosaicked: stock at depth %.0f cm (converted kg/m² → Mg/ha)", d))
    }
  }

  # Load uncertainty rasters
  se_files <- list.files(stock_dir, pattern = se_pattern, full.names = TRUE)

  if (length(se_files) > 0) {
    log_message(sprintf("Loading %d uncertainty rasters for %s method", length(se_files), method))

    # Group SE files by depth
    # Extract depth from filenames:
    #   Kriging: se_combined_{stratum}_{depth}cm.tif
    #   RF: se_combined_{depth}cm.tif
    # NOTE: Same as stocks - filenames use rounded depths (7.5→8, 22.5→22)
    se_depth_files <- data.frame(
      file = se_files,
      depth = sapply(se_files, function(f) {
        # Extract number before "cm" in filename (will be rounded)
        as.numeric(gsub(".*_(\\d+)cm\\.tif$", "\\1", basename(f)))
      })
    )

    for (d in unique(se_depth_files$depth)) {
      files_at_depth <- se_depth_files$file[se_depth_files$depth == d]

      if (length(files_at_depth) == 1) {
        # Single file for this depth
        se_raster <- rast(files_at_depth)
        # Convert SE from kg/m² to Mg/ha: multiply by 10
        uncertainties[[as.character(d)]] <- se_raster * 10
        log_message(sprintf("  Loaded: SE at depth %.0f cm (converted kg/m² → Mg/ha)", d))
      } else {
        # Multiple files - mosaic them
        log_message(sprintf("  Loading %d SE files for depth %.0f cm (mosaicking strata)",
                           length(files_at_depth), d))
        rasters <- lapply(files_at_depth, rast)
        se_raster <- do.call(mosaic, c(rasters, list(fun = "mean")))
        # Convert SE from kg/m² to Mg/ha: multiply by 10
        uncertainties[[as.character(d)]] <- se_raster * 10
        log_message(sprintf("  Mosaicked: SE at depth %.0f cm (converted kg/m² → Mg/ha)", d))
      }
    }
  } else {
    log_message(sprintf("No uncertainty files found for %s method", method), "WARNING")
  }

  return(list(stocks = stocks, uncertainties = uncertainties))
}

# ============================================================================
# LOAD STOCK RASTERS FROM MODULES 04 & 05
# ============================================================================

log_message("Loading carbon stock rasters from Modules 04 & 05...")

# Load stocks for each method
method_stocks <- list()

for (method in METHODS_TO_ANALYZE) {
  log_message(sprintf("\n=== Loading %s stocks ===", toupper(method)))

  method_stocks[[method]] <- tryCatch({
    load_stock_rasters(method)
  }, error = function(e) {
    log_message(sprintf("Failed to load %s stocks: %s", method, e$message), "ERROR")
    NULL
  })
}

# Check that at least one method loaded successfully
if (all(sapply(method_stocks, is.null))) {
  stop("No stock rasters could be loaded. Run Module 04 (kriging) or Module 05 (RF) first.")
}

# Filter out failed methods
method_stocks <- method_stocks[!sapply(method_stocks, is.null)]
METHODS_AVAILABLE <- names(method_stocks)

log_message(sprintf("\nSuccessfully loaded stocks for: %s", paste(METHODS_AVAILABLE, collapse=", ")))

# ============================================================================
# LOAD STRATUM INFORMATION
# ============================================================================

log_message("\nLoading stratum information...")

# Load stratum raster created by Module 05
stratum_raster_file <- "data_processed/stratum_raster.tif"

if (file.exists(stratum_raster_file)) {
  stratum_raster <- rast(stratum_raster_file)
  log_message(sprintf("Loaded stratum raster: %d strata", length(unique(values(stratum_raster, na.rm = TRUE)))))
  HAS_STRATUM_RASTER <- TRUE
} else {
  log_message("No stratum raster found - will calculate overall stocks only", "WARNING")
  log_message("Run Module 05 to create stratum raster from GEE masks", "INFO")
  HAS_STRATUM_RASTER <- FALSE
  stratum_raster <- NULL
}

# ============================================================================
# AGGREGATE STOCKS BY VM0033 DEPTH INTERVALS
# ============================================================================

log_message("\nAggregating carbon stocks by VM0033 depth intervals...")

# Store results for each method
method_aggregated_stocks <- list()

for (method in METHODS_AVAILABLE) {

  log_message(sprintf("\n=== Processing %s stocks ===", toupper(method)))

  stocks <- method_stocks[[method]]$stocks
  uncertainties <- method_stocks[[method]]$uncertainties
  has_uncertainties <- length(uncertainties) > 0

  # Get available depths
  available_depths <- as.numeric(names(stocks))
  log_message(sprintf("Available depths: %s cm", paste(available_depths, collapse=", ")))

  # Aggregate for each VM0033 interval
  interval_stocks <- list()

  for (i in 1:nrow(VM0033_DEPTH_INTERVALS)) {

    depth_mid <- VM0033_DEPTH_INTERVALS$depth_midpoint[i]
    depth_top <- VM0033_DEPTH_INTERVALS$depth_top[i]
    depth_bottom <- VM0033_DEPTH_INTERVALS$depth_bottom[i]

    interval_label <- sprintf("%.0f-%.0f cm", depth_top, depth_bottom)

    log_message(sprintf("\n  Interval: %s (midpoint %.1f cm)", interval_label, depth_mid))

    # Check if we have stock for this depth
    # NOTE: Files are named with rounded depths (7.5→8, 22.5→22)
    # but actual VM0033 midpoints are exact (7.5, 22.5)
    # Round the depth to match file naming convention
    depth_rounded <- round(depth_mid)
    depth_str <- as.character(depth_rounded)

    if (!depth_str %in% names(stocks)) {
      log_message(sprintf("    No stock raster for depth %.1f cm (looking for %d cm in files)",
                         depth_mid, depth_rounded), "WARNING")
      next
    }

    log_message(sprintf("    Found stock file for depth %.1f cm (file uses %d cm)",
                       depth_mid, depth_rounded))

    # Load stock raster (computed by Module 04/05, converted to Mg/ha)
    # Note: These are carbon stocks, not SOC concentrations
    # Carbon stocks calculated from harmonized SOC + BD in Module 03
    # Converted from kg/m² to Mg/ha (× 10) when loaded
    stock_raster <- stocks[[depth_str]]

    # Load uncertainty if available (also converted to Mg/ha)
    if (has_uncertainties && depth_str %in% names(uncertainties)) {
      se_raster <- uncertainties[[depth_str]]
      log_message(sprintf("    Carbon stock (Mg/ha) loaded with uncertainty"))
    } else {
      se_raster <- NULL
      log_message(sprintf("    Carbon stock (Mg/ha) loaded without uncertainty"))
    }

    # Store results
    interval_stocks[[interval_label]] <- list(
      mean = stock_raster,
      se = se_raster,
      depth_midpoint = depth_mid
    )

    # Save rasters
    mean_file <- file.path("outputs/carbon_stocks/maps",
                           sprintf("carbon_stock_%s_%s_mean.tif", method, gsub(" ", "_", interval_label)))
    writeRaster(stock_raster, mean_file, overwrite = TRUE)
    log_message(sprintf("    Saved: %s", basename(mean_file)))

    if (!is.null(se_raster)) {
      se_file <- file.path("outputs/carbon_stocks/maps",
                           sprintf("carbon_stock_%s_%s_se.tif", method, gsub(" ", "_", interval_label)))
      writeRaster(se_raster, se_file, overwrite = TRUE)

      # Calculate conservative estimate
      conservative <- calculate_conservative_estimate(stock_raster, se_raster)
      cons_file <- file.path("outputs/carbon_stocks/maps",
                             sprintf("carbon_stock_%s_%s_conservative.tif", method, gsub(" ", "_", interval_label)))
      writeRaster(conservative, cons_file, overwrite = TRUE)
      log_message(sprintf("    Saved conservative estimate"))
    }
  }

  # Store for this method
  method_aggregated_stocks[[method]] <- interval_stocks
}

# ============================================================================
# CALCULATE TOTAL CARBON STOCKS (0-100 cm)
# ============================================================================

log_message("\nCalculating total carbon stocks (0-100 cm)...")

# Store total stocks for each method
method_total_stocks <- list()

for (method in METHODS_AVAILABLE) {

  log_message(sprintf("\n  Processing %s total stocks...", toupper(method)))

  interval_stocks <- method_aggregated_stocks[[method]]

  if (length(interval_stocks) < nrow(VM0033_DEPTH_INTERVALS)) {
    log_message(sprintf("    Missing some VM0033 intervals for %s (have %d, need %d)",
                       method, length(interval_stocks), nrow(VM0033_DEPTH_INTERVALS)), "WARNING")
  }

  # Sum all intervals to get total 0-100 cm stock
  total_stock <- NULL
  total_variance <- NULL
  has_all_uncertainties <- TRUE

  for (interval_label in names(interval_stocks)) {

    stock_data <- interval_stocks[[interval_label]]

    # Add stock
    if (is.null(total_stock)) {
      total_stock <- stock_data$mean
    } else {
      total_stock <- total_stock + stock_data$mean
    }

    # Add variance (assumes independence - conservative)
    if (!is.null(stock_data$se)) {
      if (is.null(total_variance)) {
        total_variance <- stock_data$se^2
      } else {
        total_variance <- total_variance + stock_data$se^2
      }
    } else {
      has_all_uncertainties <- FALSE
    }
  }

  # Convert total variance to SE
  if (!is.null(total_variance) && has_all_uncertainties) {
    total_se <- sqrt(total_variance)

    # Conservative estimate
    total_conservative <- calculate_conservative_estimate(total_stock, total_se)
  } else {
    total_se <- NULL
    total_conservative <- NULL

    if (!has_all_uncertainties) {
      log_message(sprintf("    Cannot calculate total uncertainty for %s - some intervals missing SE", method), "WARNING")
    }
  }

  # Save rasters
  writeRaster(total_stock,
              sprintf("outputs/carbon_stocks/maps/carbon_stock_%s_total_mean.tif", method),
              overwrite = TRUE)

  if (!is.null(total_se)) {
    writeRaster(total_se,
                sprintf("outputs/carbon_stocks/maps/carbon_stock_%s_total_se.tif", method),
                overwrite = TRUE)
    writeRaster(total_conservative,
                sprintf("outputs/carbon_stocks/maps/carbon_stock_%s_total_conservative.tif", method),
                overwrite = TRUE)
  }

  log_message(sprintf("    Saved %s total stock maps", method))

  # Store for later use
  method_total_stocks[[method]] <- list(
    mean = total_stock,
    se = total_se,
    conservative = total_conservative
  )
}

# ============================================================================
# CALCULATE STRATUM-LEVEL STATISTICS
# ============================================================================

if (HAS_STRATUM_RASTER) {

  log_message("\nCalculating stratum-level carbon stocks...")

  # Get stratum levels and labels
  stratum_levels <- levels(stratum_raster)[[1]]

  if (is.null(stratum_levels) || nrow(stratum_levels) == 0) {
    log_message("Stratum raster has no factor levels - using unique values", "WARNING")
    unique_vals <- unique(values(stratum_raster, na.rm = TRUE))
    stratum_levels <- data.frame(ID = unique_vals, stratum = as.character(unique_vals))
  }

  log_message(sprintf("Found %d strata: %s", nrow(stratum_levels),
                     paste(stratum_levels$stratum, collapse=", ")))

  # Process each method
  for (method in METHODS_AVAILABLE) {

    log_message(sprintf("\n=== Processing %s by stratum ===", toupper(method)))

    stratum_stats <- data.frame()
    interval_stocks <- method_aggregated_stocks[[method]]
    total_stock_raster <- method_total_stocks[[method]]$mean

    # Process each stratum
    for (i in 1:nrow(stratum_levels)) {

      stratum_id <- stratum_levels$ID[i]
      stratum_name <- stratum_levels$stratum[i]

      log_message(sprintf("\n  Processing stratum: %s (ID=%s)", stratum_name, stratum_id))

      # Create mask for this stratum
      stratum_mask <- stratum_raster == stratum_id

      # Calculate area
      area_vals <- values(stratum_mask, mat = FALSE)
      n_pixels <- sum(area_vals == 1, na.rm = TRUE)

      if (n_pixels == 0) {
        log_message(sprintf("    No pixels found for stratum %s", stratum_name), "WARNING")
        next
      }

      pixel_area_ha <- prod(res(stratum_mask)) / 10000  # m² to ha
      area_ha <- n_pixels * pixel_area_ha

      log_message(sprintf("    Area: %.2f ha (%d pixels)", area_ha, n_pixels))

      # Extract values for each depth interval
      for (interval_label in names(interval_stocks)) {

        stock_data <- interval_stocks[[interval_label]]
        stock_raster <- stock_data$mean
        se_raster <- stock_data$se

        # Mask to stratum
        stock_masked <- mask(stock_raster, stratum_mask, maskvalues = c(0, NA))

        # Calculate statistics
        stock_vals <- values(stock_masked, mat = FALSE)
        stock_vals <- stock_vals[!is.na(stock_vals)]

        if (length(stock_vals) > 0) {

          mean_stock <- mean(stock_vals, na.rm = TRUE)
          sd_stock <- sd(stock_vals, na.rm = TRUE)
          median_stock <- median(stock_vals, na.rm = TRUE)

          # Total stock in stratum for this interval
          total_stock_Mg <- sum(stock_vals, na.rm = TRUE) * pixel_area_ha

          # If uncertainty available
          if (!is.null(se_raster)) {
            se_masked <- mask(se_raster, stratum_mask, maskvalues = c(0, NA))
            se_vals <- values(se_masked, mat = FALSE)
            se_vals <- se_vals[!is.na(se_vals)]

            # Average SE
            mean_se <- mean(se_vals, na.rm = TRUE)

            # Conservative estimate
            conservative_stock <- mean_stock - (qnorm((1 + CONFIDENCE_LEVEL) / 2) * mean_se)
            conservative_stock <- max(conservative_stock, 0)

            conservative_total_Mg <- conservative_stock * area_ha
          } else {
            mean_se <- NA
            conservative_stock <- NA
            conservative_total_Mg <- NA
          }

          # Store results
          stratum_stats <- rbind(stratum_stats, data.frame(
            method = method,
            stratum = stratum_name,
            depth_interval = interval_label,
            area_ha = area_ha,
            mean_stock_Mg_ha = mean_stock,
            sd_stock_Mg_ha = sd_stock,
            median_stock_Mg_ha = median_stock,
            se_stock_Mg_ha = mean_se,
            conservative_stock_Mg_ha = conservative_stock,
            total_stock_Mg = total_stock_Mg,
            conservative_total_Mg = conservative_total_Mg,
            n_pixels = n_pixels
          ))

          log_message(sprintf("    %s: %.1f ± %.1f Mg C/ha",
                             interval_label, mean_stock, mean_se))
        }
      }
    }

    # Save stratum statistics for this method (long format)
    write.csv(stratum_stats,
              sprintf("outputs/carbon_stocks/carbon_stocks_by_stratum_%s.csv", method),
              row.names = FALSE)
    log_message(sprintf("\nSaved %s stratum-level statistics (long format)", method))

    # Create wide-format version for temporal analysis
    # Each stratum is one row with columns for each VM0033 interval
    stratum_stats_wide <- stratum_stats %>%
      select(stratum, depth_interval, mean_stock_Mg_ha, se_stock_Mg_ha,
             conservative_stock_Mg_ha, area_ha) %>%
      pivot_wider(
        id_cols = c(stratum, area_ha),
        names_from = depth_interval,
        values_from = c(mean_stock_Mg_ha, se_stock_Mg_ha, conservative_stock_Mg_ha),
        names_sep = "_"
      ) %>%
      # Rename columns to match temporal analysis expectations
      rename_with(
        ~ gsub("mean_stock_Mg_ha_", "carbon_stock_", .x),
        starts_with("mean_stock_Mg_ha_")
      ) %>%
      rename_with(
        ~ gsub("se_stock_Mg_ha_", "carbon_stock_se_", .x),
        starts_with("se_stock_Mg_ha_")
      ) %>%
      rename_with(
        ~ gsub("conservative_stock_Mg_ha_", "carbon_stock_conservative_", .x),
        starts_with("conservative_stock_Mg_ha_")
      ) %>%
      # Add method column
      mutate(method = method, .before = 1)

    # Save wide format for temporal analysis
    write.csv(stratum_stats_wide,
              sprintf("outputs/carbon_stocks/carbon_stocks_by_stratum_%s_wide.csv", method),
              row.names = FALSE)
    log_message(sprintf("Saved %s wide-format for temporal analysis", method))
  }

} else {
  log_message("\nNo stratum raster available - skipping stratum-level analysis", "WARNING")
}

# ============================================================================
# CALCULATE OVERALL STATISTICS
# ============================================================================

log_message("\nCalculating overall carbon stock statistics...")

for (method in METHODS_AVAILABLE) {

  log_message(sprintf("\n  Processing %s overall statistics...", toupper(method)))

  overall_stats <- data.frame()
  interval_stocks <- method_aggregated_stocks[[method]]

  for (interval_label in names(interval_stocks)) {

    stock_data <- interval_stocks[[interval_label]]
    stock_raster <- stock_data$mean
    se_raster <- stock_data$se

    # Extract all values
    stock_vals <- values(stock_raster, mat = FALSE)
    stock_vals <- stock_vals[!is.na(stock_vals)]

    if (length(stock_vals) > 0) {

      mean_stock <- mean(stock_vals, na.rm = TRUE)
      sd_stock <- sd(stock_vals, na.rm = TRUE)
      median_stock <- median(stock_vals, na.rm = TRUE)
      min_stock <- min(stock_vals, na.rm = TRUE)
      max_stock <- max(stock_vals, na.rm = TRUE)

      # Calculate area
      n_pixels <- length(stock_vals)
      pixel_area_ha <- prod(res(stock_raster)) / 10000
      area_ha <- n_pixels * pixel_area_ha

      # Total stock
      total_stock_Mg <- sum(stock_vals, na.rm = TRUE) * pixel_area_ha

      # Uncertainty
      if (!is.null(se_raster)) {
        se_vals <- values(se_raster, mat = FALSE)
        se_vals <- se_vals[!is.na(se_vals)]
        mean_se <- mean(se_vals, na.rm = TRUE)

        # Conservative
        conservative_stock <- mean_stock - (qnorm((1 + CONFIDENCE_LEVEL) / 2) * mean_se)
        conservative_stock <- max(conservative_stock, 0)
        conservative_total_Mg <- conservative_stock * area_ha
      } else {
        mean_se <- NA
        conservative_stock <- NA
        conservative_total_Mg <- NA
      }

      overall_stats <- rbind(overall_stats, data.frame(
        method = method,
        depth_interval = interval_label,
        area_ha = area_ha,
        mean_stock_Mg_ha = mean_stock,
        sd_stock_Mg_ha = sd_stock,
        median_stock_Mg_ha = median_stock,
        min_stock_Mg_ha = min_stock,
        max_stock_Mg_ha = max_stock,
        se_stock_Mg_ha = mean_se,
        conservative_stock_Mg_ha = conservative_stock,
        total_stock_Mg = total_stock_Mg,
        conservative_total_Mg = conservative_total_Mg,
        n_pixels = n_pixels
      ))
    }
  }

  # Save overall statistics for this method
  write.csv(overall_stats,
            sprintf("outputs/carbon_stocks/carbon_stocks_overall_%s.csv", method),
            row.names = FALSE)

  log_message(sprintf("  Saved %s overall statistics", method))
}

# ============================================================================
# COMPARE KRIGING VS RF METHODS
# ============================================================================

if (length(METHODS_AVAILABLE) > 1 && all(c("kriging", "rf") %in% METHODS_AVAILABLE)) {

  log_message("\n\n=== COMPARING KRIGING VS RF METHODS ===")

  method_comparison <- data.frame()

  # Compare for each depth interval
  for (i in 1:nrow(VM0033_DEPTH_INTERVALS)) {

    depth_mid <- VM0033_DEPTH_INTERVALS$depth_midpoint[i]
    depth_top <- VM0033_DEPTH_INTERVALS$depth_top[i]
    depth_bottom <- VM0033_DEPTH_INTERVALS$depth_bottom[i]
    interval_label <- sprintf("%.0f-%.0f cm", depth_top, depth_bottom)

    log_message(sprintf("\nComparing %s:", interval_label))

    # Get stocks from both methods
    kriging_stock <- method_aggregated_stocks$kriging[[interval_label]]
    rf_stock <- method_aggregated_stocks$rf[[interval_label]]

    if (is.null(kriging_stock) || is.null(rf_stock)) {
      log_message(sprintf("  Skipping %s - missing data for one or both methods", interval_label), "WARNING")
      next
    }

    # Extract raster values
    kriging_vals <- values(kriging_stock$mean, mat = FALSE)
    rf_vals <- values(rf_stock$mean, mat = FALSE)

    # Only compare where both have values
    valid_idx <- !is.na(kriging_vals) & !is.na(rf_vals)
    kriging_vals <- kriging_vals[valid_idx]
    rf_vals <- rf_vals[valid_idx]

    if (length(kriging_vals) > 0) {

      # Calculate statistics
      mean_kriging <- mean(kriging_vals, na.rm = TRUE)
      mean_rf <- mean(rf_vals, na.rm = TRUE)

      # Difference
      diff_vals <- rf_vals - kriging_vals
      mean_diff <- mean(diff_vals, na.rm = TRUE)
      sd_diff <- sd(diff_vals, na.rm = TRUE)
      percent_diff <- (mean_diff / mean_kriging) * 100

      # Correlation
      correlation <- cor(kriging_vals, rf_vals, use = "complete.obs")

      # RMSD
      rmsd <- sqrt(mean((kriging_vals - rf_vals)^2, na.rm = TRUE))

      # Agreement (% within 10 Mg C/ha)
      agreement_10 <- sum(abs(diff_vals) <= 10) / length(diff_vals) * 100

      log_message(sprintf("  Kriging: %.1f Mg C/ha", mean_kriging))
      log_message(sprintf("  RF:      %.1f Mg C/ha", mean_rf))
      log_message(sprintf("  Diff:    %.1f Mg C/ha (%.1f%%)", mean_diff, percent_diff))
      log_message(sprintf("  RMSD:    %.1f Mg C/ha", rmsd))
      log_message(sprintf("  Corr:    %.3f", correlation))
      log_message(sprintf("  Agreement: %.1f%% within 10 Mg C/ha", agreement_10))

      # Store results
      method_comparison <- rbind(method_comparison, data.frame(
        depth_interval = interval_label,
        depth_midpoint = depth_mid,
        mean_kriging_Mg_ha = mean_kriging,
        mean_rf_Mg_ha = mean_rf,
        mean_difference_Mg_ha = mean_diff,
        sd_difference_Mg_ha = sd_diff,
        percent_difference = percent_diff,
        rmsd_Mg_ha = rmsd,
        correlation = correlation,
        agreement_within_10_pct = agreement_10,
        n_pixels = length(kriging_vals)
      ))
    }
  }

  # Save comparison
  write.csv(method_comparison,
            "outputs/carbon_stocks/carbon_stocks_method_comparison.csv",
            row.names = FALSE)

  log_message("\nSaved method comparison statistics")

  # Create comparison visualization
  if (nrow(method_comparison) > 0) {

    library(ggplot2)

    p_comparison <- ggplot(method_comparison,
                           aes(x = depth_midpoint)) +
      geom_line(aes(y = mean_kriging_Mg_ha, color = "Kriging"), size = 1) +
      geom_point(aes(y = mean_kriging_Mg_ha, color = "Kriging"), size = 3) +
      geom_line(aes(y = mean_rf_Mg_ha, color = "Random Forest"), size = 1) +
      geom_point(aes(y = mean_rf_Mg_ha, color = "Random Forest"), size = 3) +
      scale_color_manual(values = c("Kriging" = "#2196F3", "Random Forest" = "#4CAF50")) +
      labs(
        title = "Comparison of Kriging vs Random Forest Carbon Stock Estimates",
        x = "Depth Midpoint (cm)",
        y = "Carbon Stock (Mg C/ha)",
        color = "Method"
      ) +
      theme_minimal() +
      theme(legend.position = "top")

    ggsave("outputs/carbon_stocks/method_comparison_by_depth.png",
           p_comparison, width = 10, height = 6, dpi = 300)

    log_message("Saved method comparison plot")
  }

} else {
  log_message("\n\nSkipping method comparison - need both kriging and RF results", "INFO")
}

# ============================================================================
# CREATE VM0033 CONSERVATIVE ESTIMATES
# ============================================================================

log_message("\nCreating VM0033 conservative estimates...")

for (method in METHODS_AVAILABLE) {

  log_message(sprintf("\n  Processing %s VM0033 estimates...", toupper(method)))

  vm0033_estimates <- data.frame()

  # Get total stock for this method
  total_stock_data <- method_total_stocks[[method]]

  if (is.null(total_stock_data)) {
    log_message(sprintf("  No total stock data for %s", method), "WARNING")
    next
  }

  # Extract overall statistics
  total_vals <- values(total_stock_data$mean, mat = FALSE)
  total_vals <- total_vals[!is.na(total_vals)]

  if (length(total_vals) > 0) {

    mean_total <- mean(total_vals, na.rm = TRUE)
    n_pixels <- length(total_vals)
    pixel_area_ha <- prod(res(total_stock_data$mean)) / 10000
    area_ha <- n_pixels * pixel_area_ha
    total_stock_Mg <- sum(total_vals, na.rm = TRUE) * pixel_area_ha

    # Conservative estimate
    if (!is.null(total_stock_data$se)) {
      se_vals <- values(total_stock_data$se, mat = FALSE)
      se_vals <- se_vals[!is.na(se_vals)]
      mean_se <- mean(se_vals, na.rm = TRUE)

      conservative_total <- mean_total - (qnorm((1 + CONFIDENCE_LEVEL) / 2) * mean_se)
      conservative_total <- max(conservative_total, 0)
      conservative_stock_Mg <- conservative_total * area_ha
    } else {
      conservative_total <- NA
      conservative_stock_Mg <- NA
    }

    # Overall estimate
    vm0033_estimates <- rbind(vm0033_estimates, data.frame(
      method = method,
      stratum = "ALL",
      area_ha = area_ha,
      mean_stock_0_100_Mg_ha = mean_total,
      conservative_stock_0_100_Mg_ha = conservative_total,
      total_stock_0_100_Mg = total_stock_Mg,
      conservative_total_0_100_Mg = conservative_stock_Mg
    ))

    log_message(sprintf("  Overall: %.1f Mg C/ha (area: %.1f ha)", mean_total, area_ha))
    if (!is.na(conservative_total)) {
      log_message(sprintf("  Conservative: %.1f Mg C/ha", conservative_total))
    }
  }

  # Add stratum-level VM0033 estimates if available
  if (HAS_STRATUM_RASTER && !is.null(total_stock_data)) {

    log_message("  Adding stratum-level VM0033 estimates...")

    # Process each stratum
    for (i in 1:nrow(stratum_levels)) {

      stratum_id <- stratum_levels$ID[i]
      stratum_name <- stratum_levels$stratum[i]

      # Create mask for this stratum
      stratum_mask <- stratum_raster == stratum_id

      # Mask the total stock raster for this stratum
      stratum_stock <- total_stock_data$mean * stratum_mask
      stratum_se <- if (!is.null(total_stock_data$se)) {
        total_stock_data$se * stratum_mask
      } else {
        NULL
      }

      # Extract values
      stock_vals <- values(stratum_stock, mat = FALSE)
      stock_vals <- stock_vals[!is.na(stock_vals) & stock_vals > 0]

      if (length(stock_vals) == 0) {
        log_message(sprintf("    Skipping %s (no valid pixels)", stratum_name), "WARNING")
        next
      }

      # Calculate statistics
      mean_stock <- mean(stock_vals, na.rm = TRUE)
      n_pixels <- length(stock_vals)
      pixel_area_ha <- prod(res(stratum_stock)) / 10000
      area_ha <- n_pixels * pixel_area_ha
      total_stock_Mg <- sum(stock_vals, na.rm = TRUE) * pixel_area_ha

      # Conservative estimate
      if (!is.null(stratum_se)) {
        se_vals <- values(stratum_se, mat = FALSE)
        se_vals <- se_vals[!is.na(se_vals) & se_vals > 0]

        if (length(se_vals) > 0) {
          mean_se <- mean(se_vals, na.rm = TRUE)
          conservative_stock <- mean_stock - (qnorm((1 + CONFIDENCE_LEVEL) / 2) * mean_se)
          conservative_stock <- max(conservative_stock, 0)
          conservative_total_Mg <- conservative_stock * area_ha
        } else {
          conservative_stock <- NA
          conservative_total_Mg <- NA
        }
      } else {
        conservative_stock <- NA
        conservative_total_Mg <- NA
      }

      # Add stratum row
      vm0033_estimates <- rbind(vm0033_estimates, data.frame(
        method = method,
        stratum = stratum_name,
        area_ha = area_ha,
        mean_stock_0_100_Mg_ha = mean_stock,
        conservative_stock_0_100_Mg_ha = conservative_stock,
        total_stock_0_100_Mg = total_stock_Mg,
        conservative_total_0_100_Mg = conservative_total_Mg
      ))

      log_message(sprintf("    %s: %.1f Mg C/ha (area: %.1f ha)",
                         stratum_name, mean_stock, area_ha))
    }
  }

  # Save VM0033 estimates for this method
  write.csv(vm0033_estimates,
            sprintf("outputs/carbon_stocks/carbon_stocks_conservative_vm0033_%s.csv", method),
            row.names = FALSE)

  log_message(sprintf("  Saved %s VM0033 conservative estimates", method))
}

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

log_message("\nCreating visualizations...")

# Load all VM0033 estimates for visualization
all_vm0033_estimates <- data.frame()

for (method in METHODS_AVAILABLE) {
  file_path <- sprintf("outputs/carbon_stocks/carbon_stocks_conservative_vm0033_%s.csv", method)
  if (file.exists(file_path)) {
    estimates <- read.csv(file_path)
    all_vm0033_estimates <- rbind(all_vm0033_estimates, estimates)
  }
}

if (nrow(all_vm0033_estimates) > 0) {

  # Plot overall estimates by method
  p_methods <- ggplot(all_vm0033_estimates,
                      aes(x = method, y = mean_stock_0_100_Mg_ha, fill = method)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = conservative_stock_0_100_Mg_ha,
                      ymax = mean_stock_0_100_Mg_ha),
                  width = 0.2, color = "red") +
    geom_text(aes(label = sprintf("%.1f", mean_stock_0_100_Mg_ha)),
              vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("kriging" = "#2196F3", "rf" = "#4CAF50")) +
    labs(
      title = "Total Carbon Stocks (0-100 cm) by Prediction Method",
      subtitle = "Red bars show conservative estimates (lower 95% CI) per VM0033",
      x = "Prediction Method",
      y = "Carbon Stock (Mg C/ha)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave("outputs/carbon_stocks/carbon_stocks_by_method.png",
         p_methods, width = 8, height = 6, dpi = 300)

  log_message("Saved method visualization")
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("MODULE 06 COMPLETE\n")
cat("========================================\n\n")

cat("Carbon Stock Aggregation Summary:\n")
cat("----------------------------------------\n")
cat(sprintf("Methods analyzed: %s\n", paste(METHODS_AVAILABLE, collapse=", ")))
cat(sprintf("VM0033 depth intervals: %d layers\n", nrow(VM0033_DEPTH_INTERVALS)))

if (nrow(all_vm0033_estimates) > 0) {
  cat("\nVM0033 Conservative Estimates (0-100 cm):\n")
  cat("------------------------------------------\n")

  for (i in 1:nrow(all_vm0033_estimates)) {
    row <- all_vm0033_estimates[i, ]
    cat(sprintf("\n%s Method:\n", toupper(row$method)))
    cat(sprintf("  Area: %.1f ha\n", row$area_ha))
    cat(sprintf("  Mean Stock: %.2f Mg C/ha\n", row$mean_stock_0_100_Mg_ha))

    if (!is.na(row$conservative_stock_0_100_Mg_ha)) {
      cat(sprintf("  Conservative Stock: %.2f Mg C/ha (VM0033 compliant)\n",
                  row$conservative_stock_0_100_Mg_ha))
    }

    cat(sprintf("  Total Stock: %.0f Mg C\n", row$total_stock_0_100_Mg))

    if (!is.na(row$conservative_total_0_100_Mg)) {
      cat(sprintf("  Conservative Total: %.0f Mg C\n", row$conservative_total_0_100_Mg))
    }
  }
}

# Method comparison summary
if (length(METHODS_AVAILABLE) > 1) {
  comparison_file <- "outputs/carbon_stocks/carbon_stocks_method_comparison.csv"
  if (file.exists(comparison_file)) {
    comparison <- read.csv(comparison_file)
    if (nrow(comparison) > 0) {
      cat("\n\nMethod Comparison Summary:\n")
      cat("------------------------------------------\n")
      cat(sprintf("Average correlation: %.3f\n", mean(comparison$correlation, na.rm = TRUE)))
      cat(sprintf("Average RMSD: %.1f Mg C/ha\n", mean(comparison$rmsd_Mg_ha, na.rm = TRUE)))
      cat(sprintf("Average agreement (±10 Mg C/ha): %.1f%%\n",
                 mean(comparison$agreement_within_10_pct, na.rm = TRUE)))
    }
  }
}

cat("\n\nOutputs:\n")
cat("----------------------------------------\n")
cat("  Carbon stock maps: outputs/carbon_stocks/maps/\n")

if (HAS_STRATUM_RASTER) {
  cat(sprintf("  Stratum statistics (by method): carbon_stocks_by_stratum_*.csv\n"))
}

cat("  Overall statistics (by method): carbon_stocks_overall_*.csv\n")
cat("  VM0033 estimates (by method): carbon_stocks_conservative_vm0033_*.csv\n")

if (length(METHODS_AVAILABLE) > 1) {
  cat("  Method comparison: carbon_stocks_method_comparison.csv\n")
}

cat("\nVM0033/ORRAA Compliance:\n")
cat("----------------------------------------\n")
cat("  ✓ Conservative estimates calculated (lower 95% CI)\n")
cat(sprintf("  ✓ VM0033 depth intervals: %d layers (%s)\n",
           nrow(VM0033_DEPTH_INTERVALS),
           paste(sprintf("%.0f-%.0f cm", VM0033_DEPTH_INTERVALS$depth_top,
                        VM0033_DEPTH_INTERVALS$depth_bottom), collapse=", ")))

if (HAS_STRATUM_RASTER) {
  cat("  ✓ Stratum-specific calculations\n")
}

cat("  ✓ Uncertainty propagated from Modules 04 & 05\n")
cat("  ✓ Both kriging and RF methods compared\n")
cat("  ✓ Carbon stocks converted from kg/m² to Mg C/ha for VM0033 reporting\n")

cat("\nKey Changes:\n")
cat("  • Modules 04 & 05 now output carbon stocks (kg/m²) instead of SOC\n")
cat("  • Automatic conversion to Mg C/ha (×10) for VM0033 compliance\n")
cat("  • Carbon stocks pre-calculated from harmonized SOC + BD in Module 03\n")

cat("\nNext steps:\n")
cat("  1. Review carbon stock maps in outputs/carbon_stocks/maps/\n")
cat("  2. Compare kriging vs RF estimates in method_comparison.csv\n")

if (HAS_STRATUM_RASTER) {
  cat("  3. Verify stratum-level estimates\n")
  cat("  4. Run Module 07 for MMRV reporting\n")
  cat("  5. Prepare verification package\n\n")
} else {
  cat("  3. Run Module 07 for MMRV reporting\n")
  cat("  4. Prepare verification package\n\n")
}

log_message("=== MODULE 06 COMPLETE ===")
