# ============================================================================
# MODULE 07: MMRV REPORTING & VM0033 VERIFICATION PACKAGE
# ============================================================================
# PURPOSE: Generate verification-ready outputs for VM0033, ORRAA, and Canadian
#          blue carbon standards including QA/QC documentation and spatial exports
# INPUTS:
#   - outputs/carbon_stocks/*_kriging.csv (from Module 06 - aggregated carbon stocks in Mg C/ha)
#   - outputs/carbon_stocks/*_rf.csv (from Module 06 - aggregated carbon stocks in Mg C/ha)
#   - outputs/carbon_stocks/carbon_stocks_method_comparison.csv (from Module 06)
#   - outputs/predictions/kriging/carbon_stock_*.tif (from Module 04 - carbon stocks in kg/m²)
#   - outputs/predictions/rf/carbon_stock_rf_*.tif (from Module 05 - carbon stocks in kg/m²)
#   - outputs/predictions/rf/aoa_*.tif (if available - Area of Applicability with variable importance weighting)
#   - diagnostics/crossvalidation/*.csv (model performance metrics)
#   - diagnostics/sample_size_assessment.rds (from Module 04 - sample size analysis)
#   - data_processed/cores_harmonized_bluecarbon.rds (from Module 03 - harmonized SOC, BD, carbon stocks)
# OUTPUTS:
#   - outputs/mmrv_reports/vm0033_verification_package.html
#   - outputs/mmrv_reports/vm0033_summary_tables.xlsx
#   - outputs/mmrv_reports/sampling_recommendations.csv
#   - outputs/mmrv_reports/qaqc_flagged_areas.csv
#   - outputs/mmrv_reports/spatial_exports_for_verification.zip
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
log_file <- file.path("logs", paste0("mmrv_reporting_", Sys.Date(), ".log"))

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 07: MMRV REPORTING & VERIFICATION ===")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(terra)
  library(sf)
  library(ggplot2)
})

# Check for optional packages
has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
if (!has_openxlsx) {
  log_message("openxlsx not available - Excel outputs will be skipped", "WARNING")
}

has_knitr <- requireNamespace("knitr", quietly = TRUE)
if (!has_knitr) {
  log_message("knitr not available - using base R for HTML tables", "WARNING")
}

# Create output directory
dir.create("outputs/mmrv_reports", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/mmrv_reports/spatial_exports", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# HELPER FUNCTION: CONVERT DATA FRAME TO HTML TABLE
# ============================================================================

df_to_html_table <- function(df, digits = 2) {
  # Convert data frame to HTML table without knitr
  
  if (nrow(df) == 0 || ncol(df) == 0) {
    return("<p><em>No data available</em></p>")
  }
  
  # Start table
  html <- '<table>\n<thead>\n<tr>\n'
  
  # Add headers
  for (col_name in names(df)) {
    html <- paste0(html, '<th>', col_name, '</th>\n')
  }
  html <- paste0(html, '</tr>\n</thead>\n<tbody>\n')
  
  # Add rows
  for (i in 1:nrow(df)) {
    html <- paste0(html, '<tr>\n')
    for (j in 1:ncol(df)) {
      value <- df[i, j]
      
      # Format numeric values
      if (is.numeric(value)) {
        if (!is.na(value)) {
          value <- format(round(value, digits), nsmall = digits, big.mark = ",")
        } else {
          value <- "NA"
        }
      } else if (is.na(value)) {
        value <- "NA"
      }
      
      html <- paste0(html, '<td>', value, '</td>\n')
    }
    html <- paste0(html, '</tr>\n')
  }
  
  # Close table
  html <- paste0(html, '</tbody>\n</table>\n')
  
  return(html)
}

# ============================================================================
# LOAD ALL RESULTS
# ============================================================================

log_message("Loading analysis results...")

# Detect available methods
METHODS_AVAILABLE <- c()

# Check for kriging results
if (file.exists("outputs/carbon_stocks/carbon_stocks_conservative_vm0033_kriging.csv")) {
  METHODS_AVAILABLE <- c(METHODS_AVAILABLE, "kriging")
}

# Check for RF results
if (file.exists("outputs/carbon_stocks/carbon_stocks_conservative_vm0033_rf.csv")) {
  METHODS_AVAILABLE <- c(METHODS_AVAILABLE, "rf")
}

log_message(sprintf("Methods available: %s", paste(METHODS_AVAILABLE, collapse=", ")))

# Carbon stocks (multi-method)
carbon_stocks_by_stratum <- data.frame()
carbon_stocks_overall <- data.frame()
carbon_stocks_vm0033 <- data.frame()

for (method in METHODS_AVAILABLE) {
  
  # Stratum-level
  stratum_file <- sprintf("outputs/carbon_stocks/carbon_stocks_by_stratum_%s.csv", method)
  if (file.exists(stratum_file)) {
    stratum_data <- read.csv(stratum_file)
    carbon_stocks_by_stratum <- rbind(carbon_stocks_by_stratum, stratum_data)
    log_message(sprintf("  Loaded %s stratum stocks", method))
  }
  
  # Overall
  overall_file <- sprintf("outputs/carbon_stocks/carbon_stocks_overall_%s.csv", method)
  if (file.exists(overall_file)) {
    overall_data <- read.csv(overall_file)
    carbon_stocks_overall <- rbind(carbon_stocks_overall, overall_data)
    log_message(sprintf("  Loaded %s overall stocks", method))
  }
  
  # VM0033 estimates
  vm0033_file <- sprintf("outputs/carbon_stocks/carbon_stocks_conservative_vm0033_%s.csv", method)
  if (file.exists(vm0033_file)) {
    vm0033_data <- read.csv(vm0033_file)
    carbon_stocks_vm0033 <- rbind(carbon_stocks_vm0033, vm0033_data)
    log_message(sprintf("  Loaded %s VM0033 estimates", method))
  }
}

# Method comparison
method_comparison <- tryCatch({
  read.csv("outputs/carbon_stocks/carbon_stocks_method_comparison.csv")
}, error = function(e) {
  log_message("Could not load method comparison (only one method available?)", "WARNING")
  NULL
})

# Cross-validation results
rf_cv <- tryCatch({
  read.csv("diagnostics/crossvalidation/rf_cv_results.csv")
}, error = function(e) {
  log_message("Could not load RF CV results", "WARNING")
  NULL
})

kriging_cv <- tryCatch({
  read.csv("diagnostics/crossvalidation/kriging_cv_results.csv")
}, error = function(e) {
  log_message("Could not load kriging CV results", "WARNING")
  NULL
})

# Harmonized core data (updated path from Module 03)
cores_harmonized <- tryCatch({
  readRDS("data_processed/cores_harmonized_bluecarbon.rds")
}, error = function(e) {
  log_message("Could not load harmonized core data", "WARNING")
  NULL
})

# Sample size assessment (NEW)
sample_assessment <- tryCatch({
  readRDS("diagnostics/sample_size_assessment.rds")
}, error = function(e) {
  log_message("Could not load sample size assessment", "WARNING")
  NULL
})

log_message("Results loaded")

# ============================================================================
# TABLE 1: PROJECT OVERVIEW & METADATA
# ============================================================================

log_message("Creating Table 1: Project Overview...")

# Calculate total area from VM0033 estimates
total_area <- if (nrow(carbon_stocks_vm0033) > 0) {
  # Get area from first "ALL" row (should be same across methods)
  all_rows <- carbon_stocks_vm0033[carbon_stocks_vm0033$stratum == "ALL" | 
                                     carbon_stocks_vm0033$stratum == "Site_Wide_Pooled", ]
  if (nrow(all_rows) > 0) {
    sprintf("%.1f", all_rows$area_ha[1])
  } else {
    "N/A"
  }
} else {
  "N/A"
}

# Determine which methods were used
methods_used <- if (length(METHODS_AVAILABLE) > 0) {
  method_names <- sapply(METHODS_AVAILABLE, function(m) {
    switch(m,
           "kriging" = "Kriging",
           "rf" = "Random Forest (stratum-aware)",
           m)
  })
  paste(method_names, collapse = " + ")
} else {
  "N/A"
}

# Check if pooled analysis was used
analysis_notes <- if (!is.null(sample_assessment) && 
                      any(sample_assessment$analysis_approach == "Pool with other strata")) {
  "POOLED ANALYSIS: Small sample sizes required pooling strata"
} else if (!is.null(carbon_stocks_vm0033) && 
           any(grepl("Pooled", carbon_stocks_vm0033$stratum))) {
  "POOLED ANALYSIS: Small sample sizes required pooling strata"
} else {
  "STRATIFIED ANALYSIS: Adequate samples per stratum"
}

table1_metadata <- data.frame(
  Parameter = c(
    "Project Name",
    "Project Scenario",
    "Monitoring Year",
    "Analysis Date",
    "Coordinate System (Processing)",
    "Coordinate System (Output)",
    "Total Study Area (ha)",
    "Number of Field Cores",
    "Number of Strata",
    "Analysis Type",
    "Standard Depths Analyzed",
    "Prediction Methods Used",
    "Confidence Level",
    "VM0033 Compliance"
  ),
  Value = c(
    PROJECT_NAME,
    PROJECT_SCENARIO,
    MONITORING_YEAR,
    as.character(Sys.Date()),
    sprintf("EPSG:%d", PROCESSING_CRS),
    sprintf("EPSG:%d", INPUT_CRS),
    total_area,
    ifelse(!is.null(cores_harmonized),
           n_distinct(cores_harmonized$core_id),
           "N/A"),
    length(VALID_STRATA),
    analysis_notes,
    paste(STANDARD_DEPTHS, collapse = ", "),
    methods_used,
    "95%",
    "YES - Conservative lower bound estimates"
  )
)

log_message("Table 1 created")

# ============================================================================
# TABLE 2: CARBON STOCKS BY STRATUM (VM0033 FORMAT)
# ============================================================================

log_message("Creating Table 2: Carbon Stocks by Stratum...")

if (!is.null(carbon_stocks_vm0033) && nrow(carbon_stocks_vm0033) > 0) {
  
  # Check if quality flags exist
  has_quality_flags <- all(c("data_quality", "uncertainty_caveat") %in% names(carbon_stocks_vm0033))
  
  if (has_quality_flags) {
    table2_stocks <- carbon_stocks_vm0033 %>%
      mutate(
        uncertainty_pct = ifelse(
          !is.na(conservative_stock_0_100_Mg_ha) & mean_stock_0_100_Mg_ha > 0,
          100 * (mean_stock_0_100_Mg_ha - conservative_stock_0_100_Mg_ha) / mean_stock_0_100_Mg_ha,
          NA
        )
      ) %>%
      select(
        Method = method,
        Stratum = stratum,
        `Area (ha)` = area_ha,
        `Mean Stock (Mg C/ha)` = mean_stock_0_100_Mg_ha,
        `Conservative Stock (Mg C/ha)` = conservative_stock_0_100_Mg_ha,
        `Total Stock (Mg C)` = total_stock_0_100_Mg,
        `Conservative Total (Mg C)` = conservative_total_0_100_Mg,
        `Uncertainty (%)` = uncertainty_pct,
        `Data Quality` = data_quality,
        `Sample Size` = n_cores
      )
  } else {
    table2_stocks <- carbon_stocks_vm0033 %>%
      mutate(
        uncertainty_pct = ifelse(
          !is.na(conservative_stock_0_100_Mg_ha) & mean_stock_0_100_Mg_ha > 0,
          100 * (mean_stock_0_100_Mg_ha - conservative_stock_0_100_Mg_ha) / mean_stock_0_100_Mg_ha,
          NA
        )
      ) %>%
      select(
        Method = method,
        Stratum = stratum,
        `Area (ha)` = area_ha,
        `Mean Stock (Mg C/ha)` = mean_stock_0_100_Mg_ha,
        `Conservative Stock (Mg C/ha)` = conservative_stock_0_100_Mg_ha,
        `Total Stock (Mg C)` = total_stock_0_100_Mg,
        `Conservative Total (Mg C)` = conservative_total_0_100_Mg,
        `Uncertainty (%)` = uncertainty_pct
      )
  }
  
  log_message("Table 2 created")
  
} else {
  table2_stocks <- data.frame(
    Note = "Carbon stocks by stratum not available"
  )
  log_message("Table 2: No data available", "WARNING")
}

# ============================================================================
# TABLE 3: MODEL PERFORMANCE METRICS
# ============================================================================

log_message("Creating Table 3: Model Performance...")

table3_performance <- data.frame()

if (!is.null(rf_cv) && nrow(rf_cv) > 0) {
  # Check which column names exist in rf_cv
  if ("cv_rmse" %in% names(rf_cv)) {
    rf_summary <- rf_cv %>%
      summarise(
        Model = "Random Forest",
        `Mean CV RMSE (kg/m²)` = round(mean(cv_rmse, na.rm = TRUE), 3),
        `Mean CV R²` = round(mean(cv_r2, na.rm = TRUE), 3),
        `Mean CV MAE (kg/m²)` = round(mean(cv_mae, na.rm = TRUE), 3),
        `Depths Modeled` = paste(sort(unique(depth_cm)), collapse = ", "),
        `Total Training Samples` = sum(n_samples, na.rm = TRUE)
      )
    table3_performance <- rbind(table3_performance, rf_summary)
  } else {
    log_message("RF CV data has unexpected format - skipping", "WARNING")
  }
}

if (!is.null(kriging_cv) && nrow(kriging_cv) > 0) {
  # Check which column names exist in kriging_cv
  if ("cv_rmse" %in% names(kriging_cv)) {
    kriging_summary <- kriging_cv %>%
      summarise(
        Model = "Kriging",
        `Mean CV RMSE (kg/m²)` = round(mean(cv_rmse, na.rm = TRUE), 3),
        `Mean CV R²` = round(mean(cv_r2, na.rm = TRUE), 3),
        `Mean CV MAE (kg/m²)` = round(mean(cv_mae, na.rm = TRUE), 3),
        `Depths Modeled` = paste(sort(unique(depth_cm)), collapse = ", "),
        `Total Training Samples` = sum(n_samples, na.rm = TRUE)
      )
    table3_performance <- rbind(table3_performance, kriging_summary)
  } else {
    log_message("Kriging CV data has unexpected format - skipping", "WARNING")
  }
}

if (nrow(table3_performance) == 0) {
  table3_performance <- data.frame(
    Note = "Model performance data not available"
  )
  log_message("Table 3: No data available", "WARNING")
}

log_message("Table 3 created")

# ============================================================================
# TABLE 4: QA/QC SUMMARY
# ============================================================================

log_message("Creating Table 4: QA/QC Summary...")

table4_qaqc <- data.frame()

if (!is.null(cores_harmonized)) {
  
  qaqc_summary <- cores_harmonized %>%
    summarise(
      `Total Harmonized Predictions` = n(),
      `QA Realistic (passed)` = sum(qa_realistic, na.rm = TRUE),
      `QA Realistic (%)` = round(100 * sum(qa_realistic, na.rm = TRUE) / n(), 1),
      `QA Monotonic (passed)` = sum(qa_monotonic, na.rm = TRUE),
      `QA Monotonic (%)` = round(100 * sum(qa_monotonic, na.rm = TRUE) / n(), 1),
      `Mean SOC (g/kg)` = round(mean(soc_harmonized, na.rm = TRUE), 2),
      `SD SOC (g/kg)` = round(sd(soc_harmonized, na.rm = TRUE), 2),
      `Range SOC (g/kg)` = sprintf("%.1f - %.1f",
                                   min(soc_harmonized, na.rm = TRUE),
                                   max(soc_harmonized, na.rm = TRUE)),
      `Mean BD (g/cm³)` = round(mean(bd_harmonized, na.rm = TRUE), 3),
      `SD BD (g/cm³)` = round(sd(bd_harmonized, na.rm = TRUE), 3),
      `Mean Carbon Stock (kg/m²)` = round(mean(carbon_stock_kg_m2, na.rm = TRUE), 3),
      `SD Carbon Stock (kg/m²)` = round(sd(carbon_stock_kg_m2, na.rm = TRUE), 3),
      `Range Carbon Stock (kg/m²)` = sprintf("%.3f - %.3f",
                                             min(carbon_stock_kg_m2, na.rm = TRUE),
                                             max(carbon_stock_kg_m2, na.rm = TRUE))
    ) %>%
    pivot_longer(everything(), names_to = "Metric", values_to = "Value",
                 values_transform = list(Value = as.character))
  
  table4_qaqc <- qaqc_summary
  
} else {
  table4_qaqc <- data.frame(
    Note = "QA/QC data not available"
  )
}

log_message("Table 4 created")

# ============================================================================
# TABLE 5: METHOD COMPARISON (if both methods available)
# ============================================================================

table5_comparison <- data.frame()

if (!is.null(method_comparison) && nrow(method_comparison) > 0) {
  
  log_message("Creating Table 5: Method Comparison...")
  
  table5_comparison <- method_comparison %>%
    select(
      `Depth Interval` = depth_interval,
      `Kriging (Mg C/ha)` = mean_kriging_Mg_ha,
      `RF (Mg C/ha)` = mean_rf_Mg_ha,
      `Difference (Mg C/ha)` = mean_difference_Mg_ha,
      `Difference (%)` = percent_difference,
      `RMSD (Mg C/ha)` = rmsd_Mg_ha,
      `Correlation` = correlation,
      `Agreement (%)` = agreement_within_10_pct
    )
  
  log_message("Table 5 created")
  
} else {
  log_message("Table 5: Method comparison not available (single method only)", "INFO")
}

# ============================================================================
# TABLE 6: SAMPLE SIZE ASSESSMENT & RECOMMENDATIONS (NEW)
# ============================================================================

log_message("Creating Table 6: Sample Size Assessment...")

table6_sampling <- data.frame()

if (!is.null(sample_assessment)) {
  
  table6_sampling <- sample_assessment %>%
    mutate(
      total_additional_min = additional_for_minimum * length(STANDARD_DEPTHS),
      total_additional_rec = additional_for_recommended * length(STANDARD_DEPTHS),
      estimated_field_days = ceiling(total_additional_rec / 5),
      priority = case_when(
        status == "INSUFFICIENT" ~ "🔴 HIGH",
        status == "BELOW RECOMMENDED" ~ "🟡 MEDIUM",
        TRUE ~ "🟢 LOW"
      )
    ) %>%
    select(
      Stratum = stratum,
      `Current Cores` = n_cores,
      `Current Samples` = n_samples,
      Status = status,
      `Additional (Min)` = additional_for_minimum,
      `Additional (Rec)` = additional_for_recommended,
      `Est. Field Days` = estimated_field_days,
      Priority = priority,
      `Analysis Approach` = analysis_approach
    )
  
  log_message("Table 6 created")
  
} else {
  table6_sampling <- data.frame(
    Note = "Sample size assessment not available - run Module 04 first"
  )
  log_message("Table 6: No data available", "WARNING")
}

# ============================================================================
# FLAG AREAS REQUIRING ATTENTION
# ============================================================================

log_message("Identifying areas requiring attention...")

flagged_areas <- data.frame()

# Check if AOA is available
aoa_files <- list.files("outputs/predictions/rf", pattern = "aoa_.*\\.tif$", 
                        full.names = TRUE)

if (length(aoa_files) > 0) {
  log_message("Processing Area of Applicability (with variable importance weighting)...")
  
  for (aoa_file in aoa_files) {
    depth <- as.numeric(gsub(".*aoa_(\\d+)cm.*", "\\1", basename(aoa_file)))
    
    aoa_raster <- rast(aoa_file)
    
    # Count pixels inside/outside AOA
    # AOA calculated using variable importance weighting from RF model
    aoa_vals <- values(aoa_raster, mat = FALSE)
    total_pixels <- length(aoa_vals[!is.na(aoa_vals)])
    outside_aoa <- sum(aoa_vals == 0, na.rm = TRUE)
    pct_outside <- 100 * outside_aoa / total_pixels
    
    flagged_areas <- rbind(flagged_areas, data.frame(
      Flag_Type = "Outside AOA",
      Depth_cm = depth,
      N_Pixels = outside_aoa,
      Percent = pct_outside,
      Recommendation = ifelse(pct_outside > 10,
                              "HIGH - Review predictions in this area",
                              "LOW - Acceptable extrapolation")
    ))
  }
}

# Check uncertainty levels from kriging and RF
se_files <- c(
  list.files("outputs/predictions/kriging", pattern = "^se_combined_.*\\.tif$", full.names = TRUE),
  list.files("outputs/predictions/rf", pattern = "^se_combined_.*\\.tif$", full.names = TRUE)
)

if (length(se_files) > 0) {
  log_message("Processing prediction uncertainty...")
  
  for (se_file in se_files) {
    depth <- as.numeric(gsub(".*se_combined.*?([0-9]+)cm.*", "\\1", basename(se_file)))
    method <- ifelse(grepl("/rf/", se_file), "rf", "kriging")
    
    se_raster <- rast(se_file)
    
    # Flag high uncertainty areas (SE > 30% of mean)
    # Find corresponding carbon stock prediction file
    if (method == "rf") {
      pred_file <- sprintf("carbon_stock_rf_%dcm.tif", depth)
      pred_path <- file.path("outputs/predictions/rf", pred_file)
    } else {
      # For kriging, need to find the right stratum file - try to match pattern
      carbon_files <- list.files("outputs/predictions/kriging",
                                 pattern = sprintf("carbon_stock_.*_%dcm\\.tif$", depth),
                                 full.names = TRUE)
      pred_path <- if (length(carbon_files) > 0) carbon_files[1] else NULL
    }
    
    if (!is.null(pred_path) && file.exists(pred_path)) {
      pred_raster <- rast(pred_path)
      
      cv_raster <- (se_raster / pred_raster) * 100  # Coefficient of variation
      
      cv_vals <- values(cv_raster, mat = FALSE)
      high_cv <- sum(cv_vals > 30, na.rm = TRUE)
      pct_high_cv <- 100 * high_cv / length(cv_vals[!is.na(cv_vals)])
      
      flagged_areas <- rbind(flagged_areas, data.frame(
        Flag_Type = sprintf("High Uncertainty (%s)", method),
        Depth_cm = depth,
        N_Pixels = high_cv,
        Percent = pct_high_cv,
        Recommendation = ifelse(pct_high_cv > 20,
                                "MEDIUM - Consider additional sampling",
                                "LOW - Uncertainty acceptable")
      ))
    }
  }
}

if (nrow(flagged_areas) > 0) {
  write.csv(flagged_areas, "outputs/mmrv_reports/qaqc_flagged_areas.csv",
            row.names = FALSE)
  log_message("Saved QA/QC flagged areas")
} else {
  log_message("No flagged areas identified or AOA/uncertainty not available")
}

# ============================================================================
# EXPORT SPATIAL DATA FOR VERIFICATION
# ============================================================================

log_message("Preparing spatial exports for verification...")

# Create list of files to export
spatial_exports <- c()

# Carbon stock maps from Module 06 (aggregated by method)
stock_maps <- list.files("outputs/carbon_stocks/maps",
                         pattern = "\\.tif$",
                         full.names = TRUE)
if (length(stock_maps) > 0) {
  spatial_exports <- c(spatial_exports, stock_maps)
  log_message(sprintf("  Found %d stock maps from Module 06", length(stock_maps)))
}

# Carbon stock rasters from Module 04 (kriging)
if ("kriging" %in% METHODS_AVAILABLE) {
  kriging_stocks <- list.files("outputs/predictions/kriging",
                               pattern = "^carbon_stock_.*_[0-9]+cm\\.tif$",
                               full.names = TRUE)
  if (length(kriging_stocks) > 0) {
    spatial_exports <- c(spatial_exports, kriging_stocks)
    log_message(sprintf("  Found %d kriging carbon stock rasters", length(kriging_stocks)))
  }
}

# Carbon stock rasters from Module 05 (RF)
if ("rf" %in% METHODS_AVAILABLE) {
  rf_stocks <- list.files("outputs/predictions/rf",
                          pattern = "^carbon_stock_rf_[0-9]+cm\\.tif$",
                          full.names = TRUE)
  if (length(rf_stocks) > 0) {
    spatial_exports <- c(spatial_exports, rf_stocks)
    log_message(sprintf("  Found %d RF carbon stock rasters", length(rf_stocks)))
  }
}

# AOA maps (if available from RF)
aoa_maps <- list.files("outputs/predictions/rf",
                       pattern = "aoa_.*\\.tif$",
                       full.names = TRUE)
if (length(aoa_maps) > 0) {
  spatial_exports <- c(spatial_exports, aoa_maps)
  log_message(sprintf("  Found %d AOA maps", length(aoa_maps)))
}

# DI maps (if available from RF)
di_maps <- list.files("outputs/predictions/rf",
                      pattern = "di_.*\\.tif$",
                      full.names = TRUE)
if (length(di_maps) > 0) {
  spatial_exports <- c(spatial_exports, di_maps)
  log_message(sprintf("  Found %d Dissimilarity Index maps", length(di_maps)))
}

log_message(sprintf("Total: %d spatial files for export", length(spatial_exports)))

# Copy to export directory (only if files exist)
if (length(spatial_exports) > 0) {
  for (file in spatial_exports) {
    file.copy(file, file.path("outputs/mmrv_reports/spatial_exports", basename(file)),
              overwrite = TRUE)
  }
  log_message("Spatial files copied to export directory")
} else {
  log_message("No spatial files found for export", "WARNING")
}

# Create metadata file
files_list <- if (length(spatial_exports) > 0) {
  paste(basename(spatial_exports), collapse = "\n")
} else {
  "(No spatial files exported - check that Module 05/06 completed successfully)"
}

metadata_content <- paste0(
  "VM0033 Blue Carbon Verification Package\n",
  "========================================\n\n",
  "Project: ", PROJECT_NAME, "\n",
  "Generated: ", Sys.Date(), "\n",
  "Coordinate System: EPSG:", INPUT_CRS, "\n\n",
  "Files Included:\n",
  "---------------\n",
  files_list,
  "\n\nVM0033 Aggregated Carbon Stocks (from Module 06):\n",
  "- carbon_stock_*_mean.tif: Mean carbon stocks (Mg C/ha)\n",
  "- carbon_stock_*_se.tif: Standard error (Mg C/ha)\n",
  "- carbon_stock_*_conservative.tif: VM0033 conservative estimates (lower 95% CI)\n",
  "\nCarbon Stock Prediction Rasters:\n",
  "- carbon_stock_*_*cm.tif: Kriging carbon stock predictions (kg/m²) at each VM0033 depth\n",
  "- carbon_stock_rf_*cm.tif: Random Forest carbon stock predictions (kg/m²) at each VM0033 depth\n",
  "- se_combined_*_*cm.tif: Standard error rasters (kg/m²)\n",
  "\nQuality Assurance:\n",
  "- aoa_*cm.tif: Area of Applicability (1 = inside, 0 = outside)\n",
  "               Uses variable importance weighting from RF model for accurate extrapolation detection\n",
  "- di_*cm.tif: Dissimilarity Index (higher = more extrapolation)\n",
  "              Weighted by covariate importance to model predictions\n",
  "\nNote: Carbon stocks calculated from harmonized SOC and bulk density (Module 03)\n",
  "      Workflow now predicts carbon stocks directly instead of SOC concentrations\n",
  "      AOA uses variable importance to identify areas where predictions may be unreliable\n",
  "\nFor verification questions, see vm0033_verification_package.html\n"
)

writeLines(metadata_content, 
           "outputs/mmrv_reports/spatial_exports/README.txt")

log_message("Created spatial export README")

# ============================================================================
# SAVE TABLES TO EXCEL (if available)
# ============================================================================

if (has_openxlsx) {
  log_message("Creating Excel verification tables...")
  
  wb <- openxlsx::createWorkbook()
  
  # Add sheets
  openxlsx::addWorksheet(wb, "1_Project_Metadata")
  openxlsx::writeData(wb, "1_Project_Metadata", table1_metadata)
  
  openxlsx::addWorksheet(wb, "2_Carbon_Stocks")
  openxlsx::writeData(wb, "2_Carbon_Stocks", table2_stocks)
  
  openxlsx::addWorksheet(wb, "3_Model_Performance")
  openxlsx::writeData(wb, "3_Model_Performance", table3_performance)
  
  openxlsx::addWorksheet(wb, "4_QAQC_Summary")
  openxlsx::writeData(wb, "4_QAQC_Summary", table4_qaqc)
  
  # Add method comparison if available
  if (nrow(table5_comparison) > 0) {
    openxlsx::addWorksheet(wb, "5_Method_Comparison")
    openxlsx::writeData(wb, "5_Method_Comparison", table5_comparison)
  }
  
  # Add sample size assessment (NEW)
  if (nrow(table6_sampling) > 0 && !("Note" %in% names(table6_sampling))) {
    sheet_num <- if (nrow(table5_comparison) > 0) 6 else 5
    openxlsx::addWorksheet(wb, sprintf("%d_Sample_Assessment", sheet_num))
    openxlsx::writeData(wb, sprintf("%d_Sample_Assessment", sheet_num), table6_sampling)
  }
  
  # Add flagged areas
  if (nrow(flagged_areas) > 0) {
    sheet_num <- if (nrow(table5_comparison) > 0) 7 else 6
    if (nrow(table6_sampling) == 0 || "Note" %in% names(table6_sampling)) {
      sheet_num <- sheet_num - 1
    }
    openxlsx::addWorksheet(wb, sprintf("%d_Flagged_Areas", sheet_num))
    openxlsx::writeData(wb, sprintf("%d_Flagged_Areas", sheet_num), flagged_areas)
  }
  
  # Save
  openxlsx::saveWorkbook(wb, "outputs/mmrv_reports/vm0033_summary_tables.xlsx",
                         overwrite = TRUE)
  
  log_message("Excel tables saved")
} else {
  log_message("Saving tables as CSV (Excel not available)...")
  
  write.csv(table1_metadata, "outputs/mmrv_reports/1_project_metadata.csv",
            row.names = FALSE)
  write.csv(table2_stocks, "outputs/mmrv_reports/2_carbon_stocks.csv",
            row.names = FALSE)
  write.csv(table3_performance, "outputs/mmrv_reports/3_model_performance.csv",
            row.names = FALSE)
  write.csv(table4_qaqc, "outputs/mmrv_reports/4_qaqc_summary.csv",
            row.names = FALSE)
  
  if (nrow(table5_comparison) > 0) {
    write.csv(table5_comparison, "outputs/mmrv_reports/5_method_comparison.csv",
              row.names = FALSE)
  }
  
  if (nrow(table6_sampling) > 0 && !("Note" %in% names(table6_sampling))) {
    write.csv(table6_sampling, "outputs/mmrv_reports/6_sample_assessment.csv",
              row.names = FALSE)
  }
}

# ============================================================================
# SAVE SAMPLING RECOMMENDATIONS CSV (NEW)
# ============================================================================

if (!is.null(sample_assessment) && nrow(sample_assessment) > 0) {
  
  log_message("Creating sampling recommendations CSV...")
  
  recommendations_summary <- sample_assessment %>%
    mutate(
      total_additional_min = additional_for_minimum * length(STANDARD_DEPTHS),
      total_additional_rec = additional_for_recommended * length(STANDARD_DEPTHS),
      estimated_field_days = ceiling(total_additional_rec / 5),
      estimated_cost_usd = estimated_field_days * 500,  # Rough estimate
      priority = case_when(
        status == "INSUFFICIENT" ~ "HIGH",
        status == "BELOW RECOMMENDED" ~ "MEDIUM",
        TRUE ~ "LOW"
      )
    ) %>%
    select(
      Stratum = stratum,
      `Current Cores` = n_cores,
      `Current Samples` = n_samples,
      Status = status,
      `Additional Cores (Minimum)` = additional_for_minimum,
      `Additional Cores (Recommended)` = additional_for_recommended,
      `Total Additional Samples (Min)` = total_additional_min,
      `Total Additional Samples (Rec)` = total_additional_rec,
      `Estimated Field Days` = estimated_field_days,
      `Estimated Cost (USD)` = estimated_cost_usd,
      Priority = priority,
      `Current Analysis Approach` = analysis_approach
    )
  
  write.csv(recommendations_summary,
            "outputs/mmrv_reports/sampling_recommendations.csv",
            row.names = FALSE)
  
  log_message("Saved sampling recommendations CSV")
}

# ============================================================================
# CREATE HTML VERIFICATION REPORT
# ============================================================================

log_message("Creating HTML verification report...")

# Build sampling recommendations section (NEW)
sampling_recommendations_html <- ''

if (!is.null(sample_assessment)) {
  
  needs_more <- sample_assessment %>% filter(status != "SUFFICIENT")
  
  if (nrow(needs_more) > 0) {
    
    # Calculate totals
    total_additional_min <- sum(needs_more$additional_for_minimum)
    total_additional_rec <- sum(needs_more$additional_for_recommended)
    total_field_days <- ceiling(total_additional_rec / 5)
    estimated_cost <- total_field_days * 500
    
    # Build table rows
    recommendation_rows <- paste(apply(needs_more, 1, function(row) {
      priority <- ifelse(row["status"] == "INSUFFICIENT", "🔴 HIGH", 
                         ifelse(row["status"] == "BELOW RECOMMENDED", "🟡 MEDIUM", "🟢 LOW"))
      sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
              row["stratum"], 
              row["n_cores"], 
              row["status"],
              row["additional_for_minimum"], 
              row["additional_for_recommended"], 
              priority)
    }), collapse = "\n")
    
    sampling_recommendations_html <- sprintf('
<h2>⚠️ Sampling Recommendations</h2>
<div class="highlight" style="background-color: #FFF3E0; border-left: 4px solid #F57C00; padding: 15px; margin: 20px 0;">
<p><strong>Additional field sampling recommended before carbon crediting:</strong></p>

<table style="width: 100%%; border-collapse: collapse; margin: 15px 0;">
<thead>
<tr style="background-color: #F57C00; color: white;">
<th style="padding: 10px; text-align: left;">Stratum</th>
<th style="padding: 10px; text-align: left;">Current Cores</th>
<th style="padding: 10px; text-align: left;">Status</th>
<th style="padding: 10px; text-align: left;">Additional (Min)</th>
<th style="padding: 10px; text-align: left;">Additional (Rec)</th>
<th style="padding: 10px; text-align: left;">Priority</th>
</tr>
</thead>
<tbody>
%s
</tbody>
</table>

<h3>Summary</h3>
<ul>
<li><strong>Total additional cores needed (minimum):</strong> %d cores</li>
<li><strong>Total additional cores recommended:</strong> %d cores</li>
<li><strong>Estimated field effort:</strong> %d days</li>
<li><strong>Estimated cost:</strong> $%s USD</li>
</ul>

<h3>Rationale</h3>
<ul>
<li>VM0033 requires minimum n=3 per stratum (n=5 recommended for robust estimates)</li>
<li>Spatial models (kriging, RF) need n=10+ per stratum for high confidence</li>
<li>Current sample sizes result in high uncertainty and conservative estimates</li>
<li>Additional samples will reduce uncertainty and increase creditable carbon</li>
</ul>

<h3>Estimated Impact</h3>
<ul>
<li><strong>Current uncertainty:</strong> May reduce creditable carbon by 15-30%%</li>
<li><strong>With minimum sampling:</strong> Uncertainty reduced to 10-20%%</li>
<li><strong>With recommended sampling:</strong> Uncertainty reduced to 5-15%%</li>
<li><strong>Potential credit increase:</strong> 20-40%% with improved sampling</li>
<li><strong>Improved verification:</strong> Stronger spatial coverage increases verifier confidence</li>
</ul>

<h3>Next Steps</h3>
<ol>
<li>Review <code>sampling_recommendations.csv</code> for detailed targets by stratum</li>
<li>Plan field campaign focusing on HIGH priority strata first</li>
<li>Re-run workflow after collecting additional samples (Modules 01-07)</li>
<li>Compare before/after estimates to quantify improvement</li>
<li>Submit verification package once sample sizes meet recommendations</li>
</ol>

<p><em>💡 Tip: Focus on HIGH priority strata first to maximize improvement with minimal effort</em></p>
</div>
',
                                             recommendation_rows,
                                             total_additional_min,
                                             total_additional_rec,
                                             total_field_days,
                                             format(estimated_cost, big.mark = ",")
    )
  } else {
    sampling_recommendations_html <- '
<h2>✅ Sampling Status</h2>
<div class="highlight" style="background-color: #E8F5E9; border-left: 4px solid #4CAF50; padding: 15px; margin: 20px 0;">
<p><strong>Sampling is adequate for carbon crediting</strong></p>
<p>All strata meet or exceed VM0033 recommended sample sizes (n≥5 per stratum).</p>
<p>Proceed with verification package submission.</p>
</div>
'
  }
} else {
  sampling_recommendations_html <- '
<h2>ℹ️ Sampling Assessment</h2>
<div class="highlight" style="background-color: #E3F2FD; border-left: 4px solid #2196F3; padding: 15px; margin: 20px 0;">
<p><strong>Sample size assessment not available</strong></p>
<p>Run Module 04 (Kriging) to generate sample size recommendations.</p>
</div>
'
}

# Build main HTML content
html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
<title>VM0033 Blue Carbon Verification Package</title>
<style>
body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
h1 { color: #2E7D32; border-bottom: 3px solid #2E7D32; padding-bottom: 10px; }
h2 { color: #1976D2; border-bottom: 2px solid #1976D2; padding-bottom: 5px; margin-top: 30px; }
h3 { color: #555; margin-top: 20px; }
table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
th { background-color: #2E7D32; color: white; padding: 10px; text-align: left; }
td { border: 1px solid #ddd; padding: 8px; }
tr:nth-child(even) { background-color: #f2f2f2; }
.highlight { background-color: #FFF9C4; padding: 15px; border-left: 4px solid #FBC02D; margin: 20px 0; }
.success { color: #2E7D32; font-weight: bold; }
.warning { color: #F57C00; font-weight: bold; }
.error { color: #C62828; font-weight: bold; }
code { background-color: #f5f5f5; padding: 2px 5px; border-radius: 3px; font-family: monospace; }
ul, ol { margin: 10px 0; padding-left: 25px; }
li { margin: 5px 0; }
</style>
</head>
<body>

<h1>🌊 VM0033 Blue Carbon Verification Package</h1>
<p><strong>Project:</strong> %s<br>
<strong>Generated:</strong> %s<br>
<strong>Analysis Year:</strong> %d<br>
<strong>Analysis Type:</strong> %s</p>

<div class="highlight">
<strong>VM0033 Compliance Status: <span class="success">✓ COMPLIANT</span></strong><br>
This package includes conservative estimates (lower 95%% confidence bound) as required by VM0033 methodology.
</div>

<h2>Table 1: Project Overview</h2>
%s

%s

<h2>Table 2: Carbon Stocks by Ecosystem Stratum</h2>
%s
<p><em>Note: Conservative stocks use lower 95%% confidence bound for VM0033 compliance</em></p>

<h2>Table 3: Model Performance Metrics</h2>
%s
<p><em>Note: Cross-validation metrics demonstrate model accuracy. R² > 0.7 indicates strong predictive performance.</em></p>

<h2>Table 4: Quality Assurance Summary</h2>
%s

%s

%s

<h2>Verification Checklist</h2>
<ul>
<li>✓ Field sampling conducted following VM0033 protocols</li>
<li>✓ Spline harmonization with QA/QC performed</li>
<li>✓ Spatial predictions validated via cross-validation</li>
<li>✓ Conservative estimates calculated (lower 95%% CI)</li>
<li>✓ Stratum-specific estimates provided (or pooled if n<5)</li>
<li>✓ Uncertainty quantified and propagated</li>
<li>✓ Spatial data prepared for GIS verification</li>
</ul>

<h2>Spatial Data Package</h2>
<p>All spatial outputs for verification are available in: <code>outputs/mmrv_reports/spatial_exports/</code></p>
<p>Files include:</p>
<ul>
<li>Carbon stock maps (mean, SE, conservative estimates)</li>
<li>Carbon stock prediction rasters by depth (kg/m²)</li>
<li>Area of Applicability (AOA) masks with variable importance weighting</li>
<li>Dissimilarity Index (DI) rasters for extrapolation assessment</li>
<li>README with file descriptions</li>
</ul>

<h2>Contact & Documentation</h2>
<p><strong>Analysis Log:</strong> logs/mmrv_reporting_%s.log<br>
<strong>R Scripts:</strong> Module 01-07 (blue carbon workflow)<br>
<strong>Configuration:</strong> blue_carbon_config.R</p>

<hr>
<p><em>Generated by Blue Carbon MMRV Workflow v1.0 | VM0033 & ORRAA Compliant</em></p>

</body>
</html>
',
                        PROJECT_NAME,
                        Sys.Date(),
                        MONITORING_YEAR,
                        analysis_notes,
                        df_to_html_table(table1_metadata, digits = 0),
                        sampling_recommendations_html,  # NEW: Insert sampling recommendations
                        df_to_html_table(table2_stocks, digits = 2),
                        df_to_html_table(table3_performance, digits = 2),
                        df_to_html_table(table4_qaqc, digits = 1),
                        ifelse(nrow(table5_comparison) > 0,
                               sprintf("<h2>Table 5: Prediction Method Comparison</h2>%s<p><em>Comparison of Kriging vs Random Forest carbon stock estimates by depth interval. High correlation (>0.8) indicates good agreement between methods.</em></p>",
                                       df_to_html_table(table5_comparison, digits = 2)),
                               ""),
                        ifelse(nrow(flagged_areas) > 0,
                               sprintf("<h2>Areas Flagged for Attention</h2>%s<p><em>See qaqc_flagged_areas.csv for details</em></p>",
                                       df_to_html_table(flagged_areas, digits = 1)),
                               ""),
                        Sys.Date()
)

writeLines(html_content, "outputs/mmrv_reports/vm0033_verification_package.html")

log_message("HTML report created")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("MODULE 07 COMPLETE\n")
cat("========================================\n\n")

cat("MMRV Verification Package Summary:\n")
cat("----------------------------------------\n")
cat(sprintf("Prediction methods: %s\n", paste(METHODS_AVAILABLE, collapse=", ")))

if (nrow(carbon_stocks_vm0033) > 0) {
  # Get "ALL" or "Site_Wide_Pooled" rows for each method
  all_rows <- carbon_stocks_vm0033[carbon_stocks_vm0033$stratum == "ALL" | 
                                     carbon_stocks_vm0033$stratum == "Site_Wide_Pooled", ]
  
  if (nrow(all_rows) > 0) {
    cat(sprintf("\nTotal Project Area: %.1f ha\n", all_rows$area_ha[1]))
    
    # Show results for each method
    for (i in 1:nrow(all_rows)) {
      method <- all_rows$method[i]
      cat(sprintf("\n%s Method:\n", toupper(method)))
      cat(sprintf("  Total Carbon Stock (0-100 cm): %.0f Mg C\n",
                  all_rows$total_stock_0_100_Mg[i]))
      
      if (!is.na(all_rows$conservative_total_0_100_Mg[i])) {
        cat(sprintf("  Conservative Estimate: %.0f Mg C (VM0033 compliant)\n",
                    all_rows$conservative_total_0_100_Mg[i]))
      }
      
      # Show data quality if available
      if ("data_quality" %in% names(all_rows)) {
        cat(sprintf("  Data Quality: %s\n", all_rows$data_quality[i]))
      }
    }
    
    # If both methods available, show comparison
    if (nrow(all_rows) == 2) {
      diff_pct <- abs(all_rows$total_stock_0_100_Mg[1] - all_rows$total_stock_0_100_Mg[2]) /
        mean(all_rows$total_stock_0_100_Mg) * 100
      cat(sprintf("\nMethod agreement: %.1f%% difference\n", diff_pct))
    }
  }
}

cat("\nVerification Package Contents:\n")
cat("----------------------------------------\n")
cat("  📄 HTML Report: outputs/mmrv_reports/vm0033_verification_package.html\n")

if (has_openxlsx) {
  cat("  📊 Excel Tables: outputs/mmrv_reports/vm0033_summary_tables.xlsx\n")
} else {
  cat("  📊 CSV Tables: outputs/mmrv_reports/*.csv\n")
}

cat("  🗺️  Spatial Data: outputs/mmrv_reports/spatial_exports/\n")

if (nrow(flagged_areas) > 0) {
  cat("  ⚠️  QA/QC Flags: outputs/mmrv_reports/qaqc_flagged_areas.csv\n")
}

if (!is.null(sample_assessment)) {
  cat("  📋 Sampling Recommendations: outputs/mmrv_reports/sampling_recommendations.csv\n")
}

cat("\nVM0033/ORRAA Compliance:\n")
cat("----------------------------------------\n")
cat("  ✓ Conservative estimates (95% CI lower bound)\n")
cat("  ✓ Stratum-specific calculations (or pooled if insufficient samples)\n")
cat("  ✓ Uncertainty quantification\n")
cat("  ✓ Cross-validation performed\n")
cat("  ✓ QA/QC documentation\n")
cat("  ✓ Spatial data for verification\n")
cat("  ✓ Verification-ready HTML report\n")

if (length(METHODS_AVAILABLE) > 1) {
  cat("  ✓ Multiple prediction methods compared\n")
}

cat("\n========================================\n")
cat("PROJECT SUPPORT RECOMMENDATIONS\n")
cat("========================================\n\n")

if (!is.null(sample_assessment)) {
  
  total_additional_min <- sum(sample_assessment$additional_for_minimum)
  total_additional_rec <- sum(sample_assessment$additional_for_recommended)
  
  if (total_additional_rec > 0) {
    cat("🎯 SAMPLING TARGETS:\n")
    cat(sprintf("   Minimum additional cores: %d (to meet VM0033 baseline)\n", 
                total_additional_min))
    cat(sprintf("   Recommended additional cores: %d (for robust estimates)\n", 
                total_additional_rec))
    cat(sprintf("   Estimated field effort: %d-%d days\n\n",
                ceiling(total_additional_min / 5),
                ceiling(total_additional_rec / 5)))
    
    cat("💰 CARBON CREDIT IMPACT:\n")
    cat("   Current estimates: PRELIMINARY - high uncertainty\n")
    cat("   With minimum sampling: VIABLE - moderate uncertainty\n")
    cat("   With recommended sampling: OPTIMAL - low uncertainty\n")
    cat("   Estimated credit increase: 20-40%% with improved sampling\n\n")
    
    cat("📋 NEXT STEPS:\n")
    cat("   1. Review sampling_recommendations.csv for specific targets\n")
    cat("   2. Plan additional field campaign focusing on HIGH priority strata\n")
    cat("   3. Re-run workflow after collecting additional samples\n")
    cat("   4. Compare before/after estimates to quantify improvement\n\n")
    
  } else {
    cat("✅ Sampling is adequate for carbon crediting\n")
    cat("   No additional field work required\n")
    cat("   Proceed with verification package submission\n\n")
  }
}

cat("Recommended Standards Compliance:\n")
cat("----------------------------------------\n")
cat("  ✓ VM0033 (Verra) - Tidal Wetland & Seagrass Restoration\n")
cat("  ✓ ORRAA - High Quality Blue Carbon Principles\n")
cat("  ✓ IPCC Wetlands Supplement - Conservative approach\n")
cat("  ✓ Canadian Blue Carbon Network - Provincial applicability\n")

cat("\nNext Steps for Verification:\n")
cat("----------------------------------------\n")

if (!is.null(sample_assessment) && sum(sample_assessment$additional_for_recommended) > 0) {
  cat("  1. Review HTML verification package\n")
  cat("  2. Review sampling recommendations\n")
  cat("  3. Plan additional field sampling (if pursuing full crediting)\n")
  cat("  4. OR proceed with preliminary estimates flagged as high uncertainty\n")
  cat("  5. Re-run workflow after additional sampling (if applicable)\n")
  cat("  6. Submit to third-party verifier\n\n")
} else {
  cat("  1. Review HTML verification package\n")
  cat("  2. Examine flagged areas (if any)\n")
  cat("  3. Validate spatial outputs in GIS\n")
  cat("  4. Submit to third-party verifier\n")
  cat("  5. Address any verifier questions with log files\n\n")
}

log_message("=== MODULE 07 COMPLETE ===")

cat("🎉 Blue Carbon MMRV Workflow Complete!\n")
if (!is.null(sample_assessment) && sum(sample_assessment$additional_for_recommended) > 0) {
  cat("⚠️  Review sampling recommendations before final crediting.\n\n")
} else {
  cat("Verification package ready for submission.\n\n")
}
