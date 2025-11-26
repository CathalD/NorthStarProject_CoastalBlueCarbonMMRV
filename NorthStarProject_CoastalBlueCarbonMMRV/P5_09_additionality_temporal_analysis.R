# ============================================================================
# MODULE 09: ADDITIONALITY & TEMPORAL CHANGE ANALYSIS
# ============================================================================
# PURPOSE: Calculate project vs baseline differences and temporal trends
# INPUTS:
#   - data_temporal/carbon_stocks_aligned.rds (from Module 08)
# OUTPUTS:
#   - outputs/additionality/*.csv, *.tif
#   - outputs/temporal_change/*.csv, *.tif, *.png
# ============================================================================
# IMPORTANT: Run Module 08 FIRST to harmonize temporal datasets
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

# Load configuration
if (file.exists("blue_carbon_config.R")) {
  source("blue_carbon_config.R")
} else {
  stop("Configuration file not found. Run 00_setup_bluecarbon.R first.")
}

# Verify required config variables
required_vars <- c("ADDITIONALITY_CONFIDENCE", "ADDITIONALITY_METHOD",
                   "MIN_YEARS_FOR_CHANGE", "VALID_SCENARIOS")
missing_vars <- required_vars[!sapply(required_vars, exists)]
if (length(missing_vars) > 0) {
  stop(sprintf("Configuration error: Missing required variables: %s\nPlease check blue_carbon_config.R",
               paste(missing_vars, collapse=", ")))
}

# Initialize logging
log_file <- file.path("logs", paste0("additionality_temporal_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 09: ADDITIONALITY & TEMPORAL CHANGE ANALYSIS ===")

# Load packages
suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

log_message("Packages loaded successfully")

# Create output directories
dir.create("outputs/additionality", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/temporal_change", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/temporal_change/plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# LOAD HARMONIZED TEMPORAL DATA
# ============================================================================

log_message("Loading harmonized temporal dataset...")

temporal_data_file <- "data_temporal/carbon_stocks_aligned.rds"

if (!file.exists(temporal_data_file)) {
  stop(sprintf("Temporal data file not found: %s\nPlease run Module 08 first.",
               temporal_data_file))
}

temporal_data <- readRDS(temporal_data_file)

metadata <- temporal_data$metadata
carbon_stocks <- temporal_data$carbon_stocks
stratum_coverage <- temporal_data$stratum_coverage
rasters <- temporal_data$rasters

log_message(sprintf("Loaded %d datasets:", nrow(metadata)))
for (i in 1:nrow(metadata)) {
  log_message(sprintf("  - %s (%d)", metadata$scenario[i], metadata$year[i]))
}

# ============================================================================
# ANALYZE WHAT COMPARISONS ARE POSSIBLE
# ============================================================================

log_message("\nDetermining possible analyses...")

has_baseline <- "BASELINE" %in% metadata$scenario
project_scenarios <- metadata$scenario[grepl("^PROJECT", metadata$scenario)]
has_project <- length(project_scenarios) > 0
n_years <- length(unique(metadata$year))

run_additionality <- has_baseline && has_project
run_temporal_change <- n_years >= 2

if (!run_additionality && !run_temporal_change) {
  stop("Insufficient data for any temporal analysis.\nNeed either: (1) BASELINE + PROJECT scenarios, or (2) Multiple years")
}

cat("\n========================================\n")
cat("ANALYSIS PLAN\n")
cat("========================================\n\n")

if (run_additionality) {
  cat("✓ Additionality Analysis (PROJECT - BASELINE)\n")
  cat(sprintf("  PROJECT scenarios found: %s\n", paste(project_scenarios, collapse = ", ")))
} else {
  cat("✗ Additionality Analysis (requires BASELINE and PROJECT)\n")
}

if (run_temporal_change) {
  cat(sprintf("✓ Temporal Change Analysis (%d time points)\n", n_years))
} else {
  cat("✗ Temporal Change Analysis (requires >= 2 years)\n")
}
cat("\n")

# ============================================================================
# PART A: ADDITIONALITY ANALYSIS (PROJECT - BASELINE)
# ============================================================================

if (run_additionality) {

  log_message("\n=== ADDITIONALITY ANALYSIS ===")

  # Get baseline data - all 4 VM0033 intervals plus total
  baseline_data <- carbon_stocks %>%
    filter(scenario == "BASELINE") %>%
    group_by(stratum) %>%
    summarise(
      baseline_year = first(year),
      # VM0033 Interval 1: 0-15cm
      baseline_0_15_mean = mean(`carbon_stock_0-15cm`, na.rm = TRUE),
      baseline_0_15_se = mean(`carbon_stock_se_0-15cm`, na.rm = TRUE),
      # VM0033 Interval 2: 15-30cm
      baseline_15_30_mean = mean(`carbon_stock_15-30cm`, na.rm = TRUE),
      baseline_15_30_se = mean(`carbon_stock_se_15-30cm`, na.rm = TRUE),
      # VM0033 Interval 3: 30-50cm
      baseline_30_50_mean = mean(`carbon_stock_30-50cm`, na.rm = TRUE),
      baseline_30_50_se = mean(`carbon_stock_se_30-50cm`, na.rm = TRUE),
      # VM0033 Interval 4: 50-100cm
      baseline_50_100_mean = mean(`carbon_stock_50-100cm`, na.rm = TRUE),
      baseline_50_100_se = mean(`carbon_stock_se_50-100cm`, na.rm = TRUE),
      # Total 0-100cm
      baseline_total_mean = mean(`carbon_stock_0-100cm total`, na.rm = TRUE),
      baseline_total_se = mean(`carbon_stock_se_0-100cm total`, na.rm = TRUE),
      .groups = "drop"
    )

  # Get project data (all scenarios starting with "PROJECT") - all 4 VM0033 intervals plus total
  project_data <- carbon_stocks %>%
    filter(grepl("^PROJECT", scenario)) %>%
    group_by(scenario, stratum) %>%
    summarise(
      project_year = first(year),
      # VM0033 Interval 1: 0-15cm
      project_0_15_mean = mean(`carbon_stock_0-15cm`, na.rm = TRUE),
      project_0_15_se = mean(`carbon_stock_se_0-15cm`, na.rm = TRUE),
      # VM0033 Interval 2: 15-30cm
      project_15_30_mean = mean(`carbon_stock_15-30cm`, na.rm = TRUE),
      project_15_30_se = mean(`carbon_stock_se_15-30cm`, na.rm = TRUE),
      # VM0033 Interval 3: 30-50cm
      project_30_50_mean = mean(`carbon_stock_30-50cm`, na.rm = TRUE),
      project_30_50_se = mean(`carbon_stock_se_30-50cm`, na.rm = TRUE),
      # VM0033 Interval 4: 50-100cm
      project_50_100_mean = mean(`carbon_stock_50-100cm`, na.rm = TRUE),
      project_50_100_se = mean(`carbon_stock_se_50-100cm`, na.rm = TRUE),
      # Total 0-100cm
      project_total_mean = mean(`carbon_stock_0-100cm total`, na.rm = TRUE),
      project_total_se = mean(`carbon_stock_se_0-100cm total`, na.rm = TRUE),
      .groups = "drop"
    )

  # For each PROJECT scenario, calculate additionality
  all_additionality <- list()

  for (proj_scenario in unique(project_data$scenario)) {

    log_message(sprintf("\nCalculating additionality for: %s vs BASELINE", proj_scenario))

    proj_subset <- project_data %>% filter(scenario == proj_scenario)

    # Merge and calculate differences for all 4 VM0033 intervals + total
    additionality <- inner_join(baseline_data, proj_subset, by = "stratum") %>%
      mutate(
        project_scenario = proj_scenario,

        # ========== VM0033 INTERVAL 1: 0-15cm ==========
        delta_0_15_mean = project_0_15_mean - baseline_0_15_mean,
        delta_0_15_var = baseline_0_15_se^2 + project_0_15_se^2,
        delta_0_15_se = sqrt(delta_0_15_var),
        delta_0_15_ci_lower = delta_0_15_mean - 1.96 * delta_0_15_se,
        delta_0_15_ci_upper = delta_0_15_mean + 1.96 * delta_0_15_se,
        delta_0_15_conservative = pmax(0, delta_0_15_ci_lower),
        pct_change_0_15 = 100 * delta_0_15_mean / baseline_0_15_mean,
        t_stat_0_15 = delta_0_15_mean / delta_0_15_se,
        p_value_0_15 = 2 * pt(-abs(t_stat_0_15), df = Inf),
        significant_0_15 = p_value_0_15 < (1 - ADDITIONALITY_CONFIDENCE),

        # ========== VM0033 INTERVAL 2: 15-30cm ==========
        delta_15_30_mean = project_15_30_mean - baseline_15_30_mean,
        delta_15_30_var = baseline_15_30_se^2 + project_15_30_se^2,
        delta_15_30_se = sqrt(delta_15_30_var),
        delta_15_30_ci_lower = delta_15_30_mean - 1.96 * delta_15_30_se,
        delta_15_30_ci_upper = delta_15_30_mean + 1.96 * delta_15_30_se,
        delta_15_30_conservative = pmax(0, delta_15_30_ci_lower),
        pct_change_15_30 = 100 * delta_15_30_mean / baseline_15_30_mean,
        t_stat_15_30 = delta_15_30_mean / delta_15_30_se,
        p_value_15_30 = 2 * pt(-abs(t_stat_15_30), df = Inf),
        significant_15_30 = p_value_15_30 < (1 - ADDITIONALITY_CONFIDENCE),

        # ========== VM0033 INTERVAL 3: 30-50cm ==========
        delta_30_50_mean = project_30_50_mean - baseline_30_50_mean,
        delta_30_50_var = baseline_30_50_se^2 + project_30_50_se^2,
        delta_30_50_se = sqrt(delta_30_50_var),
        delta_30_50_ci_lower = delta_30_50_mean - 1.96 * delta_30_50_se,
        delta_30_50_ci_upper = delta_30_50_mean + 1.96 * delta_30_50_se,
        delta_30_50_conservative = pmax(0, delta_30_50_ci_lower),
        pct_change_30_50 = 100 * delta_30_50_mean / baseline_30_50_mean,
        t_stat_30_50 = delta_30_50_mean / delta_30_50_se,
        p_value_30_50 = 2 * pt(-abs(t_stat_30_50), df = Inf),
        significant_30_50 = p_value_30_50 < (1 - ADDITIONALITY_CONFIDENCE),

        # ========== VM0033 INTERVAL 4: 50-100cm ==========
        delta_50_100_mean = project_50_100_mean - baseline_50_100_mean,
        delta_50_100_var = baseline_50_100_se^2 + project_50_100_se^2,
        delta_50_100_se = sqrt(delta_50_100_var),
        delta_50_100_ci_lower = delta_50_100_mean - 1.96 * delta_50_100_se,
        delta_50_100_ci_upper = delta_50_100_mean + 1.96 * delta_50_100_se,
        delta_50_100_conservative = pmax(0, delta_50_100_ci_lower),
        pct_change_50_100 = 100 * delta_50_100_mean / baseline_50_100_mean,
        t_stat_50_100 = delta_50_100_mean / delta_50_100_se,
        p_value_50_100 = 2 * pt(-abs(t_stat_50_100), df = Inf),
        significant_50_100 = p_value_50_100 < (1 - ADDITIONALITY_CONFIDENCE),

        # ========== TOTAL 0-100cm ==========
        delta_total_mean = project_total_mean - baseline_total_mean,
        delta_total_var = baseline_total_se^2 + project_total_se^2,
        delta_total_se = sqrt(delta_total_var),
        delta_total_ci_lower = delta_total_mean - 1.96 * delta_total_se,
        delta_total_ci_upper = delta_total_mean + 1.96 * delta_total_se,
        delta_total_conservative = pmax(0, delta_total_ci_lower),
        pct_change_total = 100 * delta_total_mean / baseline_total_mean,
        t_stat_total = delta_total_mean / delta_total_se,
        p_value_total = 2 * pt(-abs(t_stat_total), df = Inf),
        significant_total = p_value_total < (1 - ADDITIONALITY_CONFIDENCE),

        # Additionality assessment (based on total 0-100cm)
        additionality_status = case_when(
          !significant_total ~ "Not Significant",
          delta_total_conservative <= 0 ~ "No Net Gain (conservative)",
          delta_total_conservative > 0 & delta_total_conservative < 5 ~ "Marginal (<5 Mg/ha)",
          delta_total_conservative >= 5 & delta_total_conservative < 20 ~ "Moderate (5-20 Mg/ha)",
          delta_total_conservative >= 20 ~ "Substantial (>20 Mg/ha)",
          TRUE ~ "Unknown"
        )
      )

    # Store results
    all_additionality[[proj_scenario]] <- additionality

    # Display results for this scenario - all 4 VM0033 intervals + total
    cat("\n========================================\n")
    cat(sprintf("ADDITIONALITY: %s vs BASELINE\n", proj_scenario))
    cat("========================================\n\n")

    for (i in 1:nrow(additionality)) {
      cat(sprintf("Stratum: %s\n", additionality$stratum[i]))
      cat("\n")

      # VM0033 Interval 1: 0-15cm
      cat("  INTERVAL 1 (0-15cm):\n")
      cat(sprintf("    Baseline: %.2f ± %.2f Mg C/ha\n",
                  additionality$baseline_0_15_mean[i], additionality$baseline_0_15_se[i]))
      cat(sprintf("    Project:  %.2f ± %.2f Mg C/ha\n",
                  additionality$project_0_15_mean[i], additionality$project_0_15_se[i]))
      cat(sprintf("    Delta:    %.2f ± %.2f Mg C/ha (%.1f%%) %s\n",
                  additionality$delta_0_15_mean[i], additionality$delta_0_15_se[i],
                  additionality$pct_change_0_15[i],
                  ifelse(additionality$significant_0_15[i], "*", "")))

      # VM0033 Interval 2: 15-30cm
      cat("  INTERVAL 2 (15-30cm):\n")
      cat(sprintf("    Baseline: %.2f ± %.2f Mg C/ha\n",
                  additionality$baseline_15_30_mean[i], additionality$baseline_15_30_se[i]))
      cat(sprintf("    Project:  %.2f ± %.2f Mg C/ha\n",
                  additionality$project_15_30_mean[i], additionality$project_15_30_se[i]))
      cat(sprintf("    Delta:    %.2f ± %.2f Mg C/ha (%.1f%%) %s\n",
                  additionality$delta_15_30_mean[i], additionality$delta_15_30_se[i],
                  additionality$pct_change_15_30[i],
                  ifelse(additionality$significant_15_30[i], "*", "")))

      # VM0033 Interval 3: 30-50cm
      cat("  INTERVAL 3 (30-50cm):\n")
      cat(sprintf("    Baseline: %.2f ± %.2f Mg C/ha\n",
                  additionality$baseline_30_50_mean[i], additionality$baseline_30_50_se[i]))
      cat(sprintf("    Project:  %.2f ± %.2f Mg C/ha\n",
                  additionality$project_30_50_mean[i], additionality$project_30_50_se[i]))
      cat(sprintf("    Delta:    %.2f ± %.2f Mg C/ha (%.1f%%) %s\n",
                  additionality$delta_30_50_mean[i], additionality$delta_30_50_se[i],
                  additionality$pct_change_30_50[i],
                  ifelse(additionality$significant_30_50[i], "*", "")))

      # VM0033 Interval 4: 50-100cm
      cat("  INTERVAL 4 (50-100cm):\n")
      cat(sprintf("    Baseline: %.2f ± %.2f Mg C/ha\n",
                  additionality$baseline_50_100_mean[i], additionality$baseline_50_100_se[i]))
      cat(sprintf("    Project:  %.2f ± %.2f Mg C/ha\n",
                  additionality$project_50_100_mean[i], additionality$project_50_100_se[i]))
      cat(sprintf("    Delta:    %.2f ± %.2f Mg C/ha (%.1f%%) %s\n",
                  additionality$delta_50_100_mean[i], additionality$delta_50_100_se[i],
                  additionality$pct_change_50_100[i],
                  ifelse(additionality$significant_50_100[i], "*", "")))

      # Total 0-100cm
      cat("  TOTAL (0-100cm):\n")
      cat(sprintf("    Baseline: %.2f ± %.2f Mg C/ha\n",
                  additionality$baseline_total_mean[i], additionality$baseline_total_se[i]))
      cat(sprintf("    Project:  %.2f ± %.2f Mg C/ha\n",
                  additionality$project_total_mean[i], additionality$project_total_se[i]))
      cat(sprintf("    Delta:    %.2f ± %.2f Mg C/ha (%.1f%%) %s\n",
                  additionality$delta_total_mean[i], additionality$delta_total_se[i],
                  additionality$pct_change_total[i],
                  ifelse(additionality$significant_total[i], "*", "")))
      cat(sprintf("    95%% CI: [%.2f, %.2f] Mg C/ha\n",
                  additionality$delta_total_ci_lower[i], additionality$delta_total_ci_upper[i]))
      cat(sprintf("    Conservative: %.2f Mg C/ha\n",
                  additionality$delta_total_conservative[i]))
      cat(sprintf("    Status: %s\n", additionality$additionality_status[i]))
      cat("\n")
      cat("  * = Statistically significant at 95% confidence\n")
      cat("\n")
    }

    # Save additionality results for this scenario
    output_file <- sprintf("outputs/additionality/additionality_%s_vs_BASELINE.csv", proj_scenario)
    write_csv(additionality, output_file)
    log_message(sprintf("Saved: %s", output_file))

    # Calculate project-wide summary for this scenario
    project_wide <- additionality %>%
      summarise(
        n_strata = n(),
        mean_delta_total = mean(delta_total_mean, na.rm = TRUE),
        mean_delta_conservative = mean(delta_total_conservative, na.rm = TRUE),
        total_significant = sum(significant_total, na.rm = TRUE)
      )

    cat("PROJECT-WIDE SUMMARY\n")
    cat("----------------------------------------\n")
    cat(sprintf("Strata analyzed: %d\n", project_wide$n_strata))
    cat(sprintf("Mean additionality: %.2f Mg C/ha\n", project_wide$mean_delta_total))
    cat(sprintf("Conservative estimate: %.2f Mg C/ha\n", project_wide$mean_delta_conservative))
    cat(sprintf("Significant strata: %d / %d\n\n", project_wide$total_significant, project_wide$n_strata))

  }  # End loop over PROJECT scenarios

  # Combine all additionality results
  all_additionality_combined <- bind_rows(all_additionality)
  write_csv(all_additionality_combined, "outputs/additionality/additionality_all_scenarios.csv")
  log_message("Saved: outputs/additionality/additionality_all_scenarios.csv")

  # ========================================================================
  # ADDITIONALITY RASTER MAPS
  # ========================================================================

  if (!is.null(rasters)) {
    log_message("\nCreating additionality raster maps...")

    # Find baseline and project rasters
    baseline_id <- sprintf("BASELINE_%d", unique(baseline_data$baseline_year))
    project_id <- sprintf("PROJECT_%d", unique(project_data$project_year))

    if (baseline_id %in% names(rasters) && project_id %in% names(rasters)) {

      # Calculate difference rasters for all 4 VM0033 intervals + total
      depth_types <- c(
        "interval_0_15_mean", "interval_15_30_mean", "interval_30_50_mean",
        "interval_50_100_mean", "total_mean",
        "interval_0_15_conservative", "interval_15_30_conservative",
        "interval_30_50_conservative", "interval_50_100_conservative",
        "total_conservative"
      )

      for (depth_type in depth_types) {

        if (depth_type %in% names(rasters[[baseline_id]]) &&
            depth_type %in% names(rasters[[project_id]])) {

          baseline_raster <- rasters[[baseline_id]][[depth_type]]
          project_raster <- rasters[[project_id]][[depth_type]]

          # Calculate difference (PROJECT - BASELINE)
          diff_raster <- project_raster - baseline_raster

          # Save difference raster
          output_file <- file.path("outputs/additionality",
                                   sprintf("additionality_%s.tif", depth_type))
          writeRaster(diff_raster, output_file, overwrite = TRUE)
          log_message(sprintf("  Saved: additionality_%s.tif", depth_type))

          # Create significance map (where difference > 0)
          sig_raster <- ifel(diff_raster > 0, 1, 0)
          sig_file <- file.path("outputs/additionality",
                               sprintf("significance_%s.tif", depth_type))
          writeRaster(sig_raster, sig_file, overwrite = TRUE)
        }
      }

      log_message("Additionality raster maps created")
    } else {
      log_message("WARNING: Could not find baseline/project rasters for mapping", "WARNING")
    }
  }

} # End additionality analysis

# ============================================================================
# PART B: TEMPORAL CHANGE ANALYSIS (MULTI-PERIOD)
# ============================================================================

if (run_temporal_change) {

  log_message("\n=== TEMPORAL CHANGE ANALYSIS ===")

  # Calculate change over time for each stratum
  temporal_change <- carbon_stocks %>%
    arrange(stratum, year) %>%
    group_by(stratum) %>%
    mutate(
      n_years = n(),
      year_span = max(year) - min(year)
    ) %>%
    ungroup()

  # Calculate year-to-year changes for all 4 VM0033 intervals + total
  temporal_trends <- carbon_stocks %>%
    arrange(stratum, scenario, year) %>%
    group_by(stratum, scenario) %>%
    summarise(
      n_timepoints = n(),
      years = paste(year, collapse = ", "),
      first_year = min(year),
      last_year = max(year),
      year_span = max(year) - min(year),

      # VM0033 Interval 1: 0-15cm
      carbon_0_15_t0 = first(`carbon_stock_0-15cm`),
      carbon_0_15_tn = last(`carbon_stock_0-15cm`),
      change_0_15 = carbon_0_15_tn - carbon_0_15_t0,
      rate_0_15_Mg_ha_yr = ifelse(year_span > 0, change_0_15 / year_span, NA),
      pct_change_0_15 = 100 * change_0_15 / carbon_0_15_t0,

      # VM0033 Interval 2: 15-30cm
      carbon_15_30_t0 = first(`carbon_stock_15-30cm`),
      carbon_15_30_tn = last(`carbon_stock_15-30cm`),
      change_15_30 = carbon_15_30_tn - carbon_15_30_t0,
      rate_15_30_Mg_ha_yr = ifelse(year_span > 0, change_15_30 / year_span, NA),
      pct_change_15_30 = 100 * change_15_30 / carbon_15_30_t0,

      # VM0033 Interval 3: 30-50cm
      carbon_30_50_t0 = first(`carbon_stock_30-50cm`),
      carbon_30_50_tn = last(`carbon_stock_30-50cm`),
      change_30_50 = carbon_30_50_tn - carbon_30_50_t0,
      rate_30_50_Mg_ha_yr = ifelse(year_span > 0, change_30_50 / year_span, NA),
      pct_change_30_50 = 100 * change_30_50 / carbon_30_50_t0,

      # VM0033 Interval 4: 50-100cm
      carbon_50_100_t0 = first(`carbon_stock_50-100cm`),
      carbon_50_100_tn = last(`carbon_stock_50-100cm`),
      change_50_100 = carbon_50_100_tn - carbon_50_100_t0,
      rate_50_100_Mg_ha_yr = ifelse(year_span > 0, change_50_100 / year_span, NA),
      pct_change_50_100 = 100 * change_50_100 / carbon_50_100_t0,

      # Total 0-100cm
      carbon_total_t0 = first(`carbon_stock_0-100cm total`),
      carbon_total_tn = last(`carbon_stock_0-100cm total`),
      total_change = carbon_total_tn - carbon_total_t0,
      rate_total_Mg_ha_yr = ifelse(year_span > 0, total_change / year_span, NA),
      pct_change_total = 100 * total_change / carbon_total_t0,

      .groups = "drop"
    )

  cat("\n========================================\n")
  cat("TEMPORAL TRENDS BY STRATUM\n")
  cat("========================================\n\n")

  for (i in 1:nrow(temporal_trends)) {
    cat(sprintf("Stratum: %s (%s)\n", temporal_trends$stratum[i], temporal_trends$scenario[i]))
    cat(sprintf("  Time span: %d years (%d - %d)\n",
                temporal_trends$year_span[i],
                temporal_trends$first_year[i],
                temporal_trends$last_year[i]))
    cat("\n")

    # Interval 1: 0-15cm
    cat("  INTERVAL 1 (0-15cm):\n")
    cat(sprintf("    t0: %.2f Mg C/ha → tn: %.2f Mg C/ha\n",
                temporal_trends$carbon_0_15_t0[i], temporal_trends$carbon_0_15_tn[i]))
    cat(sprintf("    Change: %.2f Mg C/ha (%.1f%%), Rate: %.3f Mg C/ha/yr\n",
                temporal_trends$change_0_15[i], temporal_trends$pct_change_0_15[i],
                temporal_trends$rate_0_15_Mg_ha_yr[i]))

    # Interval 2: 15-30cm
    cat("  INTERVAL 2 (15-30cm):\n")
    cat(sprintf("    t0: %.2f Mg C/ha → tn: %.2f Mg C/ha\n",
                temporal_trends$carbon_15_30_t0[i], temporal_trends$carbon_15_30_tn[i]))
    cat(sprintf("    Change: %.2f Mg C/ha (%.1f%%), Rate: %.3f Mg C/ha/yr\n",
                temporal_trends$change_15_30[i], temporal_trends$pct_change_15_30[i],
                temporal_trends$rate_15_30_Mg_ha_yr[i]))

    # Interval 3: 30-50cm
    cat("  INTERVAL 3 (30-50cm):\n")
    cat(sprintf("    t0: %.2f Mg C/ha → tn: %.2f Mg C/ha\n",
                temporal_trends$carbon_30_50_t0[i], temporal_trends$carbon_30_50_tn[i]))
    cat(sprintf("    Change: %.2f Mg C/ha (%.1f%%), Rate: %.3f Mg C/ha/yr\n",
                temporal_trends$change_30_50[i], temporal_trends$pct_change_30_50[i],
                temporal_trends$rate_30_50_Mg_ha_yr[i]))

    # Interval 4: 50-100cm
    cat("  INTERVAL 4 (50-100cm):\n")
    cat(sprintf("    t0: %.2f Mg C/ha → tn: %.2f Mg C/ha\n",
                temporal_trends$carbon_50_100_t0[i], temporal_trends$carbon_50_100_tn[i]))
    cat(sprintf("    Change: %.2f Mg C/ha (%.1f%%), Rate: %.3f Mg C/ha/yr\n",
                temporal_trends$change_50_100[i], temporal_trends$pct_change_50_100[i],
                temporal_trends$rate_50_100_Mg_ha_yr[i]))

    # Total 0-100cm
    cat("  TOTAL (0-100cm):\n")
    cat(sprintf("    t0: %.2f Mg C/ha → tn: %.2f Mg C/ha\n",
                temporal_trends$carbon_total_t0[i], temporal_trends$carbon_total_tn[i]))
    cat(sprintf("    Change: %.2f Mg C/ha (%.1f%%), Rate: %.3f Mg C/ha/yr\n",
                temporal_trends$total_change[i], temporal_trends$pct_change_total[i],
                temporal_trends$rate_total_Mg_ha_yr[i]))
    cat("\n")
  }

  # Save temporal trends
  write_csv(temporal_trends, "outputs/temporal_change/temporal_trends_by_stratum.csv")
  log_message("Saved: outputs/temporal_change/temporal_trends_by_stratum.csv")

  # Create time series plot
  log_message("\nCreating time series plots...")

  for (stratum_name in unique(carbon_stocks$stratum)) {
    stratum_data <- carbon_stocks %>%
      filter(stratum == stratum_name)

    # Plot total 0-100cm carbon stock trajectory
    p <- ggplot(stratum_data, aes(x = year, y = `carbon_stock_0-100cm total`, color = scenario)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = `carbon_stock_0-100cm total` - `carbon_stock_se_0-100cm total`,
                        ymax = `carbon_stock_0-100cm total` + `carbon_stock_se_0-100cm total`),
                    width = 0.5) +
      labs(
        title = sprintf("Carbon Stock Trajectory: %s", stratum_name),
        x = "Year",
        y = "Total Carbon Stock 0-100cm (Mg C/ha)",
        color = "Scenario"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "bottom"
      )

    plot_file <- file.path("outputs/temporal_change/plots",
                          sprintf("trajectory_total_%s.png", gsub(" ", "_", tolower(stratum_name))))
    ggsave(plot_file, p, width = 8, height = 6, dpi = 300)
    log_message(sprintf("  Saved plot: %s", basename(plot_file)))

    # Create stacked area plot showing all 4 VM0033 intervals
    # Reshape data for stacked plot
    stratum_long <- stratum_data %>%
      select(year, scenario, `carbon_stock_0-15cm`, `carbon_stock_15-30cm`,
             `carbon_stock_30-50cm`, `carbon_stock_50-100cm`) %>%
      pivot_longer(cols = starts_with("carbon_stock_"),
                   names_to = "interval",
                   values_to = "carbon_stock") %>%
      mutate(interval = factor(interval,
                              levels = c("carbon_stock_50-100cm", "carbon_stock_30-50cm",
                                       "carbon_stock_15-30cm", "carbon_stock_0-15cm"),
                              labels = c("50-100cm", "30-50cm", "15-30cm", "0-15cm")))

    p_stacked <- ggplot(stratum_long, aes(x = year, y = carbon_stock, fill = interval)) +
      geom_area(position = "stack") +
      facet_wrap(~scenario, ncol = 1) +
      labs(
        title = sprintf("Carbon Stock by Depth: %s", stratum_name),
        x = "Year",
        y = "Carbon Stock (Mg C/ha)",
        fill = "VM0033 Interval"
      ) +
      scale_fill_brewer(palette = "YlOrBr") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "bottom"
      )

    plot_file_stacked <- file.path("outputs/temporal_change/plots",
                          sprintf("trajectory_stacked_%s.png", gsub(" ", "_", tolower(stratum_name))))
    ggsave(plot_file_stacked, p_stacked, width = 8, height = 6, dpi = 300)
    log_message(sprintf("  Saved plot: %s", basename(plot_file_stacked)))
  }

} # End temporal change analysis

# ============================================================================
# SUMMARY REPORT
# ============================================================================

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("Outputs created:\n")

if (run_additionality) {
  cat("  Additionality:\n")
  cat("    - outputs/additionality/additionality_by_stratum.csv\n")
  cat("    - outputs/additionality/additionality_*.tif (raster maps)\n")
}

if (run_temporal_change) {
  cat("  Temporal Change:\n")
  cat("    - outputs/temporal_change/temporal_trends_by_stratum.csv\n")
  cat("    - outputs/temporal_change/plots/*.png (time series)\n")
}

cat("\n")
cat("Next step: Generate final VM0033 verification package (Module 11)\n")
cat("\n")

log_message("=== MODULE 09 COMPLETE ===")
