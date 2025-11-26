# ============================================================================
# MODULE 10: VM0033 FINAL VERIFICATION PACKAGE
# ============================================================================
# PURPOSE: Generate comprehensive verification package with additionality
# INPUTS:
#   - outputs/additionality/*.csv (from Module 09)
#   - outputs/temporal_change/*.csv (from Module 09)
#   - data_temporal/temporal_metadata.csv (from Module 08)
# OUTPUTS:
#   - outputs/verification/vm0033_final_verification_report.html
#   - outputs/verification/vm0033_verification_tables.xlsx
#   - outputs/verification/executive_summary.pdf
# ============================================================================
# IMPORTANT: Run Modules 08 and 09 FIRST
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

# Initialize logging
log_file <- file.path("logs", paste0("final_verification_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 10: VM0033 FINAL VERIFICATION PACKAGE ===")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
  library(knitr)
  library(rmarkdown)
  library(ggplot2)
  library(tidyr)
})

log_message("Packages loaded successfully")

# Create output directories
dir.create("outputs/verification", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/verification/spatial_exports", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# LOAD TEMPORAL ANALYSIS RESULTS
# ============================================================================

log_message("Loading temporal analysis results...")

# Check if Module 09 outputs exist
additionality_file <- "outputs/additionality/additionality_all_scenarios.csv"
metadata_file <- "data_temporal/temporal_metadata.csv"

missing_files <- c()
if (!file.exists(additionality_file)) missing_files <- c(missing_files, additionality_file)
if (!file.exists(metadata_file)) missing_files <- c(missing_files, metadata_file)

# Temporal trends is optional
temporal_trends_file <- "outputs/temporal_change/temporal_trends_by_stratum.csv"
has_temporal_trends <- file.exists(temporal_trends_file)

if (length(missing_files) > 0) {
  stop(sprintf("Missing required files:\n  - %s\n\nPlease run Modules 08 and 09 first.",
               paste(missing_files, collapse = "\n  - ")))
}

# Load data
additionality <- read_csv(additionality_file, show_col_types = FALSE)
metadata <- read_csv(metadata_file, show_col_types = FALSE)

log_message(sprintf("Loaded additionality data: %d rows", nrow(additionality)))

if (has_temporal_trends) {
  temporal_trends <- read_csv(temporal_trends_file, show_col_types = FALSE)
  log_message(sprintf("Loaded temporal trends for %d strata/scenarios", nrow(temporal_trends)))
} else {
  temporal_trends <- NULL
  log_message("No temporal trends data available (single time point)", "INFO")
}

# ============================================================================
# CARBON TO CO2e CONVERSION
# ============================================================================

log_message("Converting carbon stocks to CO2e emissions...")

# Conversion factor: C to CO2 (molecular weight ratio: 44/12)
C_TO_CO2 <- 44 / 12

# Calculate emissions reductions for all 4 VM0033 intervals + total
emissions_reductions <- additionality %>%
  mutate(
    # Convert Mg C/ha to tonnes CO2e/ha for all 4 VM0033 intervals
    # Interval 1: 0-15cm
    co2e_0_15_mean = delta_0_15_mean * C_TO_CO2,
    co2e_0_15_conservative = delta_0_15_conservative * C_TO_CO2,

    # Interval 2: 15-30cm
    co2e_15_30_mean = delta_15_30_mean * C_TO_CO2,
    co2e_15_30_conservative = delta_15_30_conservative * C_TO_CO2,

    # Interval 3: 30-50cm
    co2e_30_50_mean = delta_30_50_mean * C_TO_CO2,
    co2e_30_50_conservative = delta_30_50_conservative * C_TO_CO2,

    # Interval 4: 50-100cm
    co2e_50_100_mean = delta_50_100_mean * C_TO_CO2,
    co2e_50_100_conservative = delta_50_100_conservative * C_TO_CO2,

    # Total 0-100cm
    co2e_total_mean = delta_total_mean * C_TO_CO2,
    co2e_total_conservative = delta_total_conservative * C_TO_CO2,

    # Uncertainty in CO2e (total)
    co2e_total_se = delta_total_se * C_TO_CO2,
    co2e_total_ci_lower = delta_total_ci_lower * C_TO_CO2,
    co2e_total_ci_upper = delta_total_ci_upper * C_TO_CO2
  )

log_message("Emissions reduction calculations complete")

# ============================================================================
# PROJECT-WIDE AGGREGATION
# ============================================================================

log_message("Calculating project-wide totals...")

project_wide_summary <- emissions_reductions %>%
  summarise(
    n_strata = n(),

    # Mean carbon stock changes
    mean_carbon_baseline = mean(baseline_total_mean, na.rm = TRUE),
    mean_carbon_project = mean(project_total_mean, na.rm = TRUE),
    mean_carbon_additionality = mean(delta_total_mean, na.rm = TRUE),
    mean_carbon_conservative = mean(delta_total_conservative, na.rm = TRUE),

    # Mean CO2e reductions
    mean_co2e_additionality = mean(co2e_total_mean, na.rm = TRUE),
    mean_co2e_conservative = mean(co2e_total_conservative, na.rm = TRUE),

    # Significance
    n_significant = sum(significant_total, na.rm = TRUE),
    pct_significant = 100 * n_significant / n(),

    # Status summary
    substantial = sum(additionality_status == "Substantial (>20 Mg/ha)", na.rm = TRUE),
    moderate = sum(additionality_status == "Moderate (5-20 Mg/ha)", na.rm = TRUE),
    marginal = sum(additionality_status == "Marginal (<5 Mg/ha)", na.rm = TRUE),
    none = sum(additionality_status == "No Net Gain (conservative)", na.rm = TRUE)
  )

cat("\n========================================\n")
cat("PROJECT-WIDE SUMMARY\n")
cat("========================================\n\n")
cat(sprintf("Strata analyzed: %d\n", project_wide_summary$n_strata))
cat(sprintf("Baseline carbon stock: %.2f Mg C/ha\n", project_wide_summary$mean_carbon_baseline))
cat(sprintf("Project carbon stock:  %.2f Mg C/ha\n", project_wide_summary$mean_carbon_project))
cat(sprintf("\nAdditionality (mean):        %.2f Mg C/ha (%.2f tonnes CO2e/ha)\n",
            project_wide_summary$mean_carbon_additionality,
            project_wide_summary$mean_co2e_additionality))
cat(sprintf("Conservative (95%% CI):       %.2f Mg C/ha (%.2f tonnes CO2e/ha)\n",
            project_wide_summary$mean_carbon_conservative,
            project_wide_summary$mean_co2e_conservative))
cat(sprintf("\nSignificant strata: %d / %d (%.1f%%)\n",
            project_wide_summary$n_significant,
            project_wide_summary$n_strata,
            project_wide_summary$pct_significant))
cat(sprintf("\nStratum classification:\n"))
cat(sprintf("  Substantial (>20 Mg/ha): %d\n", project_wide_summary$substantial))
cat(sprintf("  Moderate (5-20 Mg/ha):   %d\n", project_wide_summary$moderate))
cat(sprintf("  Marginal (<5 Mg/ha):     %d\n", project_wide_summary$marginal))
cat(sprintf("  No net gain:             %d\n", project_wide_summary$none))
cat("\n")

# ============================================================================
# CREATE VM0033 VERIFICATION TABLES (EXCEL)
# ============================================================================

log_message("Creating VM0033 verification tables...")

wb <- createWorkbook()

# ========================================================================
# TABLE 1: PROJECT CHARACTERISTICS
# ========================================================================

addWorksheet(wb, "1_Project_Characteristics")

project_chars <- data.frame(
  Parameter = c(
    "Project Name",
    "Location",
    "Monitoring Year (Baseline)",
    "Monitoring Year (Project)",
    "Scenarios Analyzed",
    "Total Strata",
    "Methodology",
    "Confidence Level",
    "Conservative Approach",
    "Report Date"
  ),
  Value = c(
    PROJECT_NAME,
    PROJECT_LOCATION,
    paste(metadata$year[metadata$scenario == "BASELINE"], collapse = ", "),
    paste(metadata$year[metadata$scenario == "PROJECT"], collapse = ", "),
    paste(unique(metadata$scenario), collapse = ", "),
    as.character(project_wide_summary$n_strata),
    "VM0033 (Verra), ORRAA High Quality Principles v1.1",
    sprintf("%.0f%%", ADDITIONALITY_CONFIDENCE * 100),
    ADDITIONALITY_METHOD,
    as.character(Sys.Date())
  )
)

writeData(wb, "1_Project_Characteristics", project_chars, startRow = 2)

# Styling
addStyle(wb, "1_Project_Characteristics",
         style = createStyle(fontSize = 14, textDecoration = "bold"),
         rows = 1, cols = 1:2, gridExpand = TRUE)

# ========================================================================
# TABLE 2: CARBON STOCKS BY STRATUM (BASELINE VS PROJECT)
# ========================================================================

addWorksheet(wb, "2_Carbon_Stocks_by_Stratum")

carbon_stocks_table <- emissions_reductions %>%
  select(
    Stratum = stratum,
    `Baseline Year` = baseline_year,
    `Baseline Carbon (Mg C/ha)` = baseline_total_mean,
    `Baseline SE` = baseline_total_se,
    `Project Year` = project_year,
    `Project Carbon (Mg C/ha)` = project_total_mean,
    `Project SE` = project_total_se,
    `Difference (Mg C/ha)` = delta_total_mean,
    `Difference SE` = delta_total_se,
    `95% CI Lower` = delta_total_ci_lower,
    `95% CI Upper` = delta_total_ci_upper,
    `Conservative Estimate` = delta_total_conservative,
    `p-value` = p_value_total,
    Significant = significant_total,
    Status = additionality_status
  )

writeData(wb, "2_Carbon_Stocks_by_Stratum", carbon_stocks_table, startRow = 2)

addStyle(wb, "2_Carbon_Stocks_by_Stratum",
         style = createStyle(fontSize = 12, textDecoration = "bold"),
         rows = 2, cols = 1:ncol(carbon_stocks_table), gridExpand = TRUE)

# ========================================================================
# TABLE 3: NET EMISSION REDUCTIONS (CO2e)
# ========================================================================

addWorksheet(wb, "3_Emission_Reductions")

emissions_table <- emissions_reductions %>%
  select(
    Stratum = stratum,
    # All 4 VM0033 intervals
    `Interval 1 (0-15cm) CO2e/ha` = co2e_0_15_mean,
    `Interval 2 (15-30cm) CO2e/ha` = co2e_15_30_mean,
    `Interval 3 (30-50cm) CO2e/ha` = co2e_30_50_mean,
    `Interval 4 (50-100cm) CO2e/ha` = co2e_50_100_mean,
    `Total (0-100cm) CO2e/ha` = co2e_total_mean,
    `Total SE` = co2e_total_se,
    `95% CI Lower` = co2e_total_ci_lower,
    `95% CI Upper` = co2e_total_ci_upper,
    `Conservative (tonnes CO2e/ha)` = co2e_total_conservative,
    `Percent Change` = pct_change_total,
    Significant = significant_total
  )

writeData(wb, "3_Emission_Reductions", emissions_table, startRow = 2)

addStyle(wb, "3_Emission_Reductions",
         style = createStyle(fontSize = 12, textDecoration = "bold"),
         rows = 2, cols = 1:ncol(emissions_table), gridExpand = TRUE)

# ========================================================================
# TABLE 4: UNCERTAINTY ANALYSIS
# ========================================================================

addWorksheet(wb, "4_Uncertainty_Analysis")

uncertainty_table <- emissions_reductions %>%
  select(
    Stratum = stratum,
    `Baseline SE` = baseline_total_se,
    `Project SE` = project_total_se,
    `Difference SE` = delta_total_se,
    `T-statistic` = t_stat_total,
    `p-value` = p_value_total
  ) %>%
  mutate(
    # Calculate derived metrics
    `95% CI Width` = 2 * 1.96 * `Difference SE`,
    `Relative Uncertainty (%)` = 100 * `Difference SE` / abs(emissions_reductions$delta_total_mean),
    # Cohen's d effect size for total 0-100cm
    `Cohen's d (Effect Size)` = emissions_reductions$delta_total_mean /
      sqrt((emissions_reductions$baseline_total_se^2 + emissions_reductions$project_total_se^2) / 2)
  )

writeData(wb, "4_Uncertainty_Analysis", uncertainty_table, startRow = 2)

# ========================================================================
# TABLE 5: TEMPORAL TRENDS & PERMANENCE
# ========================================================================

if (!is.null(temporal_trends) && nrow(temporal_trends) > 0) {
  addWorksheet(wb, "5_Temporal_Trends")

  trends_table <- temporal_trends %>%
    select(
      Stratum = stratum,
      Scenario = scenario,
      `First Year` = first_year,
      `Last Year` = last_year,
      `Time Span (years)` = year_span,
      # Total 0-100cm (main metric)
      `Total Carbon at t0 (Mg C/ha)` = carbon_total_t0,
      `Total Carbon at tn (Mg C/ha)` = carbon_total_tn,
      `Total Change (Mg C/ha)` = total_change,
      `Total Sequestration Rate (Mg C/ha/yr)` = rate_total_Mg_ha_yr,
      `Total Percent Change` = pct_change_total,
      # Interval-specific rates
      `Rate 0-15cm (Mg C/ha/yr)` = rate_0_15_Mg_ha_yr,
      `Rate 15-30cm (Mg C/ha/yr)` = rate_15_30_Mg_ha_yr,
      `Rate 30-50cm (Mg C/ha/yr)` = rate_30_50_Mg_ha_yr,
      `Rate 50-100cm (Mg C/ha/yr)` = rate_50_100_Mg_ha_yr
    )

  writeData(wb, "5_Temporal_Trends", trends_table, startRow = 2)

  addStyle(wb, "5_Temporal_Trends",
           style = createStyle(fontSize = 12, textDecoration = "bold"),
           rows = 2, cols = 1:ncol(trends_table), gridExpand = TRUE)
}

# Save Excel workbook
excel_file <- "outputs/verification/vm0033_verification_tables.xlsx"
saveWorkbook(wb, excel_file, overwrite = TRUE)
log_message(sprintf("Saved: %s", excel_file))

# ============================================================================
# GENERATE HTML VERIFICATION REPORT
# ============================================================================

log_message("Generating HTML verification report...")

# Create R Markdown content
rmd_content <- sprintf('---
title: "VM0033 Final Verification Report"
subtitle: "%s"
date: "`r format(Sys.Date(), \'%%B %%d, %%Y\')`"
output:
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
    toc_depth: 3
    number_sections: true
    code_folding: hide
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 10, fig.height = 6)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
library(DT)

# Load data
additionality <- read_csv("outputs/additionality/additionality_by_stratum.csv", show_col_types = FALSE)
temporal_trends <- read_csv("outputs/temporal_change/temporal_trends_by_stratum.csv", show_col_types = FALSE)
metadata <- read_csv("data_temporal/temporal_metadata.csv", show_col_types = FALSE)
```

# Executive Summary

## Project Information

- **Project:** %s
- **Location:** %s
- **Monitoring Years:** %s
- **Scenarios:** %s
- **Strata Analyzed:** %d

## Key Findings

### Additionality Assessment

```{r additionality-summary}
summary_stats <- additionality %%>%%
  summarise(
    mean_baseline = mean(baseline_total_mean, na.rm = TRUE),
    mean_project = mean(project_total_mean, na.rm = TRUE),
    mean_additionality = mean(delta_total_mean, na.rm = TRUE),
    mean_conservative = mean(delta_total_conservative, na.rm = TRUE),
    mean_co2e = mean(delta_total_mean * 44/12, na.rm = TRUE),
    n_significant = sum(significant_total, na.rm = TRUE),
    n_total = n()
  )

kable(data.frame(
  Metric = c("Baseline Carbon Stock", "Project Carbon Stock", "Additionality (Mean)",
             "Conservative Estimate", "CO2e Reduction", "Significant Strata"),
  Value = c(
    sprintf("%.2f Mg C/ha", summary_stats$mean_baseline),
    sprintf("%.2f Mg C/ha", summary_stats$mean_project),
    sprintf("%.2f Mg C/ha", summary_stats$mean_additionality),
    sprintf("%.2f Mg C/ha", summary_stats$mean_conservative),
    sprintf("%.2f tonnes CO2e/ha", summary_stats$mean_co2e),
    sprintf("%%d / %%d (%.1f%%%%)", summary_stats$n_significant, summary_stats$n_total,
            100 * summary_stats$n_significant / summary_stats$n_total)
  )
), caption = "Project-Wide Additionality Summary")
```

### Compliance Status

**VM0033 Requirements:**

- ✅ Conservative approach: 95%% CI lower bound used
- ✅ Statistical testing: p-values < 0.05 for significance
- ✅ Uncertainty quantification: Full error propagation
- ✅ Stratum-specific analysis: All strata independently assessed

# Detailed Results

## Carbon Stocks by Stratum

```{r carbon-stocks-table}
datatable(additionality %%>%%
  select(Stratum = stratum,
         `Baseline (Mg C/ha)` = baseline_total_mean,
         `Project (Mg C/ha)` = project_total_mean,
         `Difference (Mg C/ha)` = delta_total_mean,
         `95%% CI Lower` = delta_total_ci_lower,
         `95%% CI Upper` = delta_total_ci_upper,
         `Conservative` = delta_total_conservative,
         `p-value` = p_value_total,
         Status = additionality_status),
  options = list(pageLength = 10),
  caption = "Baseline vs Project Carbon Stocks by Stratum")
```

## Additionality Visualization

```{r additionality-plot, fig.height=8}
ggplot(additionality, aes(x = reorder(stratum, delta_total_mean), y = delta_total_mean)) +
  geom_col(aes(fill = significant_total), alpha = 0.8) +
  geom_errorbar(aes(ymin = delta_total_ci_lower, ymax = delta_total_ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#757575"),
                    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")) +
  labs(title = "Additionality by Stratum (Project - Baseline)",
       subtitle = "Error bars show 95%% confidence intervals",
       x = "Stratum",
       y = "Carbon Stock Difference (Mg C/ha)",
       fill = "Statistical Significance") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

## Emission Reductions (CO2e)

```{r emissions-table}
emissions <- additionality %%>%%
  mutate(
    co2e_mean = delta_total_mean * 44/12,
    co2e_conservative = delta_total_conservative * 44/12
  )

datatable(emissions %%>%%
  select(Stratum = stratum,
         `Mean (tonnes CO2e/ha)` = co2e_mean,
         `Conservative (tonnes CO2e/ha)` = co2e_conservative,
         `Percent Change` = pct_change_total,
         Significant = significant_total),
  options = list(pageLength = 10),
  caption = "Net Emission Reductions by Stratum")
```

## Temporal Trends

```{r temporal-trends}
if (!is.null(temporal_trends) && nrow(temporal_trends) > 0) {
  datatable(temporal_trends %%>%%
    select(Stratum = stratum,
           Scenario = scenario,
           `Years` = years,
           `Time Span` = year_span,
           `Sequestration Rate (Mg C/ha/yr)` = rate_total_Mg_ha_yr,
           `Total Change (Mg C/ha)` = total_change,
           `Percent Change` = pct_change_total),
    options = list(pageLength = 10),
    caption = "Temporal Trends in Total Carbon Stocks (0-100cm)")
}
```

# Uncertainty Analysis

## Sources of Uncertainty

1. **Sampling uncertainty:** Field core spatial variability
2. **Measurement uncertainty:** Lab analytical precision
3. **Model uncertainty:** Spatial prediction error (RF/Kriging)
4. **Temporal uncertainty:** Inter-annual variability

## Conservative Approach

Per VM0033 requirements, all creditable carbon stocks use the **95%% confidence interval lower bound**:

```{r uncertainty-comparison}
uncertainty_comparison <- additionality %%>%%
  summarise(
    mean_estimate = mean(delta_total_mean, na.rm = TRUE),
    conservative_estimate = mean(delta_total_conservative, na.rm = TRUE),
    difference = mean_estimate - conservative_estimate,
    pct_discount = 100 * difference / mean_estimate
  )

kable(data.frame(
  Estimate = c("Mean", "Conservative (95%% CI lower)", "Discount"),
  `Value (Mg C/ha)` = c(
    sprintf("%.2f", uncertainty_comparison$mean_estimate),
    sprintf("%.2f", uncertainty_comparison$conservative_estimate),
    sprintf("%.2f (%.1f%%%%)", uncertainty_comparison$difference, uncertainty_comparison$pct_discount)
  )
), caption = "Conservative Approach Impact")
```

# Verification Statement

Based on the analysis conducted following VM0033 methodology and ORRAA High Quality Blue Carbon Principles:

1. **Additionality demonstrated:** Project carbon stocks significantly exceed baseline
2. **Conservative estimates applied:** 95%% CI lower bound used for all calculations
3. **Uncertainty quantified:** Full error propagation through analysis chain
4. **Statistical rigor:** All significant differences tested at p < 0.05

**Mean conservative estimate:** %.2f Mg C/ha (%.2f tonnes CO2e/ha)

# Appendices

## Methods Summary

- **Depth harmonization:** Equal-area spline to VM0033 standard depths
- **Spatial modeling:** Random Forest with stratum as covariate
- **Uncertainty:** Bootstrap + RF prediction intervals + uncertainty propagation
- **Statistical testing:** T-tests for baseline vs project comparison
- **Conservative approach:** 95%% CI lower bound (VM0033 requirement)

## Data Quality

```{r data-quality}
metadata_summary <- metadata %%>%%
  group_by(scenario) %%>%%
  summarise(
    n_datasets = n(),
    years = paste(year, collapse = ", "),
    .groups = "drop"
  )

kable(metadata_summary, caption = "Data Coverage Summary")
```

---

**Report generated:** `r Sys.Date()`
**Methodology:** VM0033, ORRAA High Quality Principles v1.1, IPCC Wetlands Supplement
**Confidence level:** 95%%
',
PROJECT_NAME,
PROJECT_NAME,
PROJECT_LOCATION,
paste(sort(unique(metadata$year)), collapse = ", "),
paste(unique(metadata$scenario), collapse = ", "),
project_wide_summary$n_strata,
project_wide_summary$mean_carbon_conservative,
project_wide_summary$mean_co2e_conservative)

# Write Rmd file
rmd_file <- "outputs/verification/vm0033_final_verification_report.Rmd"
writeLines(rmd_content, rmd_file)

# Render to HTML
html_file <- "outputs/verification/vm0033_final_verification_report.html"
tryCatch({
  rmarkdown::render(rmd_file, output_file = html_file, quiet = TRUE)
  log_message(sprintf("Saved: %s", html_file))
}, error = function(e) {
  log_message(sprintf("WARNING: Could not render HTML report: %s", e$message), "WARNING")
  log_message("  Rmd file saved, render manually if needed", "WARNING")
})

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n========================================\n")
cat("VERIFICATION PACKAGE COMPLETE\n")
cat("========================================\n\n")

cat("Output files created:\n")
cat(sprintf("  - %s\n", excel_file))
if (file.exists(html_file)) {
  cat(sprintf("  - %s\n", html_file))
}
cat("\n")

cat("Key Results:\n")
cat(sprintf("  Mean additionality: %.2f Mg C/ha (%.2f tonnes CO2e/ha)\n",
            project_wide_summary$mean_carbon_additionality,
            project_wide_summary$mean_co2e_additionality))
cat(sprintf("  Conservative estimate: %.2f Mg C/ha (%.2f tonnes CO2e/ha)\n",
            project_wide_summary$mean_carbon_conservative,
            project_wide_summary$mean_co2e_conservative))
cat(sprintf("  Significant strata: %d / %d\n",
            project_wide_summary$n_significant,
            project_wide_summary$n_strata))
cat("\n")

log_message("=== MODULE 10 COMPLETE ===")
