# ============================================================================
# MODULE 06: DUAL-ENGINE CARBON STOCK MODELING
# ============================================================================
# PURPOSE: Implement two complementary modeling paths for carbon stock estimation
#
# PATH A: CREDITING (Linear Mixed Models)
#   - Uses lme4 to calculate Stock ± Uncertainty per Stratum
#   - Accounts for site-level clustering (random effects)
#   - Generates marginal means with emmeans for credit certification
#
# PATH B: MAPPING (Random Forest with Interactions)
#   - Spatial predictions using terrestrial covariates
#   - Includes management history and environmental interactions
#   - Generates wall-to-wall carbon stock maps
#
# INPUTS:
#   - data_processed/cores_harmonized_bluecarbon.rds (from Module 03)
#   - covariates/*.tif (GEE-derived rasters)
#   - data_raw/management_history.csv (optional)
#
# OUTPUTS:
#   - data_processed/carbon_stocks_by_stratum.csv (LMM results)
#   - data_processed/lmm_marginal_means.csv (emmeans output)
#   - data_processed/rf_model_fit.rds (RF model object)
#   - outputs/plots/lmm_diagnostics.png
#   - outputs/plots/rf_variable_importance.png
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

# Create output directories
required_dirs <- c("data_processed", "outputs/plots", "logs", "diagnostics")
for (dir in required_dirs) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# Initialize logging
log_file <- file.path("logs", paste0("carbon_stock_modeling_", Sys.Date(), ".log"))

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 06: DUAL-ENGINE CARBON STOCK MODELING ===")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(lme4)
  library(emmeans)
})

# Check for randomForest (optional for PATH B)
rf_available <- requireNamespace("randomForest", quietly = TRUE)
if (!rf_available) {
  log_message("randomForest package not installed - PATH B (Mapping) will be skipped", "WARNING")
  log_message("Install with: install.packages('randomForest')", "INFO")
}

log_message("Packages loaded successfully")

# ============================================================================
# LOAD DATA
# ============================================================================

log_message("Loading harmonized core data...")

if (!file.exists("data_processed/cores_harmonized_bluecarbon.rds")) {
  stop("Harmonized data not found. Run Module 03 (Depth Harmonization) first.")
}

harmonized_cores <- readRDS("data_processed/cores_harmonized_bluecarbon.rds")
log_message(sprintf("Loaded %d samples from %d cores",
                    nrow(harmonized_cores), n_distinct(harmonized_cores$core_id)))

# Aggregate to total carbon stock per core
core_totals <- harmonized_cores %>%
  group_by(core_id, stratum) %>%
  summarise(
    total_carbon_stock = sum(carbon_stock_kg_m2, na.rm = TRUE),
    n_layers = n(),
    max_depth = max(depth_cm_midpoint),
    mean_soc = mean(soc_harmonized, na.rm = TRUE),
    mean_bd = mean(bd_harmonized, na.rm = TRUE),
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop"
  )

log_message(sprintf("Aggregated to %d core totals", nrow(core_totals)))

# Check for ecosystem_type column (from new two-tier stratification)
if ("ecosystem_type" %in% names(harmonized_cores)) {
  ecosystem_mapping <- harmonized_cores %>%
    distinct(core_id, ecosystem_type)
  core_totals <- core_totals %>%
    left_join(ecosystem_mapping, by = "core_id")
  log_message("Ecosystem type detected in data - using for stratified analysis")
}

# Extract site_id from core_id if not present
if (!"site_id" %in% names(core_totals)) {
  core_totals <- core_totals %>%
    mutate(site_id = gsub("_[0-9]+$", "", core_id))
  log_message("Inferred site_id from core_id")
}

# ============================================================================
# LOAD MANAGEMENT HISTORY (if available)
# ============================================================================

management_cols_available <- FALSE

if (file.exists("data_raw/management_history.csv")) {
  log_message("Loading management history for modeling...")

  mg_hist <- read_csv("data_raw/management_history.csv", show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      years_since_disturbance = as.numeric(format(Sys.Date(), "%Y")) - disturbance_year,
      years_since_restoration = as.numeric(format(Sys.Date(), "%Y")) - restoration_year
    ) %>%
    select(site_id, years_since_disturbance, years_since_restoration,
           any_of(c("burn_frequency_yr", "disturbance_type", "management_type")))

  core_totals <- core_totals %>%
    left_join(mg_hist, by = "site_id")

  management_cols_available <- TRUE
  log_message("Management history joined to core data")
} else {
  log_message("No management history file found - using stratum-only models", "INFO")

  # Add placeholder columns
  core_totals <- core_totals %>%
    mutate(
      years_since_disturbance = NA_real_,
      years_since_restoration = NA_real_,
      burn_frequency_yr = NA_real_
    )
}

# ============================================================================
# PATH A: CREDITING (Linear Mixed Models)
# ============================================================================

cat("\n========================================\n")
cat("PATH A: CREDITING (Linear Mixed Models)\n")
cat("========================================\n\n")

log_message("Fitting Linear Mixed Model for carbon crediting...")

# Prepare data for LMM
data_clean <- core_totals %>%
  filter(!is.na(total_carbon_stock) & !is.na(stratum)) %>%
  mutate(
    stratum = as.factor(stratum),
    site_id = as.factor(site_id)
  )

n_strata <- n_distinct(data_clean$stratum)
n_sites <- n_distinct(data_clean$site_id)

log_message(sprintf("Data for LMM: %d cores, %d strata, %d sites",
                    nrow(data_clean), n_strata, n_sites))

# Check if we have enough sites for random effects
if (n_sites < 3) {
  log_message("Fewer than 3 sites - using fixed effects model instead", "WARNING")

  # Fixed effects model (no random effects)
  lmm_model <- lm(total_carbon_stock ~ stratum, data = data_clean)
  model_type <- "fixed"

} else {
  # Fit LMM with site as random effect
  lmm_model <- tryCatch({
    lmer(total_carbon_stock ~ stratum + (1 | site_id), data = data_clean)
  }, error = function(e) {
    log_message(sprintf("LMM failed: %s - falling back to fixed effects", e$message), "WARNING")
    lm(total_carbon_stock ~ stratum, data = data_clean)
  })

  model_type <- if (inherits(lmm_model, "lmerMod")) "mixed" else "fixed"
}

log_message(sprintf("Fitted %s effects model", model_type))

# Model summary
cat("\nModel Summary:\n")
cat("----------------------------------------\n")
print(summary(lmm_model))

# ============================================================================
# Extract Marginal Means with emmeans
# ============================================================================

log_message("Calculating marginal means with emmeans...")

emm_out <- emmeans(lmm_model, ~ stratum)
emm_df <- as.data.frame(emm_out)

# Rename columns for clarity
emm_df <- emm_df %>%
  rename(
    mean_stock_kg_m2 = emmean,
    se_kg_m2 = SE,
    ci_lower_kg_m2 = lower.CL,
    ci_upper_kg_m2 = upper.CL
  ) %>%
  mutate(
    # Convert to Mg/ha for VM0033 reporting
    mean_stock_Mg_ha = mean_stock_kg_m2 * 10,
    se_Mg_ha = se_kg_m2 * 10,
    ci_lower_Mg_ha = ci_lower_kg_m2 * 10,
    ci_upper_Mg_ha = ci_upper_kg_m2 * 10,
    # Calculate relative uncertainty (%)
    relative_uncertainty_pct = (se_kg_m2 / mean_stock_kg_m2) * 100
  )

cat("\n========================================\n")
cat("MARGINAL MEANS BY STRATUM (emmeans)\n")
cat("========================================\n\n")

cat("Carbon Stock Estimates (kg/m² and Mg/ha):\n")
cat("----------------------------------------\n")

for (i in 1:nrow(emm_df)) {
  cat(sprintf("\n%s:\n", emm_df$stratum[i]))
  cat(sprintf("  Mean Stock: %.2f ± %.2f kg/m² (%.1f ± %.1f Mg/ha)\n",
              emm_df$mean_stock_kg_m2[i], emm_df$se_kg_m2[i],
              emm_df$mean_stock_Mg_ha[i], emm_df$se_Mg_ha[i]))
  cat(sprintf("  95%% CI: [%.2f, %.2f] kg/m² ([%.1f, %.1f] Mg/ha)\n",
              emm_df$ci_lower_kg_m2[i], emm_df$ci_upper_kg_m2[i],
              emm_df$ci_lower_Mg_ha[i], emm_df$ci_upper_Mg_ha[i]))
  cat(sprintf("  Relative Uncertainty: %.1f%%\n", emm_df$relative_uncertainty_pct[i]))
}

# Pairwise contrasts (for additionality testing)
log_message("Calculating pairwise contrasts...")
contrasts <- pairs(emm_out)
contrasts_df <- as.data.frame(contrasts)

cat("\n\nPairwise Contrasts (for Additionality):\n")
cat("----------------------------------------\n")
print(contrasts_df)

# Save LMM results
write_csv(emm_df, "data_processed/lmm_marginal_means.csv")
write_csv(contrasts_df, "data_processed/lmm_pairwise_contrasts.csv")
saveRDS(lmm_model, "data_processed/lmm_model.rds")

log_message("Saved LMM results and model object")

# ============================================================================
# LMM Diagnostics Plot
# ============================================================================

log_message("Generating LMM diagnostics plot...")

# Residual plot
if (model_type == "mixed") {
  resid_df <- data.frame(
    fitted = fitted(lmm_model),
    residuals = residuals(lmm_model),
    stratum = data_clean$stratum
  )
} else {
  resid_df <- data.frame(
    fitted = fitted(lmm_model),
    residuals = residuals(lmm_model),
    stratum = data_clean$stratum
  )
}

p_diag <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(aes(color = stratum), alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.5) +
  labs(
    title = "LMM Residual Diagnostics",
    subtitle = paste("Model type:", model_type, "effects"),
    x = "Fitted Values (kg/m²)",
    y = "Residuals (kg/m²)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("outputs/plots/lmm_diagnostics.png", p_diag, width = 10, height = 6, dpi = 300)
log_message("Saved LMM diagnostics plot")

# ============================================================================
# PATH B: MAPPING (Random Forest with Interactions)
# ============================================================================

if (rf_available) {
  library(randomForest)

  cat("\n========================================\n")
  cat("PATH B: MAPPING (Random Forest)\n")
  cat("========================================\n\n")

  log_message("Preparing Random Forest model for spatial mapping...")

  # Define predictor set for terrestrial ecosystems
  rf_predictors <- c(
    # Management history predictors
    "years_since_disturbance",
    "years_since_restoration",
    "burn_frequency_yr",
    # Stratum
    "stratum"
  )

  # Add spatial covariates if available
  spatial_predictors <- c(
    "NDVI_amplitude",      # Phenology
    "TWI",                 # Topographic Wetness Index
    "NDVI_mean",           # Vegetation index
    "elevation",           # Topography
    "slope"                # Terrain
  )

  # Check which predictors are available
  available_predictors <- intersect(rf_predictors, names(data_clean))
  log_message(sprintf("Available predictors: %s", paste(available_predictors, collapse = ", ")))

  # Load covariates if available (from GEE exports)
  covariate_dir <- "covariates"
  if (dir.exists(covariate_dir)) {
    # Try to extract covariate values at core locations
    covariate_files <- list.files(covariate_dir, pattern = "\\.tif$", full.names = TRUE)
    if (length(covariate_files) > 0) {
      log_message(sprintf("Found %d covariate rasters", length(covariate_files)))
      # Note: Actual raster extraction would require terra/raster package
      # This is a placeholder for the spatial extraction workflow
    }
  }

  # Prepare RF training data (remove NAs)
  rf_data <- data_clean %>%
    select(total_carbon_stock, any_of(available_predictors)) %>%
    mutate(across(where(is.character), as.factor))

  # Remove rows with NA in predictors
  complete_cases <- complete.cases(rf_data[, -1])  # Exclude response
  rf_data_complete <- rf_data[complete_cases, ]

  if (nrow(rf_data_complete) < 10) {
    log_message("Insufficient complete cases for RF - need at least 10", "WARNING")
    log_message("Fitting RF with stratum only as predictor", "INFO")

    rf_data_complete <- data_clean %>%
      select(total_carbon_stock, stratum) %>%
      filter(!is.na(total_carbon_stock))
  }

  log_message(sprintf("RF training data: %d samples, %d predictors",
                      nrow(rf_data_complete), ncol(rf_data_complete) - 1))

  # Fit Random Forest
  set.seed(42)  # Reproducibility

  rf_model <- randomForest(
    total_carbon_stock ~ .,
    data = rf_data_complete,
    ntree = 500,
    mtry = max(1, floor(sqrt(ncol(rf_data_complete) - 1))),
    importance = TRUE,
    na.action = na.omit
  )

  log_message("Random Forest model fitted")

  # Model performance
  cat("\nRandom Forest Model Summary:\n")
  cat("----------------------------------------\n")
  print(rf_model)

  cat(sprintf("\nOut-of-bag R²: %.3f\n", 1 - rf_model$mse[length(rf_model$mse)] /
                var(rf_data_complete$total_carbon_stock)))
  cat(sprintf("Out-of-bag RMSE: %.3f kg/m²\n",
              sqrt(rf_model$mse[length(rf_model$mse)])))

  # Variable importance
  importance_df <- as.data.frame(importance(rf_model)) %>%
    tibble::rownames_to_column("variable") %>%
    arrange(desc(`%IncMSE`))

  cat("\nVariable Importance:\n")
  cat("----------------------------------------\n")
  print(importance_df)

  # Save RF model
  saveRDS(rf_model, "data_processed/rf_model_fit.rds")
  write_csv(importance_df, "data_processed/rf_variable_importance.csv")

  # Variable importance plot
  p_imp <- ggplot(importance_df, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Random Forest Variable Importance",
      subtitle = "Terrestrial Carbon Stock Prediction",
      x = "Predictor",
      y = "% Increase in MSE when Permuted"
    ) +
    theme_bw()

  ggsave("outputs/plots/rf_variable_importance.png", p_imp, width = 8, height = 6, dpi = 300)
  log_message("Saved RF variable importance plot")

  # ============================================================================
  # Create Interaction Terms for Spatial Model
  # ============================================================================

  cat("\n\nInteraction Terms for Spatial Mapping:\n")
  cat("----------------------------------------\n")
  cat("The following interaction terms are recommended for spatial prediction:\n\n")

  cat("rf_predictors <- c(\n")
  cat("  'years_since_disturbance',\n")
  cat("  'years_since_restoration',\n")
  cat("  'burn_frequency_yr',\n")
  cat("  'NDVI_amplitude',\n")
  cat("  'TWI',                      # Topographic Wetness Index\n")
  cat("  'NDVI_x_TWI'                # Interaction: Vegetation × Wetness\n")
  cat(")\n\n")

  cat("Note: Create interaction rasters in GEE or R before prediction:\n")
  cat("  NDVI_x_TWI = NDVI_amplitude * TWI\n\n")

} else {
  log_message("PATH B (Random Forest) skipped - package not available", "INFO")
}

# ============================================================================
# COMBINED SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("MODULE 06 COMPLETE: DUAL-ENGINE SUMMARY\n")
cat("========================================\n\n")

cat("PATH A: Crediting (LMM)\n")
cat("----------------------------------------\n")
cat(sprintf("  Model type: %s effects\n", model_type))
cat(sprintf("  Strata modeled: %d\n", n_strata))
cat(sprintf("  Sites (clusters): %d\n", n_sites))
cat("  Output: data_processed/lmm_marginal_means.csv\n\n")

cat("PATH B: Mapping (RF)\n")
cat("----------------------------------------\n")
if (rf_available) {
  cat("  Model: Random Forest (500 trees)\n")
  cat(sprintf("  Training samples: %d\n", nrow(rf_data_complete)))
  cat("  Output: data_processed/rf_model_fit.rds\n")
} else {
  cat("  Status: SKIPPED (randomForest package not installed)\n")
}

cat("\n\nRecommendations:\n")
cat("----------------------------------------\n")
cat("1. Use PATH A (LMM) results for carbon crediting and uncertainty reporting\n")
cat("2. Use PATH B (RF) for wall-to-wall spatial mapping\n")
cat("3. Compare stratum means between models for validation\n")
cat("4. For terrestrial ecosystems, include management history predictors\n\n")

cat("Next steps:\n")
cat("  1. Review lmm_marginal_means.csv for credit estimation\n")
cat("  2. Use rf_model_fit.rds for raster prediction (Module 05)\n")
cat("  3. Run: source('P2_07_mmrv_reporting_bluecarbon.R')\n\n")

log_message("=== MODULE 06 COMPLETE ===")
