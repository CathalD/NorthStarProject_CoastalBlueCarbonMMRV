# ============================================================================
# MODULE 08A: SCENARIO BUILDER FOR TEMPORAL ANALYSIS
# ============================================================================
# PURPOSE: Generate synthetic carbon stock scenarios using reference trajectories
#          and Canadian blue carbon literature for VM0033 additionality analysis
#
# INPUTS:
#   - outputs/carbon_stocks/SCENARIO_YEAR/*.csv (available field-based scenarios)
#   - scenario_modeling_config.csv (defines scenarios to generate)
#   - canadian_bluecarbon_parameters.csv (literature database)
#   - blue_carbon_config.R (configuration)
#
# OUTPUTS:
#   - outputs/carbon_stocks/MODELED_SCENARIO_YEAR/carbon_stocks_by_stratum_rf.csv
#   - outputs/carbon_stocks/MODELED_SCENARIO_YEAR/carbon_stocks_overall_rf.csv
#   - outputs/carbon_stocks/MODELED_SCENARIO_YEAR/carbon_stocks_conservative_vm0033_rf.csv
#   - outputs/scenario_modeling/modeling_report.csv
#
# METHODS:
#   1. copy_scenario: Copy existing scenario to new name
#   2. reference_trajectory: Model recovery from degraded to reference condition
#   3. literature_lookup: Use Canadian literature values directly
#
# AUTHOR: Blue Carbon Workflow
# DATE: 2024
# ============================================================================

# Clear workspace
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(terra)
})

# Load configuration
source("blue_carbon_config.R")

# ============================================================================
# SETUP LOGGING
# ============================================================================

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cat(sprintf("%s %s: %s\n", timestamp, level, msg))
}

log_message("=== MODULE 08A: SCENARIO BUILDER ===")
log_message(sprintf("Project: %s", PROJECT_NAME))
log_message(sprintf("Scenario modeling enabled: %s", SCENARIO_MODELING_ENABLED))

# Check if scenario modeling is enabled
if (!SCENARIO_MODELING_ENABLED) {
  log_message("Scenario modeling is disabled in config. Exiting.", "WARNING")
  quit(save = "no")
}

# ============================================================================
# LOAD CANADIAN LITERATURE DATABASE
# ============================================================================

log_message("\nLoading Canadian blue carbon literature database...")

if (!file.exists(CANADIAN_LITERATURE_DB)) {
  stop(sprintf("Literature database not found: %s\nPlease create this file with BC Coast parameters.",
               CANADIAN_LITERATURE_DB))
}

lit_db <- read.csv(CANADIAN_LITERATURE_DB, stringsAsFactors = FALSE)
log_message(sprintf("Loaded %d literature records for BC Coast ecosystems", nrow(lit_db)))

# Display available ecosystems and scenario types
ecosystems_available <- unique(lit_db$ecosystem)
scenarios_available <- unique(lit_db$scenario_type)
log_message(sprintf("  Ecosystems: %s", paste(ecosystems_available, collapse = ", ")))
log_message(sprintf("  Scenario types: %d unique types", length(scenarios_available)))

# ============================================================================
# LOAD SCENARIO MODELING CONFIGURATION
# ============================================================================

log_message("\nLoading scenario modeling configuration...")

if (!file.exists(SCENARIO_CONFIG_FILE)) {
  log_message(sprintf("Configuration file not found: %s", SCENARIO_CONFIG_FILE), "WARNING")
  log_message("Looking for EXAMPLE file...", "INFO")

  example_file <- "scenario_modeling_config_EXAMPLE.csv"
  if (file.exists(example_file)) {
    log_message(sprintf("Using example file: %s", example_file), "WARNING")
    log_message("Please copy this to 'scenario_modeling_config.csv' and customize", "WARNING")
    SCENARIO_CONFIG_FILE <- example_file
  } else {
    stop("No scenario configuration file found. Please create scenario_modeling_config.csv")
  }
}

scenario_config <- read.csv(SCENARIO_CONFIG_FILE, stringsAsFactors = FALSE)

# Filter to scenarios to include
scenario_config <- scenario_config %>%
  filter(include_in_analysis == TRUE | include_in_analysis == "TRUE")

log_message(sprintf("Loaded %d scenarios to model", nrow(scenario_config)))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Function to load existing scenario data
load_scenario_data <- function(scenario_name, year = MONITORING_YEAR) {

  scenario_dir <- sprintf("outputs/carbon_stocks/%s_%s", scenario_name, year)

  if (!dir.exists(scenario_dir)) {
    # Try without year suffix
    scenario_dir <- sprintf("outputs/carbon_stocks/%s", scenario_name)
    if (!dir.exists(scenario_dir)) {
      return(NULL)
    }
  }

  # Load the three key files
  by_stratum_file <- file.path(scenario_dir, "carbon_stocks_by_stratum_rf.csv")
  overall_file <- file.path(scenario_dir, "carbon_stocks_overall_rf.csv")
  vm0033_file <- file.path(scenario_dir, "carbon_stocks_conservative_vm0033_rf.csv")

  result <- list()

  if (file.exists(by_stratum_file)) {
    result$by_stratum <- read.csv(by_stratum_file)
  }

  if (file.exists(overall_file)) {
    result$overall <- read.csv(overall_file)
  }

  if (file.exists(vm0033_file)) {
    result$vm0033 <- read.csv(vm0033_file)
  }

  if (length(result) == 0) {
    return(NULL)
  }

  return(result)
}

# Function to save modeled scenario in Module 06 format
save_modeled_scenario <- function(scenario_data, scenario_name, year = MONITORING_YEAR) {

  output_dir <- sprintf("outputs/carbon_stocks/%s_%s", scenario_name, year)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Save by_stratum file
  if (!is.null(scenario_data$by_stratum)) {
    write.csv(scenario_data$by_stratum,
              file.path(output_dir, "carbon_stocks_by_stratum_rf.csv"),
              row.names = FALSE)
  }

  # Save overall file
  if (!is.null(scenario_data$overall)) {
    write.csv(scenario_data$overall,
              file.path(output_dir, "carbon_stocks_overall_rf.csv"),
              row.names = FALSE)
  }

  # Save VM0033 file
  if (!is.null(scenario_data$vm0033)) {
    write.csv(scenario_data$vm0033,
              file.path(output_dir, "carbon_stocks_conservative_vm0033_rf.csv"),
              row.names = FALSE)
  }

  log_message(sprintf("  Saved to: %s", output_dir))
}

# Function: Reference Trajectory Model
# Models recovery from degraded to reference condition over time
model_reference_trajectory <- function(degraded_data, reference_data, years,
                                      ecosystem, recovery_model = "exponential") {

  log_message(sprintf("    Using %s recovery model", recovery_model))

  # Get recovery rate constant from literature
  lit_params <- lit_db %>%
    filter(ecosystem == !!ecosystem,
           region == "BC_coast",
           !is.na(recovery_constant_k)) %>%
    slice(1)

  if (nrow(lit_params) > 0) {
    k <- lit_params$recovery_constant_k
    log_message(sprintf("    Recovery constant k = %.3f (from literature)", k))
  } else {
    # Default values if not in literature
    k <- ifelse(ecosystem == "salt_marsh", 0.15,
                ifelse(ecosystem == "seagrass", 0.12, 0.10))
    log_message(sprintf("    Recovery constant k = %.3f (default for %s)", k, ecosystem), "WARNING")
  }

  # Function to calculate trajectory
  calculate_trajectory <- function(degraded_val, reference_val, t, k_val, model_type) {

    delta <- reference_val - degraded_val

    if (model_type == "exponential") {
      # Exponential: fast initial recovery, asymptotic approach
      recovery_pct <- 1 - exp(-k_val * t)

    } else if (model_type == "linear") {
      # Linear: constant rate
      max_years <- 20  # Assume 20 years to reach reference
      recovery_pct <- min(1.0, t / max_years)

    } else if (model_type == "logistic") {
      # Logistic: S-shaped curve
      midpoint <- 10  # Inflection at 10 years
      recovery_pct <- 1 / (1 + exp(-k_val * (t - midpoint)))

    } else {
      # Default to exponential
      recovery_pct <- 1 - exp(-k_val * t)
    }

    return(degraded_val + delta * recovery_pct)
  }

  # Model by_stratum data
  modeled_by_stratum <- degraded_data$by_stratum %>%
    left_join(reference_data$by_stratum %>%
                select(stratum, depth_interval,
                       ref_mean = mean_stock_Mg_ha,
                       ref_se = se_stock_Mg_ha),
              by = c("stratum", "depth_interval")) %>%
    mutate(
      # Calculate modeled stocks
      mean_stock_Mg_ha = mapply(calculate_trajectory,
                                mean_stock_Mg_ha, ref_mean,
                                years, k, recovery_model),

      # Propagate uncertainty (combine degraded, reference, and model uncertainty)
      model_uncertainty = mean_stock_Mg_ha * (MODELING_UNCERTAINTY_BUFFER / 100),
      se_stock_Mg_ha = sqrt(se_stock_Mg_ha^2 + ref_se^2 + model_uncertainty^2),

      # Recalculate other fields
      conservative_stock_Mg_ha = pmax(0, mean_stock_Mg_ha - qnorm((1 + CONFIDENCE_LEVEL) / 2) * se_stock_Mg_ha),
      total_stock_Mg = mean_stock_Mg_ha * area_ha,
      conservative_total_Mg = conservative_stock_Mg_ha * area_ha
    ) %>%
    select(-ref_mean, -ref_se, -model_uncertainty)

  # Model overall data
  modeled_overall <- degraded_data$overall %>%
    left_join(reference_data$overall %>%
                select(depth_interval,
                       ref_mean = mean_stock_Mg_ha,
                       ref_se = se_stock_Mg_ha),
              by = "depth_interval") %>%
    mutate(
      mean_stock_Mg_ha = mapply(calculate_trajectory,
                                mean_stock_Mg_ha, ref_mean,
                                years, k, recovery_model),
      model_uncertainty = mean_stock_Mg_ha * (MODELING_UNCERTAINTY_BUFFER / 100),
      se_stock_Mg_ha = sqrt(se_stock_Mg_ha^2 + ref_se^2 + model_uncertainty^2),
      conservative_stock_Mg_ha = pmax(0, mean_stock_Mg_ha - qnorm((1 + CONFIDENCE_LEVEL) / 2) * se_stock_Mg_ha)
    ) %>%
    select(-ref_mean, -ref_se, -model_uncertainty)

  # Model VM0033 data
  modeled_vm0033 <- degraded_data$vm0033 %>%
    left_join(reference_data$vm0033 %>%
                select(stratum,
                       ref_mean = mean_stock_0_100_Mg_ha,
                       ref_se = conservative_stock_0_100_Mg_ha),
              by = "stratum") %>%
    mutate(
      mean_stock_0_100_Mg_ha = mapply(calculate_trajectory,
                                      mean_stock_0_100_Mg_ha, ref_mean,
                                      years, k, recovery_model),
      model_uncertainty = mean_stock_0_100_Mg_ha * (MODELING_UNCERTAINTY_BUFFER / 100),
      se_modeled = sqrt(model_uncertainty^2),
      conservative_stock_0_100_Mg_ha = pmax(0, mean_stock_0_100_Mg_ha -
                                            qnorm((1 + CONFIDENCE_LEVEL) / 2) * se_modeled),
      total_stock_0_100_Mg = mean_stock_0_100_Mg_ha * area_ha,
      conservative_total_0_100_Mg = conservative_stock_0_100_Mg_ha * area_ha
    ) %>%
    select(-ref_mean, -ref_se, -model_uncertainty, -se_modeled)

  return(list(
    by_stratum = modeled_by_stratum,
    overall = modeled_overall,
    vm0033 = modeled_vm0033
  ))
}

# Function: Literature Lookup
# Use values directly from Canadian literature database
model_from_literature <- function(ecosystem, scenario_type, area_ha = NULL) {

  log_message(sprintf("    Looking up: %s, %s", ecosystem, scenario_type))

  lit_record <- lit_db %>%
    filter(ecosystem == !!ecosystem,
           scenario_type == !!scenario_type,
           region == "BC_coast") %>%
    slice(1)

  if (nrow(lit_record) == 0) {
    stop(sprintf("No literature record found for ecosystem=%s, scenario_type=%s",
                 ecosystem, scenario_type))
  }

  mean_stock <- lit_record$carbon_stock_0_100_mean_Mg_ha
  se_stock <- lit_record$carbon_stock_0_100_se

  # If no area provided, use a default
  if (is.null(area_ha)) {
    area_ha <- 100  # Default 100 ha
    log_message(sprintf("    No area specified, using default: %.0f ha", area_ha), "WARNING")
  }

  # Calculate conservative estimate
  conservative_stock <- max(0, mean_stock - qnorm((1 + CONFIDENCE_LEVEL) / 2) * se_stock)

  # Create VM0033 format (simplified - single "ALL" stratum)
  vm0033_data <- data.frame(
    method = "modeled",
    stratum = "ALL",
    area_ha = area_ha,
    mean_stock_0_100_Mg_ha = mean_stock,
    conservative_stock_0_100_Mg_ha = conservative_stock,
    total_stock_0_100_Mg = mean_stock * area_ha,
    conservative_total_0_100_Mg = conservative_stock * area_ha
  )

  log_message(sprintf("    Mean stock: %.1f ± %.1f Mg C/ha", mean_stock, se_stock))
  log_message(sprintf("    Source: %s", lit_record$source_citation))

  return(list(
    by_stratum = NULL,  # Not available from literature alone
    overall = NULL,
    vm0033 = vm0033_data
  ))
}

# ============================================================================
# PROCESS EACH SCENARIO
# ============================================================================

log_message("\n=== Processing Scenarios ===")

modeling_results <- data.frame()

for (i in 1:nrow(scenario_config)) {

  row <- scenario_config[i, ]

  log_message(sprintf("\n[%d/%d] Modeling: %s", i, nrow(scenario_config),
                     row$target_scenario))
  log_message(sprintf("  Method: %s", row$modeling_method))
  log_message(sprintf("  Ecosystem: %s", row$ecosystem_type))

  # METHOD 1: Copy Scenario
  if (row$modeling_method == "copy_scenario") {

    base_data <- load_scenario_data(row$base_scenario_1)

    if (is.null(base_data)) {
      log_message(sprintf("  ERROR: Base scenario '%s' not found", row$base_scenario_1), "ERROR")
      next
    }

    log_message(sprintf("  Copying from: %s", row$base_scenario_1))
    save_modeled_scenario(base_data, row$target_scenario, MONITORING_YEAR)

    modeling_results <- rbind(modeling_results, data.frame(
      target_scenario = row$target_scenario,
      method = "copy_scenario",
      base_scenario = row$base_scenario_1,
      years_post = row$years_post_restoration,
      status = "success"
    ))

  # METHOD 2: Reference Trajectory
  } else if (row$modeling_method == "reference_trajectory") {

    degraded_data <- load_scenario_data(row$base_scenario_1)
    reference_data <- load_scenario_data(row$base_scenario_2)

    if (is.null(degraded_data)) {
      log_message(sprintf("  ERROR: Degraded scenario '%s' not found", row$base_scenario_1), "ERROR")
      next
    }

    if (is.null(reference_data)) {
      log_message(sprintf("  ERROR: Reference scenario '%s' not found", row$base_scenario_2), "ERROR")
      next
    }

    log_message(sprintf("  Trajectory from: %s → %s", row$base_scenario_1, row$base_scenario_2))
    log_message(sprintf("  Years post-restoration: %d", row$years_post_restoration))

    modeled_data <- model_reference_trajectory(
      degraded_data,
      reference_data,
      years = row$years_post_restoration,
      ecosystem = row$ecosystem_type,
      recovery_model = RECOVERY_MODEL_TYPE
    )

    save_modeled_scenario(modeled_data, row$target_scenario, MONITORING_YEAR)

    modeling_results <- rbind(modeling_results, data.frame(
      target_scenario = row$target_scenario,
      method = "reference_trajectory",
      base_scenario = paste(row$base_scenario_1, row$base_scenario_2, sep = " → "),
      years_post = row$years_post_restoration,
      status = "success"
    ))

  # METHOD 3: Literature Lookup
  } else if (row$modeling_method == "literature_lookup") {

    log_message(sprintf("  Looking up literature values..."))

    # Determine scenario type for literature lookup
    lit_scenario_type <- if (grepl("PROJECT_Y", row$target_scenario)) {
      sprintf("restored_%dyr", row$years_post_restoration)
    } else if (row$target_scenario == "REFERENCE") {
      "reference_natural"
    } else if (row$target_scenario == "DEGRADED") {
      "degraded_lost"
    } else if (row$target_scenario == "BASELINE") {
      "baseline_typical"
    } else {
      row$target_scenario
    }

    modeled_data <- tryCatch({
      model_from_literature(
        ecosystem = row$ecosystem_type,
        scenario_type = lit_scenario_type,
        area_ha = NULL  # Will use default
      )
    }, error = function(e) {
      log_message(sprintf("  ERROR: %s", e$message), "ERROR")
      return(NULL)
    })

    if (!is.null(modeled_data)) {
      save_modeled_scenario(modeled_data, row$target_scenario, MONITORING_YEAR)

      modeling_results <- rbind(modeling_results, data.frame(
        target_scenario = row$target_scenario,
        method = "literature_lookup",
        base_scenario = lit_scenario_type,
        years_post = row$years_post_restoration,
        status = "success"
      ))
    } else {
      modeling_results <- rbind(modeling_results, data.frame(
        target_scenario = row$target_scenario,
        method = "literature_lookup",
        base_scenario = lit_scenario_type,
        years_post = row$years_post_restoration,
        status = "failed"
      ))
    }

  } else {
    log_message(sprintf("  ERROR: Unknown modeling method '%s'", row$modeling_method), "ERROR")
  }
}

# ============================================================================
# SAVE MODELING REPORT
# ============================================================================

log_message("\n=== Saving Modeling Report ===")

dir.create("outputs/scenario_modeling", recursive = TRUE, showWarnings = FALSE)

write.csv(modeling_results,
          "outputs/scenario_modeling/modeling_report.csv",
          row.names = FALSE)

log_message("Report saved: outputs/scenario_modeling/modeling_report.csv")

# ============================================================================
# SUMMARY
# ============================================================================

log_message("\n=== MODULE 08A COMPLETE ===")
log_message(sprintf("Successfully modeled: %d scenarios", sum(modeling_results$status == "success")))
log_message(sprintf("Failed: %d scenarios", sum(modeling_results$status == "failed")))
log_message("\nModeled scenarios can now be used in Module 08 (Temporal Harmonization)")
log_message("Next steps:")
log_message("  1. Review outputs in outputs/carbon_stocks/[SCENARIO]_[YEAR]/")
log_message("  2. Run Module 08 to harmonize all scenarios")
log_message("  3. Run Module 09 for additionality analysis")
log_message("  4. Run Module 10 for VM0033 verification package")
