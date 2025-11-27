# ============================================================================
# MODULE P5_02: TRAJECTORY MODELING (BASELINE + PROJECT)
# ============================================================================
# PURPOSE: Model carbon trajectories for BOTH baseline and project scenarios
#          to enable additionality calculations
#
# WHAT THIS DOES:
#   1. Models BASELINE scenario (what happens WITHOUT your project)
#   2. Models PROJECT scenario (what happens WITH restoration)
#   3. Calculates additionality (PROJECT - BASELINE)
#   4. Saves outputs for VM0033 crediting
#
# BASELINE OPTIONS:
#   - NO_ACTION: Carbon stays constant (no change)
#   - DEGRADATION: Ongoing carbon loss
#   - NATURAL_RECOVERY: Slow natural recovery
#   - Any restoration action (e.g., current management)
#
# PROJECT OPTIONS:
#   - Any restoration action(s) from restoration_actions.csv
#   - Multiple actions combined with commas
#
# OUTPUTS:
#   - scenarios/BASELINE_*/trajectories/ (baseline projections)
#   - scenarios/PROJECT_*/trajectories/ (project projections)
#   - Both ready for P5_03 crediting calculations
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
log_file <- file.path("logs", paste0("trajectory_modeling_", Sys.Date(), ".log"))
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_entry <- sprintf("%s %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

cat("\n========================================\n")
cat("SCENARIO MODELING: BASELINE + PROJECT\n")
cat("========================================\n\n")

log_message("=== MODULE P5_02: TRAJECTORY MODELING ===")

# ============================================================================
# HELPER FUNCTIONS (Define before use)
# ============================================================================

run_trajectory <- function(zones_sf, baseline_field, k_field, rate_field, target_field,
                           scenario_name, years, method) {
  
  # Create output directory
  scenario_dir <- file.path("scenarios", scenario_name)
  dir.create(file.path(scenario_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(scenario_dir, "rasters"), recursive = TRUE, showWarnings = FALSE)
  
  # Calculate trajectory for each year
  trajectory_data <- list()
  
  for (year in years) {
    zones_sf[[paste0("year_", year)]] <- mapply(
      function(C0, Ctarget, k, rate) {
        if (method == "exponential") {
          C0 + (Ctarget - C0) * (1 - exp(-k * year))
        } else {
          pmin(C0 + rate * year, Ctarget)
        }
      },
      C0 = zones_sf[[baseline_field]],
      Ctarget = zones_sf[[target_field]],
      k = zones_sf[[k_field]],
      rate = zones_sf[[rate_field]]
    )
    
    mean_stock <- mean(zones_sf[[paste0("year_", year)]], na.rm = TRUE)
    total_area <- sum(zones_sf$area_ha)
    
    trajectory_data[[as.character(year)]] <- data.frame(
      year = year,
      mean_stock_Mg_ha = mean_stock,
      total_stock_Mg = mean_stock * total_area,
      total_stock_tCO2e = mean_stock * total_area * (44/12)
    )
  }
  
  # Combine trajectory
  traj <- bind_rows(trajectory_data)
  
  # Save
  write_csv(traj, file.path(scenario_dir, "tables", "trajectory_summary.csv"))
  write_yaml(list(scenario_name = scenario_name,
                  total_area_ha = sum(zones_sf$area_ha),
                  baseline_carbon_Mg = traj$total_stock_Mg[1]),
             file.path(scenario_dir, "scenario_definition.yaml"))
  
  return(traj)
}

assign_recovery_params <- function(zones_sf, recovery_params, actions, baseline_field) {
  
  # Function to parse multiple actions
  parse_actions <- function(action_string) {
    if (is.na(action_string)) return(c("NO_ACTION"))
    trimws(strsplit(action_string, "[,;|]")[[1]])
  }
  
  # Assign recovery parameters
  for (i in 1:nrow(zones_sf)) {
    action_ids <- parse_actions(zones_sf$action_id[i])
    params <- recovery_params %>% filter(action_id %in% action_ids)
    
    if (nrow(params) == 0) {
      params <- recovery_params %>% filter(action_id == "NO_ACTION")
    }
    
    zones_sf$recovery_k[i] <- sum(params$recovery_k, na.rm = TRUE)
    zones_sf$linear_rate_Mg_ha_yr[i] <- sum(params$linear_rate_Mg_ha_yr, na.rm = TRUE)
    zones_sf$max_target_Mg_ha[i] <- max(params$max_target_Mg_ha, na.rm = TRUE)
    zones_sf$years_to_effect[i] <- min(actions$years_to_effect[actions$action_id %in% action_ids])
    zones_sf$cost_per_ha[i] <- sum(actions$cost_per_ha[actions$action_id %in% action_ids])
  }
  
  # Ensure targets > baseline for restoration
  problem_idx <- which(zones_sf$action_id != "NO_ACTION" & 
                         zones_sf$max_target_Mg_ha <= zones_sf[[baseline_field]])
  if (length(problem_idx) > 0) {
    zones_sf$max_target_Mg_ha[problem_idx] <- zones_sf[[baseline_field]][problem_idx] + 50
  }
  
  return(zones_sf)
}

# ============================================================================
# LOAD SPATIAL STRUCTURE
# ============================================================================

log_message("\n=== Loading Spatial Structure ===")

baseline_dir <- file.path("scenarios", BASELINE_SCENARIO_NAME)
if (!dir.exists(baseline_dir)) {
  stop(sprintf("Baseline scenario not found: %s\nPlease run P5_01 first.", baseline_dir))
}

# Load scenario definition
scenario_def <- read_yaml(file.path(baseline_dir, "scenario_definition.yaml"))
log_message(sprintf("Loaded spatial structure: %s", scenario_def$scenario_name))

# Load baseline raster - try multiple paths
baseline_raster_path <- NULL
possible_paths <- c(
  file.path(baseline_dir, scenario_def$files$baseline_raster),
  file.path(baseline_dir, "rasters", "baseline_stocks_total.tif"),
  file.path(baseline_dir, "baseline_stocks_total.tif")
)

for (path in possible_paths) {
  if (!is.null(path) && file.exists(path)) {
    baseline_raster_path <- path
    break
  }
}

if (is.null(baseline_raster_path)) {
  stop(sprintf("Cannot find baseline raster in %s\nExpected: rasters/baseline_stocks_total.tif", baseline_dir))
}

baseline_raster <- rast(baseline_raster_path)
log_message(sprintf("Loaded baseline raster: %s", basename(baseline_raster_path)))

# Load zones - try multiple paths
zones_path <- NULL
possible_zone_paths <- c(
  file.path(baseline_dir, scenario_def$files$management_zones_gpkg),
  file.path(baseline_dir, "vectors", "management_zones.gpkg"),
  file.path(baseline_dir, "vectors", "management_zones.shp")
)

for (path in possible_zone_paths) {
  if (!is.null(path) && file.exists(path)) {
    zones_path <- path
    break
  }
}

if (is.null(zones_path)) {
  stop(sprintf("Cannot find management zones in %s\nExpected: vectors/management_zones.gpkg or .shp", baseline_dir))
}

zones_sf <- st_read(zones_path, quiet = TRUE)
log_message(sprintf("Loaded zones: %s", basename(zones_path)))

# Handle field name variations
if ("base_C_avg" %in% names(zones_sf)) {
  zones_sf <- zones_sf %>%
    rename(baseline_carbon_mean_Mg_ha = base_C_avg,
           baseline_carbon_sd_Mg_ha = base_C_sd)
}

baseline_field <- "baseline_carbon_mean_Mg_ha"
log_message(sprintf("Loaded %d zones, %.1f ha total", 
                    nrow(zones_sf), sum(zones_sf$area_ha)))

# Load actions
actions <- read_csv("restoration_actions.csv", show_col_types = FALSE)

# ============================================================================
# MODEL 1: BASELINE SCENARIO
# ============================================================================

cat("\n========================================\n")
cat("MODELING BASELINE SCENARIO\n")
cat("========================================\n\n")

log_message(sprintf("\n=== Baseline: %s ===", BASELINE_ACTION))

# Create baseline-specific recovery parameters
if (BASELINE_ACTION == "NO_ACTION") {
  zones_sf$baseline_k <- 0
  zones_sf$baseline_rate <- 0
  zones_sf$baseline_target <- zones_sf[[baseline_field]]
  log_message("  No change - carbon stocks constant")
  
} else if (BASELINE_ACTION == "DEGRADATION") {
  zones_sf$baseline_k <- 0
  zones_sf$baseline_rate <- BASELINE_DEGRADATION_RATE  # Negative
  zones_sf$baseline_target <- zones_sf[[baseline_field]] + (BASELINE_DEGRADATION_RATE * MAX_PROJECTION_YEARS)
  log_message(sprintf("  Degradation: %.2f Mg C/ha/yr loss", BASELINE_DEGRADATION_RATE))
  
} else if (BASELINE_ACTION == "NATURAL_RECOVERY") {
  zones_sf$baseline_k <- 0.03  # Very slow
  zones_sf$baseline_rate <- BASELINE_NATURAL_RECOVERY_RATE
  zones_sf$baseline_target <- zones_sf[[baseline_field]] * 1.2  # 20% increase over 30 years
  log_message(sprintf("  Natural recovery: %.2f Mg C/ha/yr", BASELINE_NATURAL_RECOVERY_RATE))
  
} else {
  # Use specified action
  action_params <- actions %>% filter(action_id == BASELINE_ACTION)
  if (nrow(action_params) == 0) {
    stop(sprintf("Baseline action '%s' not found in restoration_actions.csv", BASELINE_ACTION))
  }
  zones_sf$baseline_k <- 0.08  # Moderate
  zones_sf$baseline_rate <- 1.5
  zones_sf$baseline_target <- zones_sf[[baseline_field]] * 1.3
  log_message(sprintf("  Custom action: %s", BASELINE_ACTION))
}

# Run baseline trajectory
baseline_traj <- run_trajectory(
  zones_sf = zones_sf,
  baseline_field = baseline_field,
  k_field = "baseline_k",
  rate_field = "baseline_rate",
  target_field = "baseline_target",
  scenario_name = BASELINE_SCENARIO_NAME,
  years = PROJECTION_YEARS,
  method = TRAJECTORY_METHOD
)

log_message(sprintf("✓ Baseline trajectory: %.1f → %.1f Mg C/ha",
                    baseline_traj$mean_stock_Mg_ha[1],
                    baseline_traj$mean_stock_Mg_ha[nrow(baseline_traj)]))

# ============================================================================
# MODEL 2: PROJECT SCENARIO
# ============================================================================

cat("\n========================================\n")
cat("MODELING PROJECT SCENARIO\n")
cat("========================================\n\n")

log_message(sprintf("\n=== Project: %s ===", 
                    ifelse(ACTION_ASSIGNMENT_METHOD == "uniform", UNIFORM_ACTION, "zone-specific")))

# Assign project actions
if (ACTION_ASSIGNMENT_METHOD == "uniform") {
  zones_sf$action_id <- UNIFORM_ACTION
  log_message(sprintf("  Uniform action: %s", UNIFORM_ACTION))
} else {
  zones_sf <- zones_sf %>%
    left_join(ZONE_SPECIFIC_ACTIONS %>% select(zone_id, action_id), by = "zone_id")
  log_message("  Zone-specific actions assigned")
}

# Load/create recovery parameters
if (file.exists("recovery_parameters.csv")) {
  recovery_params <- read_csv("recovery_parameters.csv", show_col_types = FALSE)
} else {
  mean_baseline <- mean(zones_sf[[baseline_field]], na.rm = TRUE)
  target <- max(DEFAULT_TARGET_CARBON, mean_baseline * 1.5)
  
  recovery_params <- actions %>%
    mutate(
      recovery_k = case_when(
        action_id == "NO_ACTION" ~ 0.0,
        action_id %in% c("RESTORE_HYDRO", "RECONNECT_CHANNEL") ~ 0.15,
        action_id %in% c("PLANT_NATIVE_MARSH", "PLANT_SEAGRASS") ~ 0.12,
        TRUE ~ 0.10
      ),
      linear_rate_Mg_ha_yr = case_when(
        action_id == "NO_ACTION" ~ 0.0,
        action_id %in% c("RESTORE_HYDRO", "RECONNECT_CHANNEL") ~ 3.0,
        action_id %in% c("PLANT_NATIVE_MARSH", "PLANT_SEAGRASS") ~ 2.5,
        TRUE ~ 2.0
      ),
      max_target_Mg_ha = ifelse(action_id == "NO_ACTION", mean_baseline, target)
    )
  write_csv(recovery_params %>% select(action_id, recovery_k, linear_rate_Mg_ha_yr, max_target_Mg_ha),
            "recovery_parameters.csv")
}

# Process multiple actions and assign parameters
zones_sf <- assign_recovery_params(zones_sf, recovery_params, actions, baseline_field)

# Run project trajectory
project_traj <- run_trajectory(
  zones_sf = zones_sf,
  baseline_field = baseline_field,
  k_field = "recovery_k",
  rate_field = "linear_rate_Mg_ha_yr",
  target_field = "max_target_Mg_ha",
  scenario_name = PROJECT_SCENARIO_NAME,
  years = PROJECTION_YEARS,
  method = TRAJECTORY_METHOD
)

log_message(sprintf("✓ Project trajectory: %.1f → %.1f Mg C/ha",
                    project_traj$mean_stock_Mg_ha[1],
                    project_traj$mean_stock_Mg_ha[nrow(project_traj)]))

# ============================================================================
# CALCULATE ADDITIONALITY
# ============================================================================

cat("\n========================================\n")
cat("ADDITIONALITY SUMMARY\n")
cat("========================================\n\n")

additionality <- baseline_traj %>%
  select(year, baseline_Mg_ha = mean_stock_Mg_ha, baseline_tCO2e = total_stock_tCO2e) %>%
  left_join(
    project_traj %>% select(year, project_Mg_ha = mean_stock_Mg_ha, project_tCO2e = total_stock_tCO2e),
    by = "year"
  ) %>%
  mutate(
    additional_Mg_ha = project_Mg_ha - baseline_Mg_ha,
    additional_tCO2e = project_tCO2e - baseline_tCO2e
  )

cat("Carbon Stock Trajectories:\n")
cat(sprintf("  Year 0: Baseline = %.1f, Project = %.1f Mg C/ha\n",
            additionality$baseline_Mg_ha[1], additionality$project_Mg_ha[1]))
cat(sprintf("  Year %d: Baseline = %.1f, Project = %.1f Mg C/ha\n\n",
            max(PROJECTION_YEARS),
            additionality$baseline_Mg_ha[nrow(additionality)],
            additionality$project_Mg_ha[nrow(additionality)]))

cat("Additionality (Project - Baseline):\n")
cat(sprintf("  Year %d: %.1f Mg C/ha (%.0f tonnes CO₂e)\n\n",
            max(PROJECTION_YEARS),
            additionality$additional_Mg_ha[nrow(additionality)],
            additionality$additional_tCO2e[nrow(additionality)]))

# Save additionality summary
write_csv(additionality, "scenarios/additionality_summary.csv")
log_message("Saved: scenarios/additionality_summary.csv")

# ============================================================================
# CREATE COMPARISON PLOT
# ============================================================================

if (CREATE_PLOTS) {
  p <- ggplot(additionality, aes(x = year)) +
    geom_line(aes(y = baseline_Mg_ha, color = "Baseline"), linewidth = 1.5) +
    geom_line(aes(y = project_Mg_ha, color = "Project"), linewidth = 1.5) +
    geom_ribbon(aes(ymin = baseline_Mg_ha, ymax = project_Mg_ha), 
                fill = "green", alpha = 0.3) +
    scale_color_manual(values = c("Baseline" = "red", "Project" = "darkgreen")) +
    labs(
      title = "Baseline vs Project Carbon Trajectories",
      subtitle = sprintf("Baseline: %s | Project: %s", BASELINE_ACTION, 
                         ifelse(ACTION_ASSIGNMENT_METHOD == "uniform", UNIFORM_ACTION, "mixed")),
      x = "Years",
      y = "Carbon Stock (Mg C/ha)",
      color = "Scenario",
      caption = "Green area = Additionality"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top")
  
  ggsave("scenarios/baseline_vs_project.png", p, width = 10, height = 6, dpi = PLOT_DPI)
  cat("📈 Saved: scenarios/baseline_vs_project.png\n\n")
}

cat("========================================\n")
cat("TRAJECTORY MODELING COMPLETE\n")
cat("========================================\n\n")
cat("Next step: Run P5_03_vm0033_crediting.R\n\n")

log_message("=== MODULE P5_02 COMPLETE ===")