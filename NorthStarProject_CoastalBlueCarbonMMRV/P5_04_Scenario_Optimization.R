# ============================================================================
# MODULE P5_04: MULTI-SCENARIO COMPARISON & OPTIMIZATION
# ============================================================================
# PURPOSE: Compare restoration scenarios and identify optimal strategies
#
# FEATURES:
#   1. Carbon Efficiency Analysis
#      - Cost-effectiveness curves ($/tCO2e)
#      - Marginal abatement cost curves
#      - Identify low-hanging fruit
#
#   2. Co-benefits Scoring
#      - Biodiversity (habitat quality)
#      - Flood protection (marsh elevation/resilience)
#      - Water quality (nutrient retention)
#      - Multi-criteria decision matrix
#
#   3. Spatial Prioritization
#      - Budget-constrained optimization
#      - Maximize carbon sequestration given budget
#      - Prioritized restoration zones
#
#   4. Uncertainty-Aware Planning
#      - Monte Carlo trajectory simulation
#      - Probability of achieving targets
#      - Risk-adjusted decision making
#
# INPUTS:
#   - Multiple scenario outputs from P5_02 and P5_03
#   - Co-benefit rasters (optional)
#   - Budget constraints
#
# OUTPUTS:
#   - Scenario comparison tables and plots
#   - Spatial priority maps
#   - Optimization recommendations
#   - Risk assessment reports
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
  library(patchwork)
})

# Load configuration
source("blue_carbon_config.R")
source("P5_scenario_config.R")

# Setup logging
log_file <- file.path("logs", paste0("scenario_optimization_", Sys.Date(), ".log"))
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_entry <- sprintf("%s %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

cat("\n========================================\n")
cat("MULTI-SCENARIO OPTIMIZATION\n")
cat("========================================\n\n")

log_message("=== MODULE P5_04: SCENARIO COMPARISON & OPTIMIZATION ===")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Scenarios to compare (add your scenario names here)
SCENARIOS_TO_COMPARE <- c(
  "PROJECT_RESTORATION"
  # Add more scenario names as you create them:
  # "PROJECT_HYDRO",
  # "PROJECT_COMBINED",
  # "PROJECT_LOWCOST"
)

# Budget constraint for optimization (USD)
BUDGET_CONSTRAINT <- 500000  # $500k example

# Co-benefit weights for multi-criteria analysis
WEIGHT_CARBON <- 0.4
WEIGHT_BIODIVERSITY <- 0.2
WEIGHT_FLOOD_PROTECTION <- 0.2
WEIGHT_WATER_QUALITY <- 0.2

# Uncertainty simulation parameters
N_MONTE_CARLO_ITERATIONS <- 1000
TARGET_CARBON_TCO2E <- 1000  # Target to achieve
TARGET_YEAR <- 10  # By which year

# Create output directory
output_dir <- "scenarios/optimization"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: LOAD ALL SCENARIOS
# ============================================================================

log_message("\n=== STEP 1: Loading Scenarios for Comparison ===")

scenarios_list <- list()
baseline_scenario <- NULL

for (scenario_name in SCENARIOS_TO_COMPARE) {
  
  scenario_dir <- file.path("scenarios", scenario_name)
  
  if (!dir.exists(scenario_dir)) {
    log_message(sprintf("Skipping %s - not found", scenario_name), "WARNING")
    next
  }
  
  # Load trajectory
  traj_file <- file.path(scenario_dir, "tables", "trajectory_summary.csv")
  if (!file.exists(traj_file)) {
    log_message(sprintf("Skipping %s - no trajectory", scenario_name), "WARNING")
    next
  }
  
  traj <- read_csv(traj_file, show_col_types = FALSE)
  
  # Load scenario definition
  def_file <- file.path(scenario_dir, "scenario_definition.yaml")
  scenario_def <- if (file.exists(def_file)) read_yaml(def_file) else list()
  
  # Load crediting if available
  credit_file <- file.path(scenario_dir, "crediting", "credit_issuance_schedule.csv")
  credits <- if (file.exists(credit_file)) {
    read_csv(credit_file, show_col_types = FALSE)
  } else {
    NULL
  }
  
  scenarios_list[[scenario_name]] <- list(
    name = scenario_name,
    trajectory = traj,
    definition = scenario_def,
    credits = credits,
    total_cost = if (!is.null(scenario_def$total_restoration_cost)) {
      scenario_def$total_restoration_cost
    } else {
      0
    }
  )
  
  log_message(sprintf("Loaded: %s", scenario_name))
}

# Load baseline separately
baseline_dir <- file.path("scenarios", BASELINE_SCENARIO_NAME)
if (dir.exists(baseline_dir)) {
  baseline_traj <- read_csv(file.path(baseline_dir, "tables", "trajectory_summary.csv"),
                            show_col_types = FALSE)
  baseline_scenario <- list(
    name = BASELINE_SCENARIO_NAME,
    trajectory = baseline_traj
  )
  log_message(sprintf("Loaded baseline: %s", BASELINE_SCENARIO_NAME))
}

if (length(scenarios_list) == 0) {
  stop("No scenarios found to compare. Please run P5_02 to create scenarios first.")
}

log_message(sprintf("\nComparing %d scenarios", length(scenarios_list)))

# ============================================================================
# STEP 2: CARBON EFFICIENCY ANALYSIS
# ============================================================================

log_message("\n=== STEP 2: Carbon Efficiency Analysis ===")

cat("\n========================================\n")
cat("CARBON EFFICIENCY ANALYSIS\n")
cat("========================================\n\n")

# Calculate efficiency metrics for each scenario
efficiency_data <- data.frame()

for (scenario in scenarios_list) {
  
  # Get final year carbon
  final_year <- max(scenario$trajectory$year)
  final_carbon_Mg <- scenario$trajectory$total_stock_Mg[nrow(scenario$trajectory)]
  baseline_carbon_Mg <- if (!is.null(baseline_scenario)) {
    baseline_scenario$trajectory$total_stock_Mg[baseline_scenario$trajectory$year == final_year]
  } else {
    scenario$trajectory$total_stock_Mg[1]
  }
  
  # Calculate additionality
  additional_Mg <- final_carbon_Mg - baseline_carbon_Mg
  additional_tCO2e <- additional_Mg * (44/12)
  
  # Get creditable carbon if available
  creditable_tCO2e <- if (!is.null(scenario$credits)) {
    sum(scenario$credits$creditable_tCO2e, na.rm = TRUE)
  } else {
    additional_tCO2e * 0.75  # Conservative estimate (75% after deductions)
  }
  
  # Calculate cost-effectiveness
  cost_per_tCO2e <- if (scenario$total_cost > 0 && creditable_tCO2e > 0) {
    scenario$total_cost / creditable_tCO2e
  } else {
    NA
  }
  
  efficiency_data <- rbind(efficiency_data, data.frame(
    scenario = scenario$name,
    total_cost = scenario$total_cost,
    additional_Mg = additional_Mg,
    additional_tCO2e = additional_tCO2e,
    creditable_tCO2e = creditable_tCO2e,
    cost_per_tCO2e = cost_per_tCO2e,
    stringsAsFactors = FALSE
  ))
}

# Rank by cost-effectiveness
efficiency_data <- efficiency_data %>%
  arrange(cost_per_tCO2e)

# Display results
cat("Cost-Effectiveness Ranking:\n")
cat(sprintf("%-25s | %12s | %15s | %12s\n", 
            "Scenario", "Total Cost", "Credits (tCO2e)", "$/tCO2e"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (i in 1:nrow(efficiency_data)) {
  cat(sprintf("%-25s | $%11.0f | %15.0f | $%11.2f\n",
              efficiency_data$scenario[i],
              efficiency_data$total_cost[i],
              efficiency_data$creditable_tCO2e[i],
              efficiency_data$cost_per_tCO2e[i]))
}

# Save results
write_csv(efficiency_data, file.path(output_dir, "scenario_efficiency_comparison.csv"))
log_message("Saved: scenario_efficiency_comparison.csv")

# Create cost-effectiveness plot
if (nrow(efficiency_data) > 1) {
  
  p1 <- ggplot(efficiency_data, aes(x = reorder(scenario, cost_per_tCO2e), 
                                    y = cost_per_tCO2e)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = sprintf("$%.0f", cost_per_tCO2e)), 
              hjust = -0.2, size = 4) +
    coord_flip() +
    labs(
      title = "Cost-Effectiveness Comparison",
      subtitle = "Lower = Better",
      x = "",
      y = "Cost per tonne CO₂e ($/tCO2e)"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "cost_effectiveness_ranking.png"),
         p1, width = 10, height = 6, dpi = 300)
  
  log_message("Saved: cost_effectiveness_ranking.png")
}

# Marginal abatement cost curve
if (nrow(efficiency_data) > 1) {
  
  efficiency_data_sorted <- efficiency_data %>%
    arrange(cost_per_tCO2e) %>%
    mutate(cumulative_tCO2e = cumsum(creditable_tCO2e))
  
  p2 <- ggplot(efficiency_data_sorted, 
               aes(x = cumulative_tCO2e, y = cost_per_tCO2e)) +
    geom_step(linewidth = 1.5, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    labs(
      title = "Marginal Abatement Cost Curve",
      subtitle = "Implement low-cost options first (left to right)",
      x = "Cumulative Carbon Credits (tCO2e)",
      y = "Marginal Cost ($/tCO2e)"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "marginal_abatement_curve.png"),
         p2, width = 10, height = 6, dpi = 300)
  
  log_message("Saved: marginal_abatement_curve.png")
}

# ============================================================================
# STEP 3: CO-BENEFITS SCORING
# ============================================================================

log_message("\n=== STEP 3: Co-Benefits Multi-Criteria Analysis ===")

cat("\n========================================\n")
cat("CO-BENEFITS ASSESSMENT\n")
cat("========================================\n\n")

# Check if co-benefit scores from P5_04b exist
cobenefit_file <- "scenarios/cobenefits/cobenefit_summary_by_zone.csv"
use_calculated_cobenefits <- file.exists(cobenefit_file)

if (use_calculated_cobenefits) {
  
  log_message("Loading calculated co-benefit scores from P5_04b")
  cat("Using co-benefit scores from P5_04b_cobenefit_modeling.R\n\n")
  
  cobenefit_data <- read_csv(cobenefit_file, show_col_types = FALSE)
  
  # Create co-benefits scoring matrix
  cobenefit_scores <- data.frame()
  
  for (scenario in scenarios_list) {
    
    # Calculate carbon score
    carbon_score <- scenario$trajectory$total_stock_Mg[nrow(scenario$trajectory)] / 
      max(sapply(scenarios_list, function(s) max(s$trajectory$total_stock_Mg)))
    
    # Get mean co-benefit scores from calculated data
    biodiversity_score <- mean(cobenefit_data$biodiversity_score, na.rm = TRUE)
    flood_protection_score <- mean(cobenefit_data$flood_protection_score, na.rm = TRUE)
    water_quality_score <- mean(cobenefit_data$water_quality_score, na.rm = TRUE)
    
    # Calculate weighted score
    weighted_score <- (carbon_score * WEIGHT_CARBON) +
      (biodiversity_score * WEIGHT_BIODIVERSITY) +
      (flood_protection_score * WEIGHT_FLOOD_PROTECTION) +
      (water_quality_score * WEIGHT_WATER_QUALITY)
    
    cobenefit_scores <- rbind(cobenefit_scores, data.frame(
      scenario = scenario$name,
      carbon_score = carbon_score,
      biodiversity_score = biodiversity_score,
      flood_protection_score = flood_protection_score,
      water_quality_score = water_quality_score,
      weighted_total = weighted_score,
      stringsAsFactors = FALSE
    ))
  }
  
} else {
  
  log_message("Using placeholder co-benefit scores (run P5_04b for accurate values)", "WARNING")
  cat("NOTE: Using placeholder scores. Run P5_04b_cobenefit_modeling.R for accurate co-benefits.\n\n")
  
  # Create co-benefits scoring matrix with placeholders
  cobenefit_scores <- data.frame()
  
  for (scenario in scenarios_list) {
    
    # Extract action types to estimate co-benefits
    action_name <- scenario$name
    
    # Simplified scoring based on action type
    biodiversity_score <- if (grepl("PLANT", action_name)) 0.9 else 0.6
    flood_protection_score <- if (grepl("HYDRO|SEDIMENT", action_name)) 0.9 else 0.5
    water_quality_score <- if (grepl("PLANT|INVASIVE", action_name)) 0.8 else 0.5
    carbon_score <- scenario$trajectory$total_stock_Mg[nrow(scenario$trajectory)] / 
      max(sapply(scenarios_list, function(s) max(s$trajectory$total_stock_Mg)))
    
    # Calculate weighted score
    weighted_score <- (carbon_score * WEIGHT_CARBON) +
      (biodiversity_score * WEIGHT_BIODIVERSITY) +
      (flood_protection_score * WEIGHT_FLOOD_PROTECTION) +
      (water_quality_score * WEIGHT_WATER_QUALITY)
    
    cobenefit_scores <- rbind(cobenefit_scores, data.frame(
      scenario = scenario$name,
      carbon_score = carbon_score,
      biodiversity_score = biodiversity_score,
      flood_protection_score = flood_protection_score,
      water_quality_score = water_quality_score,
      weighted_total = weighted_score,
      stringsAsFactors = FALSE
    ))
  }
}

# Rank by weighted total
cobenefit_scores <- cobenefit_scores %>%
  arrange(desc(weighted_total))

cat("Multi-Criteria Ranking (weights: C=", WEIGHT_CARBON, ", B=", WEIGHT_BIODIVERSITY, 
    ", F=", WEIGHT_FLOOD_PROTECTION, ", W=", WEIGHT_WATER_QUALITY, "):\n\n", sep="")

cat(sprintf("%-25s | %6s | %6s | %6s | %6s | %6s\n",
            "Scenario", "Carbon", "Biodiv", "Flood", "Water", "TOTAL"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (i in 1:nrow(cobenefit_scores)) {
  cat(sprintf("%-25s | %6.2f | %6.2f | %6.2f | %6.2f | %6.2f\n",
              cobenefit_scores$scenario[i],
              cobenefit_scores$carbon_score[i],
              cobenefit_scores$biodiversity_score[i],
              cobenefit_scores$flood_protection_score[i],
              cobenefit_scores$water_quality_score[i],
              cobenefit_scores$weighted_total[i]))
}

write_csv(cobenefit_scores, file.path(output_dir, "cobenefit_scores.csv"))
log_message("Saved: cobenefit_scores.csv")

if (!use_calculated_cobenefits) {
  cat("\nRECOMMENDATION: Run P5_04b_cobenefit_modeling.R for accurate co-benefit scores.\n")
  cat("Current scores are placeholders based on action types.\n\n")
}

# Create radar plot for co-benefits
if (nrow(cobenefit_scores) > 0) {
  
  cobenefit_long <- cobenefit_scores %>%
    select(-weighted_total) %>%
    pivot_longer(cols = -scenario, names_to = "benefit", values_to = "score") %>%
    mutate(benefit = gsub("_score", "", benefit))
  
  p3 <- ggplot(cobenefit_long, aes(x = benefit, y = score, group = scenario, color = scenario)) +
    geom_polygon(fill = NA, linewidth = 1) +
    geom_point(size = 3) +
    coord_polar() +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Co-Benefits Profile by Scenario",
      subtitle = "Larger area = Greater co-benefits",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 12))
  
  ggsave(file.path(output_dir, "cobenefit_radar.png"),
         p3, width = 10, height = 8, dpi = 300)
  
  log_message("Saved: cobenefit_radar.png")
}

# ============================================================================
# STEP 4: SPATIAL PRIORITIZATION
# ============================================================================

log_message("\n=== STEP 4: Spatial Prioritization Under Budget Constraint ===")

cat("\n========================================\n")
cat("SPATIAL PRIORITIZATION\n")
cat("========================================\n\n")

cat(sprintf("Budget constraint: $%s\n\n", format(BUDGET_CONSTRAINT, big.mark = ",")))

# Load spatial data for optimization
baseline_dir <- file.path("scenarios", BASELINE_SCENARIO_NAME)
zones_path <- file.path(baseline_dir, "vectors", "management_zones.gpkg")
if (!file.exists(zones_path)) {
  zones_path <- file.path(baseline_dir, "vectors", "management_zones.shp")
}

if (!file.exists(zones_path)) {
  log_message("Spatial zones not found - skipping spatial prioritization", "WARNING")
  cat("Spatial prioritization requires management zones from P5_01\n\n")
} else {
  
  zones_sf <- st_read(zones_path, quiet = TRUE)
  
  # Handle field names
  if ("base_C_avg" %in% names(zones_sf)) {
    names(zones_sf)[names(zones_sf) == "base_C_avg"] <- "baseline_carbon_mean_Mg_ha"
    names(zones_sf)[names(zones_sf) == "area_ha"] <- "area_ha"
  }
  
  # Load restoration actions
  actions <- read_csv("restoration_actions.csv", show_col_types = FALSE)
  
  # Calculate carbon gain potential per zone for each action
  zones_sf$priority_score <- 0
  
  for (i in 1:nrow(zones_sf)) {
    
    # Get zone properties
    baseline_C <- zones_sf$baseline_carbon_mean_Mg_ha[i]
    area <- zones_sf$area_ha[i]
    
    # Estimate potential gain (simple: 30% increase over 30 years)
    potential_gain_Mg_ha <- baseline_C * 0.30
    potential_gain_total_tCO2e <- potential_gain_Mg_ha * area * (44/12)
    
    # Estimate cost (use RESTORE_HYDRO as example)
    cost_per_ha <- 15000
    total_cost <- cost_per_ha * area
    
    # Calculate efficiency score
    if (total_cost > 0) {
      efficiency <- potential_gain_total_tCO2e / total_cost
    } else {
      efficiency <- 0
    }
    
    zones_sf$potential_gain_tCO2e[i] <- potential_gain_total_tCO2e
    zones_sf$estimated_cost[i] <- total_cost
    zones_sf$efficiency_score[i] <- efficiency
    zones_sf$priority_score[i] <- efficiency
  }
  
  # Rank zones by efficiency
  zones_sf <- zones_sf %>%
    arrange(desc(efficiency_score))
  
  # Budget-constrained selection
  zones_sf$cumulative_cost <- cumsum(zones_sf$estimated_cost)
  zones_sf$within_budget <- zones_sf$cumulative_cost <= BUDGET_CONSTRAINT
  
  n_zones_affordable <- sum(zones_sf$within_budget)
  affordable_area <- sum(zones_sf$area_ha[zones_sf$within_budget])
  affordable_carbon <- sum(zones_sf$potential_gain_tCO2e[zones_sf$within_budget])
  
  cat("Budget-Optimized Restoration Priority:\n")
  cat(sprintf("  Zones to restore: %d\n", n_zones_affordable))
  cat(sprintf("  Total area: %.1f ha\n", affordable_area))
  cat(sprintf("  Expected carbon gain: %.0f tCO2e\n", affordable_carbon))
  cat(sprintf("  Total cost: $%s\n", 
              format(max(zones_sf$cumulative_cost[zones_sf$within_budget]), big.mark = ",")))
  cat(sprintf("  Average cost-effectiveness: $%.2f/tCO2e\n\n",
              max(zones_sf$cumulative_cost[zones_sf$within_budget]) / affordable_carbon))
  
  # Save priority zones
  priority_zones <- zones_sf %>%
    st_drop_geometry() %>%
    select(zone_id, area_ha, baseline_carbon_mean_Mg_ha, 
           potential_gain_tCO2e, estimated_cost, efficiency_score, 
           cumulative_cost, within_budget) %>%
    arrange(desc(efficiency_score))
  
  write_csv(priority_zones, file.path(output_dir, "spatial_priority_zones.csv"))
  log_message("Saved: spatial_priority_zones.csv")
  
  # Save spatial priority map
  st_write(zones_sf %>% select(zone_id, priority_score, within_budget, 
                               potential_gain_tCO2e, estimated_cost),
           file.path(output_dir, "priority_zones.gpkg"),
           delete_dsn = TRUE, quiet = TRUE)
  log_message("Saved: priority_zones.gpkg")
  
  cat("Priority zones saved to:\n")
  cat(sprintf("  - %s\n", file.path(output_dir, "spatial_priority_zones.csv")))
  cat(sprintf("  - %s\n\n", file.path(output_dir, "priority_zones.gpkg")))
}

# ============================================================================
# STEP 5: UNCERTAINTY-AWARE PLANNING
# ============================================================================

log_message("\n=== STEP 5: Uncertainty Analysis & Risk Assessment ===")

cat("\n========================================\n")
cat("MONTE CARLO UNCERTAINTY ANALYSIS\n")
cat("========================================\n\n")

cat(sprintf("Simulating %d trajectories...\n", N_MONTE_CARLO_ITERATIONS))
cat(sprintf("Target: %d tCO2e by Year %d\n\n", TARGET_CARBON_TCO2E, TARGET_YEAR))

# Run Monte Carlo for best scenario
if (length(scenarios_list) > 0) {
  
  best_scenario <- scenarios_list[[1]]
  
  # Get trajectory at target year
  target_traj <- best_scenario$trajectory %>%
    filter(year == TARGET_YEAR)
  
  if (nrow(target_traj) > 0) {
    
    mean_carbon_Mg <- target_traj$total_stock_Mg
    
    # Estimate uncertainty (use 15% CV as default)
    cv <- 0.15
    sd_carbon_Mg <- mean_carbon_Mg * cv
    
    # Monte Carlo simulation
    simulated_carbon_Mg <- rnorm(N_MONTE_CARLO_ITERATIONS, mean_carbon_Mg, sd_carbon_Mg)
    simulated_carbon_tCO2e <- simulated_carbon_Mg * (44/12)
    
    # Calculate probability of achieving target
    if (!is.null(baseline_scenario)) {
      baseline_at_target <- baseline_scenario$trajectory$total_stock_Mg[
        baseline_scenario$trajectory$year == TARGET_YEAR
      ] * (44/12)
      simulated_additional <- simulated_carbon_tCO2e - baseline_at_target
    } else {
      simulated_additional <- simulated_carbon_tCO2e - simulated_carbon_tCO2e[1]
    }
    
    prob_success <- mean(simulated_additional >= TARGET_CARBON_TCO2E)
    
    # Calculate risk metrics
    percentile_10 <- quantile(simulated_additional, 0.10)
    percentile_50 <- quantile(simulated_additional, 0.50)
    percentile_90 <- quantile(simulated_additional, 0.90)
    
    cat("Monte Carlo Results:\n")
    cat(sprintf("  Probability of achieving target: %.1f%%\n", prob_success * 100))
    cat(sprintf("  10th percentile (pessimistic): %.0f tCO2e\n", percentile_10))
    cat(sprintf("  50th percentile (median): %.0f tCO2e\n", percentile_50))
    cat(sprintf("  90th percentile (optimistic): %.0f tCO2e\n\n", percentile_90))
    
    # Save results
    uncertainty_results <- data.frame(
      scenario = best_scenario$name,
      target_year = TARGET_YEAR,
      target_tCO2e = TARGET_CARBON_TCO2E,
      mean_additional_tCO2e = mean(simulated_additional),
      sd_additional_tCO2e = sd(simulated_additional),
      prob_success = prob_success,
      percentile_10 = percentile_10,
      percentile_50 = percentile_50,
      percentile_90 = percentile_90
    )
    
    write_csv(uncertainty_results, file.path(output_dir, "uncertainty_analysis.csv"))
    log_message("Saved: uncertainty_analysis.csv")
    
    # Create histogram
    p4 <- ggplot(data.frame(carbon = simulated_additional), aes(x = carbon)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = TARGET_CARBON_TCO2E, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = percentile_50, color = "darkgreen", linewidth = 1) +
      annotate("text", x = TARGET_CARBON_TCO2E, y = Inf, 
               label = "Target", vjust = 2, color = "red") +
      labs(
        title = "Monte Carlo Uncertainty Analysis",
        subtitle = sprintf("Probability of achieving target: %.0f%%", prob_success * 100),
        x = "Additional Carbon (tCO2e)",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
    
    ggsave(file.path(output_dir, "uncertainty_distribution.png"),
           p4, width = 10, height = 6, dpi = 300)
    
    log_message("Saved: uncertainty_distribution.png")
  }
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("OPTIMIZATION COMPLETE\n")
cat("========================================\n\n")

cat("Analysis outputs saved to:\n")
cat(sprintf("  %s/\n\n", output_dir))

cat("Key files:\n")
cat("  📊 scenario_efficiency_comparison.csv - Cost-effectiveness ranking\n")
cat("  📊 cobenefit_scores.csv - Multi-criteria scores\n")
cat("  📊 spatial_priority_zones.csv - Priority restoration areas\n")
cat("  📊 uncertainty_analysis.csv - Risk assessment\n")
cat("  📈 cost_effectiveness_ranking.png\n")
cat("  📈 marginal_abatement_curve.png\n")
cat("  📈 cobenefit_radar.png\n")
cat("  📈 uncertainty_distribution.png\n")
cat("  🗺️  priority_zones.gpkg - Spatial priorities\n\n")

cat("Recommendations:\n")
cat("  1. Review cost-effectiveness ranking to identify best value\n")
cat("  2. Consider co-benefits alongside carbon when selecting scenario\n")
cat("  3. Use priority zones map to focus implementation efforts\n")
cat("  4. Check uncertainty analysis for project risk\n\n")

log_message("=== MODULE P5_04 COMPLETE ===")