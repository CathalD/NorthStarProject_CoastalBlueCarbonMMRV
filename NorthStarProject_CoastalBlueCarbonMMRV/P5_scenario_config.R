# ============================================================================
# PART 5 SCENARIO MODELING CONFIGURATION
# ============================================================================
# PURPOSE: Centralized configuration for blue carbon scenario modeling
#
# WORKFLOW:
#   1. Edit settings in this file
#   2. Run P5_01 to set up spatial structure (run once)
#   3. Run P5_02 to model baseline and project scenarios
#   4. Run P5_03 to calculate VM0033 credits
#
# KEY CONCEPT:
#   Additionality = Project Scenario - Baseline Scenario
#   
#   BASELINE = What happens without your intervention
#   PROJECT = What happens with your restoration actions
#
# ============================================================================

# ============================================================================
# GENERAL SETTINGS
# ============================================================================

# Which prediction method to use as initial carbon stocks?
# Options: "rf" (Random Forest) or "kriging"
BASELINE_METHOD <- "rf"

# How to divide the landscape into management units:
# Options: "single_zone", "auto_stratum", "auto_grid", "shapefile"
ZONE_DEFINITION_METHOD <- "single_zone"

# If using "auto_grid", specify cell size in meters
GRID_CELL_SIZE <- 100

# If using "shapefile", specify path
ZONE_SHAPEFILE_PATH <- "data_raw/management_zones.shp"

# Minimum zone area (hectares) - smaller zones will be filtered out
MIN_ZONE_AREA_HA <- 0.1

# ============================================================================
# BASELINE SCENARIO SETTINGS
# ============================================================================
# The baseline represents what would happen WITHOUT your project
# This is your counterfactual for calculating additionality

BASELINE_SCENARIO_NAME <- "BASELINE_2024"

# What happens in the baseline scenario?
# Options:
#   "NO_ACTION" - No change (carbon stocks stay constant)
#   "DEGRADATION" - Ongoing degradation (carbon loss over time)
#   "NATURAL_RECOVERY" - Slow natural recovery without intervention
#   Or specify specific action(s): "GRAZE_ROTATIONAL", etc.

BASELINE_ACTION <- "NO_ACTION"

# If BASELINE_ACTION = "DEGRADATION", set degradation rate
# Negative value = carbon loss (e.g., -1.0 means losing 1 Mg C/ha/yr)
BASELINE_DEGRADATION_RATE <- -0.5  # Mg C/ha/yr

# If BASELINE_ACTION = "NATURAL_RECOVERY", set recovery rate
# Positive but slow (e.g., 0.3 = slow natural recovery at 0.3 Mg C/ha/yr)
BASELINE_NATURAL_RECOVERY_RATE <- 0.3  # Mg C/ha/yr

# ============================================================================
# PROJECT SCENARIO SETTINGS
# ============================================================================
# The project represents what happens WITH your restoration actions

PROJECT_SCENARIO_NAME <- "PROJECT_RESTORATION"

# How to assign restoration actions to zones:
# Options: "uniform" (same action everywhere) or "zone_specific"
ACTION_ASSIGNMENT_METHOD <- "uniform"

# For uniform assignment - what action(s) to apply everywhere:
# Single action:
UNIFORM_ACTION <- "RESTORE_HYDRO"

# Multiple actions (comma-separated for additive effects):
# UNIFORM_ACTION <- "RESTORE_HYDRO,PLANT_NATIVE_MARSH"

# For zone-specific assignment - different actions in different zones:
ZONE_SPECIFIC_ACTIONS <- data.frame(
  zone_id = c("ZONE_1", "ZONE_2", "ZONE_3", "SITE_ALL"),
  action_id = c(
    "RESTORE_HYDRO",
    "PLANT_NATIVE_MARSH",
    "REMOVE_INVASIVE",
    "RESTORE_HYDRO,PLANT_NATIVE_MARSH"
  ),
  stringsAsFactors = FALSE
)
# Only used if ACTION_ASSIGNMENT_METHOD = "zone_specific"

# ============================================================================
# TRAJECTORY MODELING SETTINGS
# ============================================================================

# Modeling approach for projecting future carbon stocks
# Options: "exponential", "linear", "gain_loss", "chronosequence"
TRAJECTORY_METHOD <- "exponential"

# Years to project (VM0033 verification periods)
PROJECTION_YEARS <- c(0, 5, 10, 15, 20, 25, 30)

# Maximum projection period (years)
MAX_PROJECTION_YEARS <- 30

# Default recovery parameters (if not specified in recovery_parameters.csv)
DEFAULT_RECOVERY_K <- 0.12  # Exponential rate constant
DEFAULT_RECOVERY_RATE_LINEAR <- 2.5  # Linear rate (Mg C/ha/yr)
DEFAULT_TARGET_CARBON <- 150  # Target carbon stock (Mg C/ha)

# Uncertainty buffer for projections (%)
UNCERTAINTY_BUFFER_PCT <- 15

# ============================================================================
# VM0033 CREDITING PARAMETERS
# ============================================================================

# Additionality confidence level (VM0033 requires 95%)
ADDITIONALITY_CONFIDENCE <- 0.95

# Leakage deduction (% of gross sequestration)
# VM0033 typically requires 10-15% unless site-specific study done
LEAKAGE_PERCENT <- 10

# Permanence risk buffer (% of net sequestration after leakage)
# Based on VM0033 Table 4 risk assessment
# You can either set PERMANENCE_BUFFER_PERCENT directly, or specify individual risks:

# Option 1: Set total buffer directly
PERMANENCE_BUFFER_PERCENT <- 15

# Option 2: Specify individual risk components (will be summed, max 20%)
# RISK_NATURAL_DISTURBANCE <- 5  # Fire, storm, disease (0-10%)
# RISK_MANAGEMENT_FAILURE <- 3   # Poor implementation (0-5%)
# RISK_POLITICAL_SOCIAL <- 2     # Policy changes (0-5%)
# RISK_FINANCIAL <- 5             # Project viability (0-10%)

# VM0033 verification periods (years)
VERIFICATION_YEARS <- c(5, 10, 15, 20, 25, 30)

# ============================================================================
# AVAILABLE RESTORATION ACTIONS (Reference)
# ============================================================================
# These are defined in restoration_actions.csv
#
# NO_ACTION              - No intervention - $0/ha
# RESTORE_HYDRO          - Remove dikes, restore tidal flow - $15,000/ha
# PLANT_NATIVE_MARSH     - Plant salt marsh species - $8,000/ha
# PLANT_SEAGRASS         - Transplant eelgrass - $12,000/ha
# REMOVE_INVASIVE        - Remove invasive species - $5,000/ha
# THIN_FOREST            - Thin forest canopy - $3,000/ha
# GRAZE_ROTATIONAL       - Rotational grazing - $2,000/ha
# CEASE_MOWING           - Stop mowing/haying - $500/ha
# ADD_SEDIMENT           - Beneficial sediment - $20,000/ha
# RECONNECT_CHANNEL      - Restore channels - $10,000/ha
# REMOVE_FILL            - Remove historic fill - $18,000/ha
# BREACH_ROADWAY         - Breach roadway/berm - $25,000/ha
# INSTALL_RUNNELS        - Install tidal runnels - $4,000/ha
# NUTRIENT_MANAGE        - Reduce nutrient loading - $6,000/ha
# CONTROL_EROSION        - Install erosion control - $8,000/ha
# SEED_BROADCAST         - Broadcast seeds - $2,500/ha
# THIN_INVASIVE          - Thin invasive canopy - $3,500/ha
# RESTORE_HISTORIC       - Restore historic salinity - $7,000/ha
# REMOVE_DEBRIS          - Remove marine debris - $1,500/ha
# ADAPTIVE_MANAGE        - Adaptive management - $1,000/ha
#
# COMBINE MULTIPLE: Use commas for additive effects
# Example: "RESTORE_HYDRO,PLANT_NATIVE_MARSH"
#
# SPECIAL BASELINE OPTIONS:
# DEGRADATION            - Custom degradation scenario
# NATURAL_RECOVERY       - Slow natural recovery
# ============================================================================

# ============================================================================
# ECONOMIC SETTINGS
# ============================================================================

# Discount rate for NPV calculations (decimal)
DISCOUNT_RATE <- 0.03  # 3%

# Carbon price assumption ($/tonne CO2e)
CARBON_PRICE_PER_TCO2E <- 15

# ============================================================================
# OUTPUT PREFERENCES
# ============================================================================

# Create plots?
CREATE_PLOTS <- TRUE

# Plot resolution (DPI)
PLOT_DPI <- 300

# Save rasters for each projection year?
SAVE_ANNUAL_RASTERS <- TRUE

# ============================================================================
# P5_04: OPTIMIZATION SETTINGS
# ============================================================================

# Scenarios to compare (will be auto-detected, or specify manually)
# Leave NULL to compare all scenarios in scenarios/ folder
SCENARIOS_TO_COMPARE <- NULL  
# Or specify: c("PROJECT_HYDRO", "PROJECT_COMBINED", "PROJECT_LOWCOST")

# Budget constraint for spatial prioritization (USD)
BUDGET_CONSTRAINT <- 500000  # $500,000

# Co-benefit weights for multi-criteria analysis (must sum to 1.0)
WEIGHT_CARBON <- 0.40
WEIGHT_BIODIVERSITY <- 0.20
WEIGHT_FLOOD_PROTECTION <- 0.20
WEIGHT_WATER_QUALITY <- 0.20

# Uncertainty analysis settings
N_MONTE_CARLO_ITERATIONS <- 1000  # Number of simulations
TARGET_CARBON_TCO2E <- 1000  # Target carbon sequestration (tCO2e)
TARGET_YEAR <- 10  # Target year to achieve goal

# Co-benefit spatial layers (optional - if not provided, placeholder scores used)
COBENEFIT_BIODIVERSITY_RASTER <- NULL  # Path to habitat suitability raster
COBENEFIT_FLOOD_RASTER <- NULL  # Path to flood protection raster
COBENEFIT_WATER_QUALITY_RASTER <- NULL  # Path to water quality raster

# ============================================================================
# VALIDATION
# ============================================================================

if (!TRAJECTORY_METHOD %in% c("exponential", "linear", "gain_loss", "chronosequence")) {
  stop("TRAJECTORY_METHOD must be: exponential, linear, gain_loss, or chronosequence")
}

if (!ZONE_DEFINITION_METHOD %in% c("single_zone", "auto_stratum", "auto_grid", "shapefile")) {
  stop("ZONE_DEFINITION_METHOD must be: single_zone, auto_stratum, auto_grid, or shapefile")
}

if (!ACTION_ASSIGNMENT_METHOD %in% c("uniform", "zone_specific")) {
  stop("ACTION_ASSIGNMENT_METHOD must be: uniform or zone_specific")
}

if (!BASELINE_METHOD %in% c("rf", "kriging")) {
  stop("BASELINE_METHOD must be: rf or kriging")
}

if (!BASELINE_ACTION %in% c("NO_ACTION", "DEGRADATION", "NATURAL_RECOVERY")) {
  # Check if it's a valid action ID from restoration_actions.csv
  if (file.exists("restoration_actions.csv")) {
    valid_actions <- read.csv("restoration_actions.csv")$action_id
    if (!BASELINE_ACTION %in% valid_actions) {
      warning(sprintf("BASELINE_ACTION '%s' not found in restoration_actions.csv. Will use NO_ACTION.", 
                      BASELINE_ACTION))
      BASELINE_ACTION <- "NO_ACTION"
    }
  }
}

# Print configuration summary
cat("\n========================================\n")
cat("PART 5 CONFIGURATION LOADED\n")
cat("========================================\n\n")
cat("BASELINE Scenario:\n")
cat(sprintf("  Name: %s\n", BASELINE_SCENARIO_NAME))
cat(sprintf("  Action: %s\n", BASELINE_ACTION))
if (BASELINE_ACTION == "DEGRADATION") {
  cat(sprintf("  Degradation rate: %.2f Mg C/ha/yr\n", BASELINE_DEGRADATION_RATE))
} else if (BASELINE_ACTION == "NATURAL_RECOVERY") {
  cat(sprintf("  Recovery rate: %.2f Mg C/ha/yr\n", BASELINE_NATURAL_RECOVERY_RATE))
}
cat("\n")
cat("PROJECT Scenario:\n")
cat(sprintf("  Name: %s\n", PROJECT_SCENARIO_NAME))
cat(sprintf("  Action assignment: %s\n", ACTION_ASSIGNMENT_METHOD))
if (ACTION_ASSIGNMENT_METHOD == "uniform") {
  cat(sprintf("  Action(s): %s\n", UNIFORM_ACTION))
}
cat("\n")
cat("Trajectory Settings:\n")
cat(sprintf("  Method: %s\n", TRAJECTORY_METHOD))
cat(sprintf("  Projection period: %d years\n", MAX_PROJECTION_YEARS))
cat("\n")
cat("VM0033 Crediting:\n")
cat(sprintf("  Leakage deduction: %d%%\n", LEAKAGE_PERCENT))
cat(sprintf("  Permanence buffer: %d%%\n", PERMANENCE_BUFFER_PERCENT))
cat("\n")
cat("Ready to run:\n")
cat("  1. P5_01_scenario_definition_spatial.R (run once)\n")
cat("  2. P5_02_trajectory_modeling.R (creates baseline AND project)\n")
cat("  3. P5_03_vm0033_crediting.R (calculates additionality)\n")
cat("\n")