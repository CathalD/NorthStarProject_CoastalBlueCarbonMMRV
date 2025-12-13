# ============================================================================
# BLUE CARBON PROJECT CONFIGURATION
# ============================================================================
# Edit these parameters for your specific project
# This file is sourced by analysis modules

# ============================================================================
# PROJECT METADATA (VM0033 Required)
# ============================================================================

PROJECT_NAME <- "BC_Coastal_BlueCarbon_2024"
PROJECT_SCENARIO <- "PROJECT"  # Options: BASELINE, PROJECT, CONTROL, DEGRADED
MONITORING_YEAR <- 2025

# Project location (for documentation)
PROJECT_LOCATION <- "Chemainus Estuary, British Columbia, Canada"
PROJECT_DESCRIPTION <- "Blue carbon monitoring for carbon credit development - VM0033 compliant assessment of coastal salt marsh and eelgrass restoration"

# ============================================================================
# TERRESTRIAL ECOSYSTEM STRATIFICATION (Two-Tier)
# ============================================================================
# This section supports landscape-scale MMRV across multiple ecosystem types.
# Two-tier stratification combines Ecosystem Type with Management History.
#
# FILE NAMING CONVENTION:
#   Module 05 auto-detects GEE stratum masks using this pattern:
#   "Stratum Name" → stratum_name.tif in data_raw/gee_strata/
#
# Examples:
#   "Wetland_Forested_Reference" → wetland_forested_reference.tif
#   "Forest_Deciduous_Restored"  → forest_deciduous_restored.tif
#
# CUSTOMIZATION OPTIONS:
#   1. Simple: Edit ECOSYSTEM_TYPES and MANAGEMENT_CLASSES below
#   2. Advanced: Create stratum_definitions.csv in project root for custom mappings
#
# See README section "Customizing Ecosystem Strata" for full details.
#

# Tier 1: Ecosystem Types
ECOSYSTEM_TYPES <- c("Wetland_Forested", "Wetland_Emergent", "Grassland_Native",
                     "Grassland_Restored", "Forest_Deciduous", "Forest_Coniferous")

# Tier 2: Management/Disturbance History
MANAGEMENT_CLASSES <- c("Reference", "Restored", "Prescribed_Burn",
                        "Disturbed_Recent", "Disturbed_Historic", "Agricultural_Former")

# Generate Stratification Matrix (Two-Tier Combination)
suppressPackageStartupMessages(library(dplyr))
VALID_STRATA <- expand.grid(ecosystem = ECOSYSTEM_TYPES, management = MANAGEMENT_CLASSES) %>%
  mutate(stratum = paste(ecosystem, management, sep = "_")) %>%
  pull(stratum)

# Legacy coastal strata (for backward compatibility)
LEGACY_COASTAL_STRATA <- c("IM", "NM", "MF")

# Stratum colors for plotting (auto-generated for new strata)
# Base colors by ecosystem type
ECOSYSTEM_BASE_COLORS <- c(
  "Wetland_Forested" = "#1B4F72",    # Dark blue
  "Wetland_Emergent" = "#2E86AB",    # Light blue
  "Grassland_Native" = "#A8D08D",    # Light green
  "Grassland_Restored" = "#70AD47",  # Medium green
  "Forest_Deciduous" = "#538135",    # Dark green
  "Forest_Coniferous" = "#375623"    # Very dark green
)

# Generate stratum colors based on ecosystem type
STRATUM_COLORS <- setNames(
  sapply(VALID_STRATA, function(s) {
    eco_type <- gsub("_(Reference|Restored|Prescribed_Burn|Disturbed_Recent|Disturbed_Historic|Agricultural_Former)$", "", s)
    if (eco_type %in% names(ECOSYSTEM_BASE_COLORS)) {
      ECOSYSTEM_BASE_COLORS[[eco_type]]
    } else {
      "#808080"  # Default gray
    }
  }),
  VALID_STRATA
)

# Add legacy colors for backward compatibility
STRATUM_COLORS <- c(STRATUM_COLORS, c("IM" = "#FFFF99", "NM" = "#99FF99", "MF" = "#33CC33"))

# ============================================================================
# ECOSYSTEM-SPECIFIC DEPTH CONFIGURATION
# ============================================================================
# Different ecosystems have distinct soil profile characteristics:
# - Forests: O-horizon organic layer, A-horizon, mineral horizons
# - Grasslands: Deep root penetration, potential buried horizons
# - Wetlands: Deep peat accumulation, extended profiles

# VM0033 standard depth intervals (cm) - depth midpoints for harmonization
# These correspond to VM0033 depth layers: 0-15, 15-30, 30-50, 50-100 cm
VM0033_DEPTH_MIDPOINTS <- c(7.5, 22.5, 40, 75)

# VM0033 depth intervals (cm) - for mass-weighted aggregation (coastal default)
VM0033_DEPTH_INTERVALS <- data.frame(
  depth_top = c(0, 15, 30, 50),
  depth_bottom = c(15, 30, 50, 100),
  depth_midpoint = c(7.5, 22.5, 40, 75),
  thickness_cm = c(15, 15, 20, 50)
)

# ============================================================================
# ECOSYSTEM-SPECIFIC DEPTH INTERVALS
# ============================================================================

# Forest depths - includes O-horizon organic layer, A-horizon, and mineral soils
FOREST_DEPTHS <- data.frame(
  depth_top = c(0, 0, 10, 30),
  depth_bottom = c(10, 10, 30, 100),
  layer_type = c("O_horizon", "A_horizon", "mineral", "deep_mineral")
)

# Grassland depths - moderate profile with root zone emphasis
GRASSLAND_DEPTHS <- data.frame(
  depth_top = c(0, 15, 30),
  depth_bottom = c(15, 30, 100)
)

# Wetland depths - deep peat accumulation possible
WETLAND_DEPTHS <- data.frame(
  depth_top = c(0, 15, 50),
  depth_bottom = c(15, 50, 150)  # Extended depth for peat
)

#' Get ecosystem-specific depth intervals
#'
#' @param ecosystem_type Character string indicating ecosystem type
#' @return Data frame with depth_top and depth_bottom columns
#' @examples
#' get_depth_intervals("Forest_Deciduous")
#' get_depth_intervals("Wetland_Forested")
#' get_depth_intervals("Grassland_Native")
get_depth_intervals <- function(ecosystem_type) {
  if (grepl("Forest", ecosystem_type, ignore.case = TRUE)) {
    return(FOREST_DEPTHS)
  }
  if (grepl("Grassland", ecosystem_type, ignore.case = TRUE)) {
    return(GRASSLAND_DEPTHS)
  }
  if (grepl("Wetland", ecosystem_type, ignore.case = TRUE)) {
    return(WETLAND_DEPTHS)
  }
  # Fallback to VM0033 standard intervals for coastal/unknown ecosystems
  return(VM0033_DEPTH_INTERVALS)
}

#' Get standard depth midpoints for an ecosystem type
#'
#' @param ecosystem_type Character string indicating ecosystem type
#' @return Numeric vector of depth midpoints (cm)
get_standard_depths <- function(ecosystem_type) {
  intervals <- get_depth_intervals(ecosystem_type)
  return((intervals$depth_top + intervals$depth_bottom) / 2)
}

# Standard depths for harmonization (VM0033 midpoints are default)
STANDARD_DEPTHS <- VM0033_DEPTH_MIDPOINTS

# Fine-scale depth intervals (optional, for detailed analysis)
FINE_SCALE_DEPTHS <- c(0, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)

# Maximum core depth (cm) - ecosystem-specific
MAX_CORE_DEPTH <- 100  # Default for coastal/forest
MAX_CORE_DEPTH_WETLAND <- 150  # Extended for deep peat

# Key depth intervals for reporting (cm)
REPORTING_DEPTHS <- list(
  surface = c(0, 30),      # Top 30 cm (most active layer)
  subsurface = c(30, 100)  # 30-100 cm (long-term storage)
)

# ============================================================================
# COORDINATE SYSTEMS
# ============================================================================

# Input CRS (usually WGS84 for GPS data)
INPUT_CRS <- 4326  # EPSG:4326 (WGS84)

# Processing CRS (projected, equal-area for accurate calculations)
# Change this for your region:
PROCESSING_CRS <- 3347 # Canada Albers Equal Area (good for all Canada)
# Other options:
#   - 3005:  EPSG:3005 (NAD83 / BC Albers) - OPTIMIZED FOR BC
#   - 3347: Canada Albers Equal Area (good for all Canada)
#   - 32610: WGS 84 / UTM zone 10N (BC coast - Chemainus area)
#   - 32611: WGS 84 / UTM zone 11N (BC interior)

# ============================================================================
# BULK DENSITY DEFAULTS BY STRATUM
# ============================================================================
# Use these when bulk density is not measured
# Values in g/cm³ based on literature for BC coastal ecosystems
BD_DEFAULTS <- list(
  "IM" = 0.8,              # Lower density, more organic matter
  "NM" = 0.8,                # Moderate density
  "MF" = 0.8              # Higher 
)

# ============================================================================
# QUALITY CONTROL THRESHOLDS
# ============================================================================

# Soil Organic Carbon (SOC) thresholds (g/kg)
QC_SOC_MIN <- 0      # Minimum valid SOC
QC_SOC_MAX <- 500    # Maximum valid SOC (adjust for your ecosystem)

# Bulk Density thresholds (g/cm³)
QC_BD_MIN <- 0.1     # Minimum valid bulk density
QC_BD_MAX <- 3.0     # Maximum valid bulk density

# Depth thresholds (cm)
QC_DEPTH_MIN <- 0
QC_DEPTH_MAX <- MAX_CORE_DEPTH

# Coordinate validity (decimal degrees for WGS84)
QC_LON_MIN <- -180
QC_LON_MAX <- 180
QC_LAT_MIN <- -90
QC_LAT_MAX <- 90

# ============================================================================
# VM0033 SAMPLING REQUIREMENTS
# ============================================================================

# Minimum cores per stratum (VM0033 requirement)
VM0033_MIN_CORES <- 3

# Target precision (VM0033 acceptable range: 10-20% relative error at 95% CI)
VM0033_TARGET_PRECISION <- 20  # percent

# Target CV threshold (higher CV = higher uncertainty)
VM0033_CV_THRESHOLD <- 30  # percent

# Assumed CV for sample size calculation (conservative estimate)
VM0033_ASSUMED_CV <- 30  # percent

# ============================================================================
# TEMPORAL MONITORING & ADDITIONALITY PARAMETERS
# ============================================================================

# Valid scenario types for VM0033 (expanded for restoration chronosequence)
# Core scenarios:
# - BASELINE: Pre-restoration or current degraded condition (t0)
# - DEGRADED: Heavily degraded/lost ecosystem (lower bound)
# - DISTURBED: Moderately impacted ecosystem
# - REFERENCE: Natural healthy ecosystem (upper bound target)
# - CONTROL: No-intervention control site (tracks natural variation)
# Restoration trajectory scenarios:
# - PROJECT_Y0: Immediately post-restoration
# - PROJECT_Y1: 1 year post-restoration
# - PROJECT_Y5: 5 years post-restoration (VM0033 first verification)
# - PROJECT_Y10: 10 years post-restoration (VM0033 second verification)
# - PROJECT_Y15: 15+ years post-restoration
# - PROJECT: Generic project scenario (when year not specified)
VALID_SCENARIOS <- c("BASELINE", "DEGRADED", "DISTURBED", "REFERENCE", "CONTROL",
                     "PROJECT", "PROJECT_Y0", "PROJECT_Y1", "PROJECT_Y5",
                     "PROJECT_Y10", "PROJECT_Y15", "CUSTOM")

# Scenario hierarchy for modeling (relative carbon stock levels)
# Used by Module 08A to model missing scenarios from available data
SCENARIO_CARBON_LEVELS <- c(
  DEGRADED = 1.0,
  DISTURBED = 2.0,
  BASELINE = 3.0,
  PROJECT_Y0 = 3.0,
  PROJECT_Y1 = 4.0,
  PROJECT_Y5 = 6.0,
  PROJECT_Y10 = 8.0,
  PROJECT_Y15 = 9.5,
  REFERENCE = 10.0
)

# Minimum monitoring frequency (years) - VM0033 typically requires verification every 5 years
VM0033_MONITORING_FREQUENCY <- 5

# Minimum years for temporal change analysis
MIN_YEARS_FOR_CHANGE <- 3  # At least 3 years to establish trend

# Additionality test confidence level
ADDITIONALITY_CONFIDENCE <- 0.95  # 95% CI for statistical tests

# Conservative approach for additionality calculations
ADDITIONALITY_METHOD <- "lower_bound"  # Options: "mean", "lower_bound", "conservative"
# - "mean": Use mean difference between project and baseline
# - "lower_bound": Use 95% CI lower bound of difference (most conservative, VM0033 recommended)
# - "conservative": Use mean - 1SD (moderately conservative)

# ============================================================================
# SCENARIO MODELING PARAMETERS (Module 08A)
# ============================================================================

# Enable scenario modeling (generate synthetic scenarios from reference trajectories)
SCENARIO_MODELING_ENABLED <- TRUE

# Canadian literature database for BC Coast ecosystems
CANADIAN_LITERATURE_DB <- "canadian_bluecarbon_parameters.csv"

# Scenario modeling configuration file
SCENARIO_CONFIG_FILE <- "scenario_modeling_config.csv"

# Recovery model types for reference trajectory method
# - "exponential": Fast initial recovery, slowing over time (most common)
# - "linear": Constant accumulation rate
# - "logistic": S-shaped curve with inflection point
# - "asymptotic": Approaches target asymptotically
RECOVERY_MODEL_TYPE <- "exponential"

# Uncertainty inflation for modeled scenarios (%)
# Adds additional uncertainty to account for modeling assumptions
MODELING_UNCERTAINTY_BUFFER <- 10  # percent

# Spatial resolution for modeled scenario rasters (if generating spatial outputs)
MODELED_RASTER_RESOLUTION <- 30  # meters

# ============================================================================
# BAYESIAN PRIOR PARAMETERS (Part 4 - Optional)
# ============================================================================

# Enable Bayesian workflow (requires GEE prior maps)
USE_BAYESIAN <- FALSE  # Set to TRUE to enable Part 4

# Prior data directory
BAYESIAN_PRIOR_DIR <- "data_prior"

# GEE Data Sources (BC Coast)
# SoilGrids 250m - Global soil organic carbon maps
GEE_SOILGRIDS_ASSET <- "projects/soilgrids-isric/soc_mean"
GEE_SOILGRIDS_UNCERTAINTY <- "projects/soilgrids-isric/soc_uncertainty"

# Sothe et al. 2022 - BC Forest biomass and soil carbon
# Users should input their specific GEE asset paths here:
GEE_SOTHE_FOREST_BIOMASS <- ""  # User to provide
GEE_SOTHE_SOIL_CARBON <- ""     # User to provide
GEE_SOTHE_OTHER_BIOMASS <- ""   # User to provide

# Prior resolution (will be resampled to PREDICTION_RESOLUTION)
PRIOR_RESOLUTION <- 250  # meters (SoilGrids native resolution)
PREDICTION_RESOLUTION <- 30
# Bayesian sampling design (Neyman allocation)
USE_NEYMAN_SAMPLING <- TRUE  # Enable optimal allocation based on prior uncertainty
NEYMAN_STRATA <- 3           # Number of uncertainty strata (low/med/high)
NEYMAN_BUFFER_SAMPLES <- 1.2 # Oversample by 20% to account for inaccessible locations

# Uncertainty strata thresholds (coefficient of variation %)
UNCERTAINTY_LOW_THRESHOLD <- 10    # CV < 10% = low uncertainty
UNCERTAINTY_HIGH_THRESHOLD <- 30   # CV > 30% = high uncertainty
# Medium uncertainty = between thresholds

# Bayesian posterior weighting
# How to weight prior vs field data based on sample size
BAYESIAN_WEIGHT_METHOD <- "sqrt_samples"  # Options: "sqrt_samples", "linear", "fixed"
BAYESIAN_FIXED_WEIGHT <- 0.5              # Only used if method = "fixed"
BAYESIAN_TARGET_SAMPLES <- 30             # Target sample size for full field weight

# Precision adjustment
# Inflate prior uncertainty to account for potential bias/mismatch
PRIOR_UNCERTAINTY_INFLATION <- 1.2  # Multiply prior SE by this factor (conservative)

# Information gain threshold
# Minimum posterior uncertainty reduction to declare "informative prior"
MIN_INFORMATION_GAIN_PCT <- 20  # At least 20% uncertainty reduction

# ============================================================================
# DEPTH HARMONIZATION PARAMETERS
# ============================================================================

# Interpolation method: "equal_area_spline", "smoothing_spline", "linear", "all"
INTERPOLATION_METHOD <- "equal_area_spline"  # VM0033 recommended default

# Spline smoothing parameters by core type
SPLINE_SPAR_HR <- 0.3           # Less smoothing for high-resolution cores
SPLINE_SPAR_COMPOSITE <- 0.5    # More smoothing for composite cores
SPLINE_SPAR_AUTO <- NULL        # NULL = automatic cross-validation

# Monotonicity parameters
ALLOW_DEPTH_INCREASES <- FALSE   # Allow slight SOC increases with depth (common in some ecosystems)
MAX_INCREASE_THRESHOLD <- 20    # Maximum % increase allowed between adjacent depths

# ============================================================================
# UNCERTAINTY PARAMETERS
# ============================================================================

# Confidence level for uncertainty estimation (VM0033 requires 95%)
CONFIDENCE_LEVEL <- 0.95

# Bootstrap parameters for spline uncertainty
BOOTSTRAP_ITERATIONS <- 100
BOOTSTRAP_SEED <- 42

# Cross-validation parameters
CV_FOLDS <- 3           # Number of folds for spatial CV (reduced for small datasets)
CV_SEED <- 42           # Random seed for reproducibility

# ============================================================================
# SPATIAL MODELING PARAMETERS
# ============================================================================

# Prediction resolution (meters)
KRIGING_CELL_SIZE <- 10
RF_CELL_SIZE <- 10

# Kriging parameters
KRIGING_MAX_DISTANCE <- 5000  # Maximum distance for variogram (meters)
KRIGING_CUTOFF <- NULL        # NULL = automatic
KRIGING_WIDTH <- 100          # Lag width for variogram (meters)

# Random Forest parameters
RF_NTREE <- 500              # Number of trees
RF_MTRY <- NULL              # NULL = automatic (sqrt of predictors)
RF_MIN_NODE_SIZE <- 5        # Minimum node size
RF_IMPORTANCE <- "permutation"  # Variable importance method

# ============================================================================
# AREA OF APPLICABILITY (AOA) PARAMETERS
# ============================================================================

# Enable AOA analysis (requires CAST package)
ENABLE_AOA <- TRUE

# AOA threshold (dissimilarity index)
AOA_THRESHOLD <- "default"  # "default" or numeric value

# ============================================================================
# REPORT GENERATION PARAMETERS
# ============================================================================

# Figure dimensions for saving (inches)
FIGURE_WIDTH <- 10
FIGURE_HEIGHT <- 6
FIGURE_DPI <- 300

# Table formatting
TABLE_DIGITS <- 2  # Decimal places for tables

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Convert between carbon stock units
#'
#' @param value Numeric value to convert
#' @param from Source unit (kg_m2, Mg_ha, g_kg, pct)
#' @param to Target unit
#' @return Converted value
#' @examples
#' convert_units(1, "kg_m2", "Mg_ha")  # Returns 10
#' convert_units(10, "Mg_ha", "kg_m2") # Returns 1
convert_units <- function(value, from, to) {
  conversions <- list(
    "kg_m2_to_Mg_ha" = 10,
    "Mg_ha_to_kg_m2" = 0.1,
    "g_kg_to_pct" = 0.1,
    "pct_to_g_kg" = 10
  )

  key <- paste(from, "to", to, sep = "_")
  if (key %in% names(conversions)) {
    return(value * conversions[[key]])
  } else {
    stop(sprintf("Unknown conversion: %s to %s", from, to))
  }
}

# ============================================================================
# SESSION TRACKING
# ============================================================================

# Session tracking for reproducibility and unique output naming
SESSION_START <- Sys.time()
SESSION_ID <- format(SESSION_START, "%Y%m%d_%H%M%S")

# ============================================================================
# END OF CONFIGURATION
# ============================================================================

# Print confirmation when loaded
if (interactive()) {
  cat("Blue Carbon configuration loaded ✓
")
  cat(sprintf("  Project: %s
", PROJECT_NAME))
  cat(sprintf("  Location: %s
", PROJECT_LOCATION))
  cat(sprintf("  Scenario: %s
", PROJECT_SCENARIO))
  cat(sprintf("  Monitoring year: %d
", MONITORING_YEAR))
  cat(sprintf("  Session ID: %s
", SESSION_ID))
}

