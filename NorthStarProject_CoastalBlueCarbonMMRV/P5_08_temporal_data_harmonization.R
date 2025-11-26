# ============================================================================
# MODULE 08: TEMPORAL DATA HARMONIZATION
# ============================================================================
# PURPOSE: Load and align carbon stock outputs from multiple scenarios/years
# INPUTS:
#   - outputs/carbon_stocks/carbon_stocks_by_stratum.csv (from multiple runs)
#   - outputs/carbon_stocks/maps/*.tif (from multiple runs)
# OUTPUTS:
#   - data_temporal/carbon_stocks_aligned.rds
#   - data_temporal/temporal_metadata.csv
#   - data_temporal/stratum_coverage.csv
# ============================================================================
# IMPORTANT: This is PART 3 of the workflow
# Run Module 01-07 separately for each scenario/year BEFORE running this module
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
required_vars <- c("VALID_SCENARIOS", "MIN_YEARS_FOR_CHANGE",
                   "ADDITIONALITY_CONFIDENCE", "PROCESSING_CRS")
missing_vars <- required_vars[!sapply(required_vars, exists)]
if (length(missing_vars) > 0) {
  stop(sprintf("Configuration error: Missing required variables: %s\nPlease check blue_carbon_config.R",
               paste(missing_vars, collapse=", ")))
}

# Initialize logging
log_file <- file.path("logs", paste0("temporal_harmonization_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 08: TEMPORAL DATA HARMONIZATION ===")

# Load packages
suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
})

log_message("Packages loaded successfully")

# Create output directory
dir.create("data_temporal", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# DISCOVER AVAILABLE SCENARIOS AND YEARS
# ============================================================================

log_message("Scanning for available carbon stock datasets...")

# Look for scenario subdirectories in outputs/carbon_stocks/
# Structure: outputs/carbon_stocks/SCENARIO_YEAR/carbon_stocks_by_stratum_rf.csv
carbon_stock_dir <- "outputs/carbon_stocks"

if (!dir.exists(carbon_stock_dir)) {
  stop(sprintf("Carbon stocks directory not found: %s\nPlease run Module 01-07 or Module 08A first to generate carbon stock outputs.",
               carbon_stock_dir))
}

# Find all subdirectories (scenario folders)
scenario_dirs <- list.dirs(carbon_stock_dir, recursive = FALSE, full.names = TRUE)

if (length(scenario_dirs) == 0) {
  stop(sprintf("No scenario folders found in %s\nPlease run Module 01-07 or Module 08A to generate scenarios.",
               carbon_stock_dir))
}

log_message(sprintf("Found %d scenario folder(s):", length(scenario_dirs)))

# Find carbon_stocks_by_stratum_rf_wide.csv in each folder
# Wide format contains all 4 VM0033 intervals as columns
csv_files <- c()
for (dir in scenario_dirs) {
  csv_file <- file.path(dir, "carbon_stocks_by_stratum_rf_wide.csv")
  if (file.exists(csv_file)) {
    csv_files <- c(csv_files, csv_file)
    log_message(sprintf("  - %s", basename(dir)))
  }
}

if (length(csv_files) == 0) {
  stop(sprintf("No carbon_stocks_by_stratum_rf_wide.csv files found in scenario folders.\nPlease check outputs/carbon_stocks/*/\nMake sure Module 06 has been run to generate wide-format outputs."))
}

log_message(sprintf("Found %d carbon stock dataset(s) total", length(csv_files)))

# ============================================================================
# EXTRACT SCENARIO AND YEAR FROM DIRECTORY NAMES
# ============================================================================

cat("\n========================================\n")
cat("TEMPORAL DATASET CONFIGURATION\n")
cat("========================================\n\n")

cat("Extracting scenario and year from folder names...\n")
cat("Expected format: SCENARIO_YEAR (e.g., PROJECT_Y5_2024, BASELINE_2024)\n\n")

# Extract scenario and year from directory names
temporal_metadata <- data.frame(
  file_path = csv_files,
  folder_name = basename(dirname(csv_files))
) %>%
  mutate(
    # Try to extract scenario and year from folder name
    # Pattern: SCENARIO_YEAR or SCENARIO (will use config year)
    scenario = str_extract(folder_name, "^[A-Z_0-9]+(?=_[0-9]{4}$|$)"),
    year = as.integer(str_extract(folder_name, "[0-9]{4}$"))
  )

# Handle folders without year suffix
no_year_idx <- is.na(temporal_metadata$year)
if (any(no_year_idx)) {
  log_message(sprintf("Using MONITORING_YEAR (%d) for folders without year suffix:", MONITORING_YEAR), "INFO")
  for (i in which(no_year_idx)) {
    log_message(sprintf("  - %s", temporal_metadata$folder_name[i]), "INFO")
    temporal_metadata$year[i] <- MONITORING_YEAR
  }
}

# Handle folders without clear scenario
no_scenario_idx <- is.na(temporal_metadata$scenario)
if (any(no_scenario_idx)) {
  log_message(sprintf("Using PROJECT_SCENARIO (%s) for folders without clear scenario:", PROJECT_SCENARIO), "WARNING")
  for (i in which(no_scenario_idx)) {
    log_message(sprintf("  - %s", temporal_metadata$folder_name[i]), "WARNING")
    temporal_metadata$scenario[i] <- PROJECT_SCENARIO
  }
}

# Display detected scenarios
cat("\nDetected scenarios and years:\n")
print(temporal_metadata %>% select(folder_name, scenario, year))
cat("\n")

# Validate scenarios
invalid_scenarios <- setdiff(unique(temporal_metadata$scenario), VALID_SCENARIOS)
if (length(invalid_scenarios) > 0) {
  stop(sprintf("Invalid scenario types detected: %s\nValid options: %s",
               paste(invalid_scenarios, collapse = ", "),
               paste(VALID_SCENARIOS, collapse = ", ")))
}

# ============================================================================
# LOAD AND MERGE CARBON STOCK DATA
# ============================================================================

log_message("Loading carbon stock datasets...")

carbon_stocks_all <- list()

for (i in 1:nrow(temporal_metadata)) {
  scenario <- temporal_metadata$scenario[i]
  year <- temporal_metadata$year[i]
  file_path <- temporal_metadata$file_path[i]

  log_message(sprintf("  Loading: %s (%d)", scenario, year))

  # Load CSV
  carbon_stocks <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      scenario = scenario,
      year = year,
      dataset_id = sprintf("%s_%d", scenario, year)
    )

  carbon_stocks_all[[i]] <- carbon_stocks
}

# Combine all datasets
carbon_stocks_combined <- bind_rows(carbon_stocks_all)

log_message(sprintf("Loaded %d datasets with %d total rows",
                   length(carbon_stocks_all),
                   nrow(carbon_stocks_combined)))

# ============================================================================
# VALIDATE STRATUM COVERAGE
# ============================================================================

log_message("Validating stratum coverage across scenarios...")

stratum_coverage <- carbon_stocks_combined %>%
  group_by(scenario, year, stratum) %>%
  summarise(
    has_data = n() > 0,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(scenario, year),
    values_from = has_data,
    values_fill = FALSE
  )

cat("\n========================================\n")
cat("STRATUM COVERAGE MATRIX\n")
cat("========================================\n\n")
print(as.data.frame(stratum_coverage))
cat("\n")

# Check for overlapping strata across scenarios
all_combos <- temporal_metadata %>%
  select(scenario, year) %>%
  distinct()

if (nrow(all_combos) > 1) {
  for (i in 1:(nrow(all_combos) - 1)) {
    for (j in (i + 1):nrow(all_combos)) {
      scenario1 <- all_combos$scenario[i]
      year1 <- all_combos$year[i]
      scenario2 <- all_combos$scenario[j]
      year2 <- all_combos$year[j]

      strata1 <- carbon_stocks_combined %>%
        filter(scenario == scenario1, year == year1) %>%
        pull(stratum) %>%
        unique()

      strata2 <- carbon_stocks_combined %>%
        filter(scenario == scenario2, year == year2) %>%
        pull(stratum) %>%
        unique()

      overlap <- intersect(strata1, strata2)

      if (length(overlap) > 0) {
        log_message(sprintf("✓ %s_%d vs %s_%d: %d overlapping strata",
                           scenario1, year1, scenario2, year2, length(overlap)))
      } else {
        log_message(sprintf("WARNING: %s_%d vs %s_%d: NO overlapping strata",
                           scenario1, year1, scenario2, year2), "WARNING")
      }
    }
  }
}

# ============================================================================
# LOAD AND ALIGN RASTER DATA
# ============================================================================

log_message("\nChecking for carbon stock rasters...")

# For each scenario/year, check if rasters exist
rasters_list <- list()
any_rasters_found <- FALSE

for (i in 1:nrow(temporal_metadata)) {
  scenario <- temporal_metadata$scenario[i]
  year <- temporal_metadata$year[i]
  dataset_id <- sprintf("%s_%d", scenario, year)
  folder_name <- temporal_metadata$folder_name[i]

  # Check for maps subdirectory in scenario folder
  scenario_maps_dir <- file.path(carbon_stock_dir, folder_name, "maps")

  if (!dir.exists(scenario_maps_dir)) {
    log_message(sprintf("  No maps directory for: %s", dataset_id), "INFO")
    rasters_list[[dataset_id]] <- list()
    next
  }

  log_message(sprintf("  Loading rasters for: %s", dataset_id))
  any_rasters_found <- TRUE

  # Key rasters to load - all 4 VM0033 intervals plus total
  # VM0033 intervals: 0-15cm, 15-30cm, 30-50cm, 50-100cm
  raster_patterns <- c(
    interval_0_15_mean = "carbon_stock_0-15cm_mean\\.tif$",
    interval_15_30_mean = "carbon_stock_15-30cm_mean\\.tif$",
    interval_30_50_mean = "carbon_stock_30-50cm_mean\\.tif$",
    interval_50_100_mean = "carbon_stock_50-100cm_mean\\.tif$",
    total_mean = "carbon_stock_0-100cm total_mean\\.tif$",
    interval_0_15_conservative = "carbon_stock_0-15cm_conservative\\.tif$",
    interval_15_30_conservative = "carbon_stock_15-30cm_conservative\\.tif$",
    interval_30_50_conservative = "carbon_stock_30-50cm_conservative\\.tif$",
    interval_50_100_conservative = "carbon_stock_50-100cm_conservative\\.tif$",
    total_conservative = "carbon_stock_0-100cm total_conservative\\.tif$"
  )

  dataset_rasters <- list()

  for (raster_name in names(raster_patterns)) {
    pattern <- raster_patterns[raster_name]
    raster_file <- list.files(scenario_maps_dir, pattern = pattern, full.names = TRUE)

    if (length(raster_file) > 0) {
      r <- rast(raster_file[1])
      dataset_rasters[[raster_name]] <- r
      log_message(sprintf("    Loaded: %s", raster_name))
    }
  }

  rasters_list[[dataset_id]] <- dataset_rasters
}

if (!any_rasters_found) {
  log_message("No rasters found - proceeding with CSV data only", "WARNING")
  rasters_aligned <- NULL
} else {

  # Find first non-empty raster list for reference
  reference_raster <- NULL
  for (dataset_id in names(rasters_list)) {
    if (length(rasters_list[[dataset_id]]) > 0) {
      reference_raster <- rasters_list[[dataset_id]][[1]]
      log_message(sprintf("Using %s as spatial reference", dataset_id))
      break
    }
  }

  if (is.null(reference_raster)) {
    log_message("No valid rasters found for spatial alignment", "WARNING")
    rasters_aligned <- NULL
  } else {
    # Check spatial alignment
    log_message("Checking spatial alignment...")
    all_aligned <- TRUE

    for (dataset_id in names(rasters_list)) {
      if (length(rasters_list[[dataset_id]]) > 0) {
        test_raster <- rasters_list[[dataset_id]][[1]]

        if (!compareGeom(reference_raster, test_raster, stopOnError = FALSE)) {
          log_message(sprintf("WARNING: Rasters for %s not aligned with reference",
                             dataset_id), "WARNING")
          all_aligned <- FALSE
        }
      }
    }

    if (all_aligned) {
      log_message("✓ All rasters are spatially aligned")
      rasters_aligned <- rasters_list
    } else {
      log_message("Resampling rasters to reference grid...", "WARNING")

      # Resample all to reference
      for (dataset_id in names(rasters_list)) {
        if (length(rasters_list[[dataset_id]]) > 0) {
          for (raster_name in names(rasters_list[[dataset_id]])) {
            if (!is.null(rasters_list[[dataset_id]][[raster_name]])) {
              rasters_list[[dataset_id]][[raster_name]] <- resample(
                rasters_list[[dataset_id]][[raster_name]],
                reference_raster,
                method = "bilinear"
              )
            }
          }
        }
      }

      log_message("✓ Resampling complete")
      rasters_aligned <- rasters_list
    }
  }
}

# ============================================================================
# SAVE HARMONIZED TEMPORAL DATASET
# ============================================================================

log_message("\nSaving harmonized temporal dataset...")

# Create combined temporal data structure
temporal_data <- list(
  metadata = temporal_metadata,
  carbon_stocks = carbon_stocks_combined,
  stratum_coverage = stratum_coverage,
  rasters = rasters_aligned
)

# Save as RDS
saveRDS(temporal_data, "data_temporal/carbon_stocks_aligned.rds")
log_message("Saved: data_temporal/carbon_stocks_aligned.rds")

# Save metadata CSV
write_csv(temporal_metadata, "data_temporal/temporal_metadata.csv")
log_message("Saved: data_temporal/temporal_metadata.csv")

# Save stratum coverage
write_csv(stratum_coverage, "data_temporal/stratum_coverage.csv")
log_message("Saved: data_temporal/stratum_coverage.csv")

# ============================================================================
# SUMMARY REPORT
# ============================================================================

cat("\n========================================\n")
cat("TEMPORAL HARMONIZATION COMPLETE\n")
cat("========================================\n\n")

cat(sprintf("Total datasets: %d\n", nrow(temporal_metadata)))
cat(sprintf("Scenarios: %s\n", paste(unique(temporal_metadata$scenario), collapse = ", ")))
cat(sprintf("Years: %s\n", paste(sort(unique(temporal_metadata$year)), collapse = ", ")))
cat(sprintf("Total strata: %d\n", length(unique(carbon_stocks_combined$stratum))))
cat("\n")

# Check what analyses are possible
has_baseline <- "BASELINE" %in% temporal_metadata$scenario
has_project <- "PROJECT" %in% temporal_metadata$scenario
n_years <- length(unique(temporal_metadata$year))

cat("Possible analyses:\n")
if (has_baseline && has_project) {
  cat("  ✓ Additionality analysis (baseline vs project comparison)\n")
} else {
  cat("  ✗ Additionality analysis (requires BASELINE and PROJECT scenarios)\n")
}

if (n_years >= 2) {
  cat(sprintf("  ✓ Temporal change analysis (%d time points)\n", n_years))

  if (n_years >= MIN_YEARS_FOR_CHANGE) {
    cat(sprintf("  ✓ Trend analysis (>= %d years)\n", MIN_YEARS_FOR_CHANGE))
  }
} else {
  cat("  ✗ Temporal change analysis (requires multiple years)\n")
}

cat("\n")
cat("Next step: Run Module 09 for additionality and temporal change analysis\n")
cat("\n")

log_message("=== MODULE 08 COMPLETE ===")
