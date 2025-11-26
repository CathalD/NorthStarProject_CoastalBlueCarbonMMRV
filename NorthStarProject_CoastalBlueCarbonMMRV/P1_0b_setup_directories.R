# ============================================================================
# MODULE 00b: BLUE CARBON - DIRECTORY SETUP
# ============================================================================
# PURPOSE: Create only the directories actually used by workflow modules
# USAGE: Run this AFTER 00a_install_packages_v2.R
# ============================================================================

cat("\n========================================\n")
cat("BLUE CARBON - DIRECTORY SETUP\n")
cat("========================================\n\n")

# ============================================================================
# CREATE DIRECTORY STRUCTURE (ONLY WHAT'S ACTUALLY USED)
# ============================================================================

cat("Creating workflow directories...\n\n")

# Core directories used by ALL modules
core_dirs <- c(
  "data_raw",           # Field data input (used by all modules)
  "data_processed",     # Processed R objects (heavily used)
  "data_global",        # Large-scale datasets (Module 00d: transfer learning)
  "logs"                # Log files (all modules write here)
)

# Covariate directories (used by Module 05)
# NOTE: Subdirectories (optical/, sar/, etc.) created by user as needed
covariate_dirs <- c(
  "covariates"          # GEE exports for Random Forest
)

# Output directories (used by Modules 02-07)
output_dirs <- c(
  "outputs",
  "outputs/plots",
  "outputs/plots/by_stratum",      # Module 03: harmonization plots
  "outputs/plots/exploratory",     # Module 02: EDA figures
  "outputs/models",
  "outputs/models/kriging",        # Module 04: saved kriging models
  "outputs/models/rf",             # Module 05: saved RF models
  "outputs/models/large_scale_bluecarbon",  # Module 00d: global transfer learning models
  "outputs/predictions",
  "outputs/predictions/kriging",   # Module 04: kriging predictions
  "outputs/predictions/rf",        # Module 05: RF predictions
  "outputs/predictions/uncertainty", # Module 04: uncertainty maps
  "outputs/carbon_stocks",         # Module 06: stock calculations
  "outputs/carbon_stocks/maps",    # Module 06: stock raster maps
  "outputs/mmrv_reports"           # Module 07: verification package
)

# Diagnostic directories (used by Modules 01, 03-05)
diagnostic_dirs <- c(
  "diagnostics",
  "diagnostics/data_prep",         # Module 01: data prep diagnostics
  "diagnostics/qaqc",              # Module 01: QA/QC reports
  "diagnostics/variograms",        # Module 04: variogram plots
  "diagnostics/crossvalidation",   # Modules 04 & 05: CV metrics
  "diagnostics/large_scale_bluecarbon"  # Module 00d: transfer learning diagnostics
)

# Combine all required directories
required_dirs <- c(
  core_dirs,
  covariate_dirs,
  output_dirs,
  diagnostic_dirs
)

# Create directories
created <- 0
existed <- 0

for (dir_name in required_dirs) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
    created <- created + 1
    cat(sprintf("  ✓ Created: %s\n", dir_name))
  } else {
    existed <- existed + 1
  }
}

cat(sprintf("\nDirectories created: %d\n", created))
cat(sprintf("Already existed: %d\n", existed))
cat(sprintf("Total: %d\n\n", created + existed))

# ============================================================================
# CHECK FOR CONFIGURATION FILE
# ============================================================================

cat("========================================\n")
cat("CHECKING CONFIGURATION\n")
cat("========================================\n\n")

if (file.exists("blue_carbon_config.R")) {
  cat("✓ Configuration file exists: blue_carbon_config.R\n")

  # Test loading
  tryCatch({
    source("blue_carbon_config.R", local = TRUE)
    cat("✓ Configuration file validated\n\n")
  }, error = function(e) {
    cat("⚠️  Warning: Configuration file has errors\n")
    cat(sprintf("   Error: %s\n\n", e$message))
  })

} else {
  cat("⚠️  Configuration file not found: blue_carbon_config.R\n")
  cat("   This should already exist in your repository.\n")
  cat("   If missing, check your git repository.\n\n")
}

# ============================================================================
# CHECK FOR DATA FILES
# ============================================================================

cat("========================================\n")
cat("CHECKING FOR DATA FILES\n")
cat("========================================\n\n")

# Check for field data in both root and data_raw/
data_files_to_check <- list(
  core_locations = c("core_locations.csv", "data_raw/core_locations.csv"),
  core_samples = c("core_samples.csv", "data_raw/core_samples.csv")
)

data_status <- list()

for (file_type in names(data_files_to_check)) {
  found <- FALSE
  found_path <- NA

  for (path in data_files_to_check[[file_type]]) {
    if (file.exists(path)) {
      found <- TRUE
      found_path <- path
      break
    }
  }

  data_status[[file_type]] <- list(found = found, path = found_path)

  if (found) {
    cat(sprintf("  ✓ Found: %s\n", found_path))
  } else {
    cat(sprintf("  ✗ Missing: %s\n", data_files_to_check[[file_type]][2]))
  }
}

cat("\n")

# ============================================================================
# CHECK FOR GEE COVARIATES (OPTIONAL)
# ============================================================================

cat("========================================\n")
cat("CHECKING FOR GEE COVARIATES (OPTIONAL)\n")
cat("========================================\n\n")

cat("GEE covariates are required for Module 05 (Random Forest).\n")
cat("If you don't have them yet, Module 05 will be skipped.\n\n")

# Check for any .tif files in covariates/
covariate_files <- list.files("covariates",
                              pattern = "\\.tif$",
                              recursive = TRUE,
                              full.names = TRUE)

if (length(covariate_files) > 0) {
  cat(sprintf("✓ Found %d covariate raster(s)\n", length(covariate_files)))

  # Show subdirectories
  subdirs <- unique(dirname(covariate_files))
  subdirs <- gsub("covariates/?", "", subdirs)
  subdirs <- subdirs[subdirs != "."]

  if (length(subdirs) > 0) {
    cat("\nCovariate categories found:\n")
    for (sd in subdirs) {
      n_files <- length(list.files(file.path("covariates", sd),
                                   pattern = "\\.tif$"))
      cat(sprintf("  - %s/ (%d files)\n", sd, n_files))
    }
  }
  cat("\n")

} else {
  cat("⚠️  No GEE covariate TIF files found in covariates/\n")
  cat("\nTo use Module 05 (Random Forest), add GEE exports:\n")
  cat("  covariates/optical/  → NDVI, EVI, NDWI, etc.\n")
  cat("  covariates/sar/      → VV, VH backscatter\n")
  cat("  covariates/topography/ → Elevation, slope\n")
  cat("  covariates/coastal/  → Tidal metrics\n\n")
  cat("Alternatively, use Module 04 (Kriging) which doesn't require covariates.\n\n")
}

# ============================================================================
# ADDITIONAL DIRECTORIES CREATED BY MODULES
# ============================================================================

cat("========================================\n")
cat("NOTE: ADDITIONAL DIRECTORIES\n")
cat("========================================\n\n")

cat("Some directories are created automatically by modules:\n")
cat("  - diagnostics/variable_importance/ (Module 05)\n")
cat("  - outputs/predictions/stocks/ (Modules 04 & 05)\n")
cat("  - outputs/predictions/aoa/ (if AOA enabled)\n")
cat("  - outputs/mmrv_reports/spatial_exports/ (Module 07)\n\n")
cat("These will be created when needed.\n\n")

# ============================================================================
# SAVE SETUP SUMMARY
# ============================================================================

setup_summary <- list(
  date = Sys.Date(),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  working_directory = getwd(),
  directories_created = created,
  directories_existed = existed,
  config_exists = file.exists("blue_carbon_config.R"),
  core_locations_found = data_status$core_locations$found,
  core_samples_found = data_status$core_samples$found,
  covariates_found = length(covariate_files)
)

saveRDS(setup_summary, "data_processed/setup_summary.rds")

# Also save to logs
if (!dir.exists("logs")) dir.create("logs", showWarnings = FALSE)
log_file <- file.path("logs", paste0("setup_", Sys.Date(), ".txt"))
sink(log_file)
cat("Blue Carbon Workflow Setup Summary\n")
cat("===================================\n\n")
cat(sprintf("Date: %s\n", Sys.Date()))
cat(sprintf("R version: %s\n", setup_summary$r_version))
cat(sprintf("Working directory: %s\n\n", setup_summary$working_directory))
cat(sprintf("Directories created: %d\n", created))
cat(sprintf("Directories existed: %d\n", existed))
cat(sprintf("Config file: %s\n", ifelse(setup_summary$config_exists, "Found", "Missing")))
cat(sprintf("Core locations: %s\n", ifelse(setup_summary$core_locations_found, "Found", "Missing")))
cat(sprintf("Core samples: %s\n", ifelse(setup_summary$core_samples_found, "Found", "Missing")))
cat(sprintf("GEE covariates: %d files\n", setup_summary$covariates_found))
sink()

# ============================================================================
# FINAL SUMMARY & NEXT STEPS
# ============================================================================

cat("========================================\n")
cat("SETUP SUMMARY\n")
cat("========================================\n\n")

cat(sprintf("R version: %s\n", setup_summary$r_version))
cat(sprintf("Working directory: %s\n\n", getwd()))

cat("Status:\n")
cat(sprintf("  ✓ Directory structure: %d folders\n", created + existed))
cat(sprintf("  %s Configuration file: %s\n",
            ifelse(setup_summary$config_exists, "✓", "✗"),
            ifelse(setup_summary$config_exists, "Found", "Missing")))
cat(sprintf("  %s Field data: %s\n",
            ifelse(setup_summary$core_locations_found && setup_summary$core_samples_found, "✓", "⚠️"),
            ifelse(setup_summary$core_locations_found && setup_summary$core_samples_found,
                   "Complete", "Incomplete")))
cat(sprintf("  %s GEE covariates: %d files\n",
            ifelse(setup_summary$covariates_found > 0, "✓", "⚠️"),
            setup_summary$covariates_found))
cat("\n")

# ============================================================================
# NEXT STEPS
# ============================================================================

cat("========================================\n")
cat("NEXT STEPS\n")
cat("========================================\n\n")

ready_for_analysis <- (setup_summary$config_exists &&
                      setup_summary$core_locations_found &&
                      setup_summary$core_samples_found)

if (ready_for_analysis) {
  cat("✓✓✓ READY TO START ANALYSIS!\n\n")
  cat("Run the workflow modules in order:\n")
  cat("  1. source('01_data_prep_bluecarbon.R')\n")
  cat("  2. source('02_exploratory_analysis_bluecarbon.R')\n")
  cat("  3. source('03_depth_harmonization_bluecarbon.R')\n")
  cat("  4. source('05_raster_predictions_rf_bluecarbon.R')  # Requires GEE covariates\n")
  cat("     OR source('04_raster_predictions_kriging_bluecarbon.R')  # No covariates needed\n")
  cat("  5. source('06_carbon_stock_calculation_bluecarbon.R')\n")
  cat("  6. source('07_mmrv_reporting_bluecarbon.R')\n\n")

  if (setup_summary$covariates_found == 0) {
    cat("NOTE: No GEE covariates found.\n")
    cat("  - Use Module 04 (Kriging) instead of Module 05 (RF)\n")
    cat("  - Or add covariates to covariates/ directory first\n\n")
  }

} else {
  cat("⚠️  SETUP INCOMPLETE - Please fix the following:\n\n")

  if (!setup_summary$config_exists) {
    cat("  ✗ Configuration file missing\n")
    cat("    → Check git repository for blue_carbon_config.R\n\n")
  }

  if (!setup_summary$core_locations_found) {
    cat("  ✗ core_locations.csv missing\n")
    cat("    → Add to data_raw/ or project root\n\n")
  }

  if (!setup_summary$core_samples_found) {
    cat("  ✗ core_samples.csv missing\n")
    cat("    → Add to data_raw/ or project root\n\n")
  }

  cat("After adding missing files, run:\n")
  cat("  source('01_data_prep_bluecarbon.R')\n\n")
}

cat("Setup log saved to: ", log_file, "\n\n")
cat("Done! 🌊\n\n")
