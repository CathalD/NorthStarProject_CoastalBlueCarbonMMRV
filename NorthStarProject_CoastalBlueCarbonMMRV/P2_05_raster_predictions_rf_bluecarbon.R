# ============================================================================
# MODULE 05: BLUE CARBON RANDOM FOREST PREDICTIONS (LOCAL BASELINE)
# ============================================================================
# PURPOSE: Predict carbon stocks (kg/m²) using Random Forest with
#          stratum-aware training and spatial covariates.
#          This is the "Local Benchmark" (No Transfer Learning yet).
# ============================================================================

# --- SETUP ---
if (file.exists("blue_carbon_config.R")) {
  source("blue_carbon_config.R")
} else {
  # Fallback defaults for testing
  STANDARD_DEPTHS <- c(7.5, 22.5, 40, 75)
  CV_FOLDS <- 5
  CV_SEED <- 123
  RF_NTREE <- 500
  RF_MIN_NODE_SIZE <- 5
  RF_MTRY <- NULL
  ENABLE_AOA <- TRUE
  INPUT_CRS <- 4326 # WGS84
  VALID_STRATA <- c("High Marsh", "Low Marsh", "Mudflat") # Example
  
  # Define VM0033 Intervals for aggregation
  VM0033_DEPTH_INTERVALS <- data.frame(
    depth_top = c(0, 15, 30, 50),
    depth_bottom = c(15, 30, 50, 100),
    depth_midpoint = c(7.5, 22.5, 40, 75),
    thickness_cm = c(15, 15, 20, 50)
  )
}

# Logging
log_file <- file.path("logs", paste0("rf_predictions_", Sys.Date(), ".log"))
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

log_message <- function(msg, level = "INFO") {
  entry <- sprintf("[%s] %s: %s", format(Sys.time(), "%H:%M:%S"), level, msg)
  cat(entry, "\n")
  cat(entry, "\n", file = log_file, append = TRUE)
}

log_message("=== MODULE 05: RANDOM FOREST PREDICTIONS ===")

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(terra); library(randomForest); library(caret)
})

# Configure Terra Memory (Allow using 80% of RAM)
terraOptions(memfrac = 0.8)
log_message(sprintf("Terra memory set to %.0f%% of available RAM", 80))

# Optional Packages
has_CAST <- requireNamespace("CAST", quietly = TRUE)
if(has_CAST) {
  library(CAST)
  log_message("CAST package loaded for AOA analysis")
} else {
  log_message("CAST package not available - AOA analysis will be skipped", "WARNING")
}

# Directories
dir.create("outputs/predictions/rf", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/predictions/stocks", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models/rf", recursive = TRUE, showWarnings = FALSE)
dir.create("diagnostics/crossvalidation", recursive = TRUE, showWarnings = FALSE)
dir.create("diagnostics/varimp", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. LOAD HARMONIZED DATA
# ============================================================================
log_message("Loading harmonized data...")

if (!file.exists("data_processed/cores_harmonized_bluecarbon.rds")) {
  stop("ERROR: Harmonized data missing. Run Module 03 first.")
}

cores_harmonized <- readRDS("data_processed/cores_harmonized_bluecarbon.rds") %>%
  filter(depth_cm_midpoint %in% STANDARD_DEPTHS) %>%
  filter(qa_realistic) %>%
  mutate(depth_cm = depth_cm_midpoint)

# DATA QUALITY CHECKS
log_message(sprintf("Loaded: %d predictions from %d cores", 
                    nrow(cores_harmonized), n_distinct(cores_harmonized$core_id)))

# Check for required columns
required_cols <- c("core_id", "longitude", "latitude", "depth_cm", "carbon_stock_kg_m2", "stratum")
missing_cols <- setdiff(required_cols, names(cores_harmonized))
if (length(missing_cols) > 0) {
  stop(sprintf("ERROR: Missing required columns: %s", paste(missing_cols, collapse=", ")))
}

# Check coordinate validity
invalid_coords <- cores_harmonized %>%
  filter(is.na(longitude) | is.na(latitude) | 
           abs(longitude) > 180 | abs(latitude) > 90)
if (nrow(invalid_coords) > 0) {
  log_message(sprintf("WARNING: %d cores have invalid coordinates - removing", nrow(invalid_coords)), "WARNING")
  cores_harmonized <- cores_harmonized %>%
    filter(!is.na(longitude), !is.na(latitude),
           abs(longitude) <= 180, abs(latitude) <= 90)
}

# Check carbon stock values
carbon_range <- range(cores_harmonized$carbon_stock_kg_m2, na.rm = TRUE)
log_message(sprintf("Carbon stock range: %.2f to %.2f kg/m²", carbon_range[1], carbon_range[2]))
if (carbon_range[1] < 0 || carbon_range[2] > 100) {
  log_message("WARNING: Unusual carbon stock values detected - check data quality", "WARNING")
}

# Stratum distribution
strat_summary <- cores_harmonized %>% 
  count(stratum, depth_cm) %>%
  arrange(depth_cm, stratum)
log_message("Sample distribution by stratum and depth:")
print(strat_summary)

# ============================================================================
# 2. LOAD COVARIATES
# ============================================================================
log_message("Loading covariate rasters...")

cov_files <- list.files("covariates", pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
if (length(cov_files) == 0) stop("ERROR: No .tif files found in covariates/")

log_message(sprintf("Found %d covariate files", length(cov_files)))

# Load and check rasters
covariate_stack <- tryCatch({
  terra::rast(cov_files)
}, error = function(e) {
  stop(sprintf("ERROR loading rasters: %s", e$message))
})

# Clean names (remove .tif extension, handle spaces)
names(covariate_stack) <- tools::file_path_sans_ext(basename(cov_files)) %>% 
  make.names() %>%
  gsub("\\.\\.+", ".", .) %>%
  gsub("\\.$", "", .)

log_message(sprintf("Loaded %d covariate layers:", terra::nlyr(covariate_stack)))
log_message(paste("  -", names(covariate_stack), collapse = "\n"))

# Check raster properties
log_message(sprintf("Raster CRS: %s", terra::crs(covariate_stack, describe=TRUE)$name))
log_message(sprintf("Raster extent: xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f", 
                    terra::ext(covariate_stack)[1], terra::ext(covariate_stack)[2],
                    terra::ext(covariate_stack)[3], terra::ext(covariate_stack)[4]))
log_message(sprintf("Raster resolution: %.2f x %.2f", 
                    terra::res(covariate_stack)[1], terra::res(covariate_stack)[2]))

# Check for constant layers (no variance)
for (i in 1:terra::nlyr(covariate_stack)) {
  layer_vals <- terra::values(covariate_stack[[i]], na.rm=TRUE)
  if (length(unique(layer_vals[!is.na(layer_vals)])) == 1) {
    log_message(sprintf("WARNING: Layer '%s' has constant values - consider removing", 
                        names(covariate_stack)[i]), "WARNING")
  }
}

# ============================================================================
# 3. STRATUM HANDLING (Categorical Raster)
# ============================================================================
stratum_file <- "data_processed/stratum_raster.tif"
has_stratum_raster <- file.exists(stratum_file)

if (has_stratum_raster) {
  log_message("Loading Stratum Raster...")
  strat_r <- terra::rast(stratum_file)
  
  if (!terra::same.crs(strat_r, covariate_stack)) {
    log_message("Reprojecting stratum raster to match covariates...")
    strat_r <- terra::project(strat_r, covariate_stack, method="near")
  }
  
  if (!all(terra::res(strat_r) == terra::res(covariate_stack))) {
    log_message("Resampling stratum raster to match covariate resolution...")
    strat_r <- terra::resample(strat_r, covariate_stack, method="near")
  }
  
  names(strat_r) <- "stratum"
  strat_vals <- unique(terra::values(strat_r, na.rm=TRUE))
  log_message(sprintf("Stratum raster contains %d unique values", length(strat_vals)))
  
  covariate_stack <- c(covariate_stack, strat_r)
  log_message("Stratum raster added to stack.")
} else {
  log_message("No stratum raster found - predictions will not include stratum effects", "WARNING")
}

# ============================================================================
# 4. EXTRACT TRAINING DATA
# ============================================================================
log_message("Extracting covariates at core locations...")

n_cores_input <- nrow(cores_harmonized)
cores_vect <- terra::vect(cores_harmonized, geom=c("longitude", "latitude"), crs="EPSG:4326")

if (nrow(cores_vect) != n_cores_input) {
  log_message(sprintf("WARNING: Lost %d cores during SpatVector conversion", 
                      n_cores_input - nrow(cores_vect)), "WARNING")
}

log_message("Reprojecting core locations to match raster CRS...")
cores_vect_orig <- cores_vect
cores_vect <- terra::project(cores_vect, terra::crs(covariate_stack))

if (nrow(cores_vect) != nrow(cores_vect_orig)) {
  log_message("ERROR: Lost cores during reprojection", "ERROR")
}

raster_ext <- terra::ext(covariate_stack)
cores_ext <- terra::ext(cores_vect)

log_message(sprintf("Core extent: xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f",
                    cores_ext[1], cores_ext[2], cores_ext[3], cores_ext[4]))

cores_outside <- sum(
  terra::crds(cores_vect)[,1] < raster_ext[1] | 
    terra::crds(cores_vect)[,1] > raster_ext[2] |
    terra::crds(cores_vect)[,2] < raster_ext[3] | 
    terra::crds(cores_vect)[,2] > raster_ext[4]
)

if (cores_outside > 0) {
  log_message(sprintf("WARNING: %d cores fall outside raster extent", cores_outside), "WARNING")
}

log_message("Extracting covariate values...")
cov_vals <- terra::extract(covariate_stack, cores_vect, ID=FALSE)

if (nrow(cov_vals) != nrow(cores_harmonized)) {
  log_message("ERROR: Covariate extraction returned unexpected number of rows", "ERROR")
  log_message(sprintf("Expected: %d, Got: %d", nrow(cores_harmonized), nrow(cov_vals)), "ERROR")
}

training_data <- bind_cols(cores_harmonized, cov_vals)

n_total <- nrow(training_data)
na_counts <- colSums(is.na(training_data %>% select(all_of(names(covariate_stack)))))
if (any(na_counts > 0)) {
  log_message("NA counts by covariate:")
  for (nm in names(na_counts[na_counts > 0])) {
    log_message(sprintf("  %s: %d NAs", nm, na_counts[nm]))
  }
}

training_data <- training_data %>% drop_na(all_of(names(covariate_stack)))
n_removed <- n_total - nrow(training_data)
log_message(sprintf("Removed %d samples (%.1f%%) with NA covariates", 
                    n_removed, 100*n_removed/n_total))
log_message(sprintf("Final training dataset: %d samples", nrow(training_data)))

depth_counts <- training_data %>% count(depth_cm)
log_message("Samples per depth after NA removal:")
print(depth_counts)

if (any(depth_counts$n < 30)) {
  log_message("WARNING: Some depths have <30 samples - models may be unreliable", "WARNING")
}

# ============================================================================
# 5. MODELING LOOP (BY DEPTH)
# ============================================================================

rf_results <- list()
cv_summary <- data.frame()
varimp_all <- list()

for (d in STANDARD_DEPTHS) {
  
  log_message(sprintf("\n=== Processing Depth: %.1f cm ===", d))
  
  df_depth <- training_data %>% filter(depth_cm == d)
  
  if (nrow(df_depth) < 10) {
    log_message(sprintf("WARNING: Only %d samples - model may be unreliable (minimum 10 recommended)", 
                        nrow(df_depth)), "WARNING")
    log_message("Continuing with modeling despite low sample size...", "WARNING")
  }
  
  if (nrow(df_depth) < 3) {
    log_message(sprintf("SKIPPING: Only %d samples (absolute minimum 3 required for RF)", 
                        nrow(df_depth)), "ERROR")
    next
  }
  
  log_message(sprintf("Training samples: %d", nrow(df_depth)))
  
  exclude_cols <- c("core_id", "site_id", "depth_cm", "depth_cm_midpoint", 
                    "carbon_stock_kg_m2", "longitude", "latitude", 
                    "qa_realistic", "dataset_source")
  
  pred_cols <- setdiff(names(covariate_stack), exclude_cols)
  preds <- df_depth %>% select(all_of(pred_cols))
  resp <- df_depth$carbon_stock_kg_m2
  
  resp_stats <- summary(resp)
  log_message(sprintf("Response variable (kg/m²): Min=%.2f, Median=%.2f, Max=%.2f, SD=%.2f",
                      min(resp), median(resp), max(resp), sd(resp)))
  
  if ("stratum" %in% names(preds)) {
    log_message("Converting stratum to factor...")
    if (exists("VALID_STRATA")) {
      preds$stratum <- factor(preds$stratum, levels = VALID_STRATA)
      if (any(is.na(preds$stratum))) {
        log_message("WARNING: Some samples have stratum values not in VALID_STRATA", "WARNING")
        valid_idx <- !is.na(preds$stratum)
        preds <- preds[valid_idx, ]
        resp <- resp[valid_idx]
        df_depth <- df_depth[valid_idx, ]
        log_message(sprintf("Removed %d samples with invalid strata", sum(!valid_idx)))
        
        if (nrow(preds) < 3) {
          log_message(sprintf("SKIPPING: Only %d samples remain after stratum filtering", 
                              nrow(preds)), "ERROR")
          next
        }
      }
    } else {
      preds$stratum <- as.factor(preds$stratum)
    }
    log_message(sprintf("Stratum levels: %s", paste(levels(preds$stratum), collapse=", ")))
    log_message("Stratum distribution:")
    print(table(preds$stratum))
  }
  
  nzv <- caret::nearZeroVar(preds, saveMetrics = TRUE)
  if (any(nzv$nzv)) {
    log_message("WARNING: Near-zero variance predictors detected:", "WARNING")
    print(rownames(nzv)[nzv$nzv])
  }
  
  log_message("Training Random Forest model...")
  set.seed(CV_SEED)
  
  if (is.null(RF_MTRY)) {
    mtry_use <- max(floor(ncol(preds)/3), 1)
  } else {
    mtry_use <- RF_MTRY
  }
  
  if (nrow(df_depth) < 30) {
    ntree_use <- min(RF_NTREE, max(100, nrow(df_depth) * 10))
    nodesize_use <- max(1, floor(nrow(df_depth) / 10))
    log_message(sprintf("Adjusted parameters for small sample: ntree=%d, nodesize=%d", 
                        ntree_use, nodesize_use), "WARNING")
  } else {
    ntree_use <- RF_NTREE
    nodesize_use <- RF_MIN_NODE_SIZE
  }
  
  log_message(sprintf("RF parameters: ntree=%d, mtry=%d, nodesize=%d", 
                      ntree_use, mtry_use, nodesize_use))
  
  rf_model <- tryCatch({
    randomForest(
      x = preds, y = resp,
      ntree = ntree_use,
      mtry = mtry_use,
      importance = TRUE,
      nodesize = nodesize_use,
      keep.forest = TRUE
    )
  }, error = function(e) {
    log_message(sprintf("Random Forest training failed: %s", e$message), "ERROR")
    return(NULL)
  })
  
  if (is.null(rf_model)) {
    log_message("SKIPPING: Model training failed", "ERROR")
    next
  }
  
  log_message(sprintf("OOB R²: %.3f", 1 - rf_model$mse[ntree_use]/var(resp)))
  log_message(sprintf("OOB RMSE: %.3f kg/m²", sqrt(rf_model$mse[ntree_use])))
  
  varimp <- randomForest::importance(rf_model)  # Explicitly use randomForest namespace
  varimp_df <- data.frame(
    variable = rownames(varimp),
    IncMSE = varimp[, "%IncMSE"],
    IncNodePurity = varimp[, "IncNodePurity"],
    depth = d
  ) %>% arrange(desc(IncMSE))
  
  varimp_all[[as.character(d)]] <- varimp_df
  
  log_message("Top 5 important variables:")
  print(head(varimp_df, 5))
  
  if (nrow(df_depth) < 10) {
    log_message("SKIPPING spatial CV due to low sample size - using OOB error only", "WARNING")
    
    cv_preds <- rf_model$predicted
    rmse <- sqrt(mean((resp - cv_preds)^2))
    mae <- mean(abs(resp - cv_preds))
    r2 <- 1 - sum((resp - cv_preds)^2) / sum((resp - mean(resp))^2)
    bias <- mean(cv_preds - resp)
    
    log_message(sprintf("OOB Results: R² = %.3f, RMSE = %.3f kg/m², MAE = %.3f kg/m², Bias = %.3f kg/m²", 
                        r2, rmse, mae, bias))
    
    cv_diagnostics <- data.frame(
      core_id = df_depth$core_id,
      depth = d,
      observed = resp,
      predicted = cv_preds,
      residual = resp - cv_preds,
      fold = NA
    )
    write.csv(cv_diagnostics, 
              sprintf("diagnostics/crossvalidation/cv_predictions_%.0fcm.csv", d),
              row.names = FALSE)
    
  } else {
    log_message("Running spatial cross-validation...")
    
    set.seed(CV_SEED)
    coords <- df_depth %>% select(longitude, latitude)
    
    n_folds <- min(CV_FOLDS, max(3, floor(nrow(df_depth)/3)))
    if (n_folds < CV_FOLDS) {
      log_message(sprintf("Reducing CV folds to %d (insufficient samples)", n_folds), "WARNING")
    }
    
    folds <- kmeans(coords, centers = n_folds, nstart = 25)$cluster
    log_message(sprintf("Created %d spatial folds", n_folds))
    
    fold_counts <- table(folds)
    log_message(sprintf("Fold sizes: min=%d, max=%d", min(fold_counts), max(fold_counts)))
    
    cv_preds <- numeric(nrow(df_depth))
    
    for(k in unique(folds)) {
      test_idx <- which(folds == k)
      train_idx <- which(folds != k)
      
      log_message(sprintf("  Fold %d: Training on %d, Testing on %d", 
                          k, length(train_idx), length(test_idx)))
      
      rf_cv <- randomForest(
        x = preds[train_idx, ], y = resp[train_idx],
        ntree = ntree_use,
        mtry = mtry_use,
        nodesize = nodesize_use
      )
      cv_preds[test_idx] <- predict(rf_cv, preds[test_idx, ])
    }
    
    rmse <- sqrt(mean((resp - cv_preds)^2))
    mae <- mean(abs(resp - cv_preds))
    r2 <- 1 - sum((resp - cv_preds)^2) / sum((resp - mean(resp))^2)
    bias <- mean(cv_preds - resp)
    
    log_message(sprintf("CV Results: R² = %.3f, RMSE = %.3f kg/m², MAE = %.3f kg/m², Bias = %.3f kg/m²", 
                        r2, rmse, mae, bias))
    
    cv_diagnostics <- data.frame(
      core_id = df_depth$core_id,
      depth = d,
      observed = resp,
      predicted = cv_preds,
      residual = resp - cv_preds,
      fold = folds
    )
    write.csv(cv_diagnostics, 
              sprintf("diagnostics/crossvalidation/cv_predictions_%.0fcm.csv", d),
              row.names = FALSE)
  }
  
  cv_summary <- rbind(cv_summary, data.frame(
    depth = d, 
    n = nrow(df_depth), 
    r2 = r2, 
    rmse = rmse,
    mae = mae,
    bias = bias,
    oob_r2 = 1 - rf_model$mse[ntree_use]/var(resp),
    cv_method = ifelse(nrow(df_depth) < 10, "OOB", "Spatial_CV")
  ))
  
  log_message("Generating prediction raster...")
  
  ncells <- terra::ncell(covariate_stack)
  mem_gb <- (ncells * 8 * terra::nlyr(covariate_stack)) / 1e9
  log_message(sprintf("Estimated memory for prediction: %.2f GB", mem_gb))
  
  if (mem_gb > 4) {
    log_message("Large raster - using chunked prediction to manage memory...")
  }
  
  pred_map <- tryCatch({
    terra::predict(covariate_stack, rf_model, na.rm=TRUE, cores = 1)
  }, error = function(e) {
    log_message(sprintf("Prediction failed: %s", e$message), "ERROR")
    NULL
  })
  
  if (!is.null(pred_map)) {
    pred_range <- range(terra::values(pred_map, na.rm=TRUE), na.rm=TRUE)
    log_message(sprintf("Prediction range: %.2f to %.2f kg/m²", pred_range[1], pred_range[2]))
    
    if (pred_range[1] < 0 || pred_range[2] > 150) {
      log_message("WARNING: Predictions outside expected range - check model", "WARNING")
    }
    
    writeRaster(pred_map, 
                sprintf("outputs/predictions/rf/carbon_stock_rf_%.0fcm.tif", d), 
                overwrite=TRUE)
    log_message("Saved prediction raster")
    
    log_message("Calculating prediction uncertainty...")
    
    all_trees <- tryCatch({
      terra::predict(covariate_stack, rf_model, predict.all=TRUE, na.rm=TRUE, cores=1)
    }, error = function(e) {
      log_message(sprintf("Uncertainty calculation failed: %s", e$message), "WARNING")
      NULL
    })
    
    if (!is.null(all_trees)) {
      # FIXED: Use anonymous function to avoid name conflicts with covariate layers
      rf_se <- terra::app(all_trees, fun=function(x) sd(x, na.rm=TRUE))
      
      se_range <- range(terra::values(rf_se, na.rm=TRUE), na.rm=TRUE)
      log_message(sprintf("Uncertainty (SE) range: %.2f to %.2f kg/m²", se_range[1], se_range[2]))
      
      writeRaster(rf_se, 
                  sprintf("outputs/predictions/rf/se_rf_%.0fcm.tif", d), 
                  overwrite=TRUE)
      log_message("Saved uncertainty raster")
      
      cv_map <- (rf_se / pred_map) * 100
      writeRaster(cv_map,
                  sprintf("outputs/predictions/rf/cv_rf_%.0fcm.tif", d),
                  overwrite=TRUE)
      log_message("Saved coefficient of variation raster")
    }
    
    if (has_CAST && ENABLE_AOA) {
      log_message("Calculating Area of Applicability (AOA)...")
      tryCatch({
        train_data_aoa <- df_depth %>% select(all_of(pred_cols))
        
        aoa_result <- CAST::aoa(
          newdata = covariate_stack,
          model = rf_model,
          trainDI = NULL,
          CVtest = NULL,
          cl = NULL
        )
        
        writeRaster(aoa_result$AOA, 
                    sprintf("outputs/predictions/rf/aoa_%.0fcm.tif", d), 
                    overwrite=TRUE)
        
        writeRaster(aoa_result$DI, 
                    sprintf("outputs/predictions/rf/di_%.0fcm.tif", d), 
                    overwrite=TRUE)
        
        aoa_pct <- 100 * sum(terra::values(aoa_result$AOA, na.rm=TRUE), na.rm=TRUE) / 
          sum(!is.na(terra::values(aoa_result$AOA)))
        
        log_message(sprintf("AOA coverage: %.1f%% of study area", aoa_pct))
        
        if (aoa_pct < 50) {
          log_message("WARNING: Less than 50% of area within AOA - predictions may be unreliable", "WARNING")
        }
        
      }, error = function(e) {
        log_message(sprintf("AOA calculation failed: %s", e$message), "WARNING")
      })
    }
  }
  
  rf_results[[as.character(d)]] <- rf_model
  log_message(sprintf("=== Completed depth %.1f cm ===\n", d))
}

# ============================================================================
# 6. VM0033 AGGREGATION (Mg C / ha)
# ============================================================================
log_message("\n=== Aggregating to VM0033 Layers (Mg C/ha) ===")

KG_TO_MGHA <- 10
aggregation_summary <- data.frame()

for (i in 1:nrow(VM0033_DEPTH_INTERVALS)) {
  target_mid <- VM0033_DEPTH_INTERVALS$depth_midpoint[i]
  depth_range <- sprintf("%d-%d cm", 
                         VM0033_DEPTH_INTERVALS$depth_top[i],
                         VM0033_DEPTH_INTERVALS$depth_bottom[i])
  
  log_message(sprintf("Processing Layer %d: %s", i, depth_range))
  
  pred_file <- sprintf("outputs/predictions/rf/carbon_stock_rf_%.0fcm.tif", target_mid)
  
  if (file.exists(pred_file)) {
    r <- terra::rast(pred_file)
    stock_mgha <- r * KG_TO_MGHA
    
    log_message(sprintf("  Mean: %.2f Mg C/ha, SD: %.2f Mg C/ha", 
                        mean(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE),
                        sd(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE)))
    
    fname <- sprintf("outputs/predictions/stocks/stock_layer%d_%d-%dcm.tif", 
                     i, VM0033_DEPTH_INTERVALS$depth_top[i], 
                     VM0033_DEPTH_INTERVALS$depth_bottom[i])
    
    writeRaster(stock_mgha, fname, overwrite=TRUE)
    log_message(sprintf("Saved: %s", basename(fname)))
    
    aggregation_summary <- rbind(aggregation_summary, data.frame(
      layer = i,
      depth_range = depth_range,
      mean_mgha = mean(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE),
      sd_mgha = sd(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE),
      min_mgha = min(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE),
      max_mgha = max(terra::values(stock_mgha, na.rm=TRUE), na.rm=TRUE)
    ))
  } else {
    log_message(sprintf("WARNING: Prediction file not found for layer %d", i), "WARNING")
  }
}

# ============================================================================
# 7. SAVE OUTPUTS
# ============================================================================
log_message("\n=== Saving Model Outputs ===")

saveRDS(rf_results, "outputs/models/rf/rf_models_all_depths.rds")
log_message("Saved: RF models for all depths")

write.csv(cv_summary, "diagnostics/crossvalidation/rf_cv_results.csv", row.names=FALSE)
log_message("Saved: Cross-validation summary")

if (length(varimp_all) > 0) {
  varimp_combined <- bind_rows(varimp_all)
  write.csv(varimp_combined, "diagnostics/varimp/variable_importance_all_depths.csv", row.names=FALSE)
  log_message("Saved: Variable importance for all depths")
}

if (nrow(aggregation_summary) > 0) {
  write.csv(aggregation_summary, "outputs/predictions/stocks/vm0033_layer_summary.csv", row.names=FALSE)
  log_message("Saved: VM0033 aggregation summary")
}

# ============================================================================
# 8. FINAL SUMMARY
# ============================================================================
log_message("\n=== MODELING SUMMARY ===")
log_message(sprintf("Successfully modeled %d depths", nrow(cv_summary)))

if (nrow(cv_summary) > 0) {
  log_message("\nCross-validation performance:")
  print(cv_summary)
  
  log_message(sprintf("\nOverall mean R²: %.3f", mean(cv_summary$r2)))
  log_message(sprintf("Overall mean RMSE: %.3f kg/m²", mean(cv_summary$rmse)))
}

log_message("\n=== MODULE 05 COMPLETE ===")
