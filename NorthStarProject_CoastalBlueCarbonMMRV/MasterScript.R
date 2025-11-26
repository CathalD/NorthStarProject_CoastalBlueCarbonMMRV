
          ## =======================================##
          ## ---- PART 1 - SETUP THE WORKSPACE ---- ##
          ## =======================================##
rm(list = ls()) # - Clear existing Environment to avoid conflicts
setwd('~/Desktop/Chem_TestFiles1')

#Step 2 - Download packages "Make sure type = 'binary')

source("P1_0a_install_packages.R") # make sure packages are installed
  
# Step 3: Create directories and configure workflow
source("P1_0b_setup_directories.R")

# Step 4: Add data files to data_raw/ files found in directory
# See "Core_Locations_TEMPLATE.csv"
# See Core_Samples_TEMPLATE.csv

# Verify blue_carbon_config.R to make sure the settings for the code align with your project
# Instructions found in script - read full document

# Step 5 - Organize and prepare the data
# Input files: 
#- Core_locations.csv # This file contains core_id, lat, long etc
# - Core_samples.csv # this file contains core_id, sample_id, soc, bd, depth_top, depth_bottom 

# Output files:
# - 
# Run the code - 
source("P1_01_data_prep_bluecarbon.R")


            ## =======================================##
            ## ---- PART 2 - ANALYSIS ---- ##
            ## =======================================##

# Step 6 - Initial analysis of field data
# Input files:
# - 

# Output files:
# - 
source("P2_02_exploratory_analysis_bluecarbon.R")

# Step 7 - Depth interpolation and extrapolation modelling using equal area spline model
# Input files:
# - 

# Output files: 
# -
source("P2_3a_Depth_Harmonization_Local.R")

# Step 8 -Spatial Analysis
# Kriging
source("P2_04_raster_predictions_kriging_bluecarbon.R")

# Random forests + remote sensing
source("P2_05_raster_predictions_rf_bluecarbon.R")

# Step 9 - Results
source("P2_06_carbon_stock_calculation_bluecarbon.R")

#Step 10 - Reporting
source("P2_07_mmrv_reporting_bluecarbon.R")


          ## =========================================##
          ## ---- PART 3 (OPTIONAL) BAYESIAN ANALYSIS ##
          ## =========================================##

# clear environment
rm(list = ls())
# set bayesian to true
source("blue_carbon_config.R")
exists("USE_BAYESIAN")  # Should be TRUE
#Select save
# setup for bayesian modelling
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/00c_bayesian_prior_setup_bluecarbon.R")

#Optional - Uncertainty/prior informed sampling design
# Run prior informed sampling design using Neyman stratified sampling design (can probably run in GEE)
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/01c_bayesian_sampling_design_bluecarbon.R")
# ===== After sample collection, proceed with standard workflow ========
# Step 5: Start analysis
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/01_data_prep_bluecarbon.R")
# Step 6 - Initial analysis of data
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/02_exploratory_analysis_bluecarbon.R")
# Step 7 - Depth modelling
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/03_depth_harmonization_bluecarbon.R")
# Step 8 -Spatial Analysis
# variograms/Kriging
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/04_raster_predictions_kriging_bluecarbon.R")
# Random forests + remote sensing
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/05_raster_predictions_rf_bluecarbon.R")
# ====== end of Bayesian analysis ===== #
# With the new maps, create a Bayesian updated spatial map
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/06c_bayesian_posterior_estimation_bluecarbon.R")
# Reporting
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/07b_comprehensive_standards_report.R")


          ## ======================================================##
          ## ---- PART 4 (OPTIONAL) TRANSFER LEARNING MODELLING -- ##
          ## ======================================================##

# First we need to make sure the local harmonized and global harmonized data are ready

# if 03a wasn't run for local, then run now
# Now run module 3b for global hamronization
source("P4_3b_Depth_Harmonization_Global.R")

## Make sure the data_globel folder has file "global_cores_with_gee_covariates.csv"
## make sure in data_processed folder there is file "cores_harmonized_bluecarbon.csv"
## Make sure covariates from Global data match Local datafiles

source("P4_05_Transfer_Learning_Modelling.R")

source("P4_Transfer_Learning_Visualizations.R")



          ## ======================================================##
          ## ---- PART 5 SCENARIO BUILDING + TEMPORAL MODELLING -- ##
          ## ======================================================##


# Part 5 - Temporal and scenario modeling
#==== Set-up ======#
# Configure data
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/blue_carbon_config.R")
# Run scenario builder....
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/08a_scenario_builder_bluecarbon.R")

#==== run module 8 ===== #
# Run temporal data harmonization
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/08_temporal_data_harmonization.R")

#====== run module 9 ======#
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/09_additionality_temporal_analysis.R")

# ====== run module 10 ====
source("~/Desktop/CompositeSampling_CoastalBlueCarbon_Wrokflow-main/10_vm0033_final_verification.R")




