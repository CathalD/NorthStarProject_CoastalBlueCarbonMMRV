# Blue Carbon MMRV Workflow
 
**Date:** November 2024  
**Language:** R  
**Platform:** RStudio  
-----
## Table of Contents

1. [Overview](#overview)
2. [Scientific Foundation](#scientific-foundation)
3. [Workflow Architecture](#workflow-architecture)
4. [Required Inputs](#required-inputs)
5. [Expected Outputs](#expected-outputs)
6. [Implementation Guide](#implementation-guide)
7. [Standards Compliance](#standards-compliance)
8. [References](#references)

---

## Overview

This workflow provides a **Measurment, Monitoring, Reporting, and Verification (MMRV)** system for coastal blue carbon ecosystems (saltmarsh, seagrass and eelgrass) compliant with international carbon crediting and MMRV standards. The system processes field sediment core data through spatial modeling to produce verification-ready carbon stock estimates with quantified uncertainty.

### Key Features

- **VM0033 Compliant:** Follows Verra's Methodology for Tidal Wetland and Seagrass Restoration (VM0033 v2.1)
- ** Includes guidance from ORRAA, IPCC Wetlands Supplement, Canadian Blue Carbon Network
- **Spatial Modeling:** Kriging (geostatistical) and Random Forest (machine learning) approaches
- **Uncertainty Quantification:** Conservative estimates using 95% confidence intervals
- **Bayesian Integration:** Optional incorporation of global prior knowledge
- **Transfer Learning:** Leverage global datasets when local samples are limited

### Scientific Approach

The workflow implements peer-reviewed methods for blue carbon assessment:

1. **Depth Harmonization:** Equal-area spline functions (Bishop et al. 1999; Malone et al. 2009)
2. **Spatial Interpolation:** Universal kriging with variogram modeling (Goovaerts 1997)
3. **Machine Learning:** Random Forest with Area of Applicability (Meyer & Pebesma 2021)
4. **Bayesian Updating:** Prior knowledge integration (Wadoux et al. 2021)
5. **Carbon Stock Calculation:** VM0033 methodology with conservative uncertainty bounds
---
## Scientific Foundation

### Primary Methodologies

#### VM0033 Methodology (Verra 2015, 2022)
The workflow follows VM0033 v2.0 "Methodology for Tidal Wetland and Seagrass Restoration" which specifies:

- **Standard Depth Intervals:** 0-15, 15-30, 30-50, 50-100 cm below surface
- **Midpoint Reporting:** 7.5, 22.5, 40, 75 cm for spatial predictions
- **Sample Size Requirements:** Minimum n=3 cores per stratum, with statistical precision targets
- **Conservative Crediting:** Lower 95% confidence bound for carbon stock estimates
- **Additionality Demonstration:** Statistical significance testing (α = 0.05)
- **Deductions:** Leakage (typically 10%) and permanence risk buffer (10-20%)

**References:**
- Verra (2022). VM0033 Methodology for Tidal Wetland and Seagrass Restoration, v2.0. https://verra.org/methodologies/vm0033-methodology-for-tidal-wetland-and-seagrass-restoration-v2-0/
- Emmer et al. (2015). "The Blue Carbon Guidebook: Guidance for Protecting Coastal Carbon Sinks." UNEP, CIFOR, UNESCO-IOC.

#### Depth Harmonization

**Equal-Area Spline Functions** (Bishop et al. 1999; Malone et al. 2009):
- Fits continuous depth functions to irregular sampling intervals
- Preserves mass balance (∫SOC·BD·dz constant)
- Prevents artifacts from interval averaging
- Enables standardized depth reporting for VM0033 compliance

**Hybrid Approach** (implemented in Module P2_03):
- Equal-area splines for profiles with ≥3 samples
- Mass-preserving linear interpolation for 2-sample profiles
- Exponential decay extrapolation beyond deepest sample (Holmquist et al. 2018)

**References:**
- Bishop, T.F.A., McBratney, A.B., & Laslett, G.M. (1999). "Modelling soil attribute depth functions with equal-area quadratic smoothing splines." Geoderma, 91(1-2), 27-45. https://doi.org/10.1016/S0016-7061(99)00003-8
- Malone, B.P., McBratney, A.B., Minasny, B., & Laslett, G.M. (2009). "Mapping continuous depth functions of soil carbon storage and available water capacity." Geoderma, 154(1-2), 138-152. https://doi.org/10.1016/j.geoderma.2009.10.007
- Holmquist, J.R., Windham-Myers, L., Blaauw, M., et al. (2018). "Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States." Scientific Reports, 8(1), 9478. https://doi.org/10.1038/s41598-018-26948-7

#### Spatial Interpolation - Kriging

**Universal Kriging** (Goovaerts 1997; Webster & Oliver 2007):
- Geostatistical method accounting for spatial autocorrelation
- Variogram modeling captures spatial dependency structure
- Best Linear Unbiased Prediction (BLUP) with uncertainty estimates
- Suitable when covariates unavailable or sample size limited

**Implementation** (Module P2_04):
- Automated variogram fitting (exponential, spherical, Gaussian models)
- Cross-validation for model selection
- Prediction standard errors for uncertainty quantification

**References:**
- Goovaerts, P. (1997). Geostatistics for Natural Resources Evaluation. Oxford University Press.
- Webster, R., & Oliver, M.A. (2007). Geostatistics for Environmental Scientists, 2nd Edition. Wiley. https://doi.org/10.1002/9780470517277

#### Spatial Interpolation - Random Forest

**Machine Learning Approach** (Breiman 2001; Hengl et al. 2018):
- Ensemble decision tree method using remote sensing covariates
- Handles non-linear relationships and variable interactions
- Feature importance quantification
- Area of Applicability assessment (Meyer & Pebesma 2021)

**Implementation** (Module P2_05):
- Integrates Landsat, Sentinel-1 SAR, topographic data
- Ranger package for efficient random forest
- CAST package for spatial cross-validation and AOA
- Uncertainty via quantile regression forests

**References:**
- Breiman, L. (2001). "Random Forests." Machine Learning, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324
- Hengl, T., Nussbaum, M., Wright, M.N., Heuvelink, G.B.M., & Gräler, B. (2018). "Random forest as a generic framework for predictive modeling of spatial and spatio-temporal variables." PeerJ, 6, e5518. https://doi.org/10.7717/peerj.5518
- Meyer, H., & Pebesma, E. (2021). "Predicting into unknown space? Estimating the area of applicability of spatial prediction models." Methods in Ecology and Evolution, 12(9), 1620-1633. https://doi.org/10.1111/2041-210X.13650

#### Bayesian Integration (Optional)

**Prior Knowledge Transfer** (Wadoux et al. 2021):
- Combines site-specific likelihood with global prior distribution
- Reduces uncertainty when local samples limited
- Weight calibration via empirical Bayes or cross-validation

**Implementation** (Part 3 modules):
- Global database as informative prior (Holmquist et al. 2018; Macreadie et al. 2019)
- Bayesian updating via conjugate Normal-Normal framework
- Posterior mean and credible intervals for conservative estimates

**References:**
- Wadoux, A.M.J.-C., Brus, D.J., & Heuvelink, G.B.M. (2021). "Accounting for non-stationary variance in geostatistical mapping of soil properties." Geoderma, 324, 114138. https://doi.org/10.1016/j.geoderma.2018.03.010
- Holmquist, J.R., et al. (2018). "Coastal and Marine Ecological Classification Standard." NOAA Technical Memorandum NOS NCCOS 258.

#### Transfer Learning (Optional)

**Global-to-Local Knowledge Transfer** (Module P4_05):
- Instance weighting (Wadoux et al. 2021) to identify relevant global samples
- Hierarchical modeling across depth intervals
- Bias correction for local conditions

**Global Database:**
- Coastal Carbon Network synthesis (Holmquist et al. 2018)
- Smithsonian MarineGEO global cores
- Harmonized to VM0033 depths

**References:**
- Wadoux, A.M.J.-C., Samuel-Rosa, A., Poggio, L., & Mulder, V.L. (2021). "A note on knowledge discovery and machine learning in digital soil mapping." European Journal of Soil Science, 71(2), 133-136. https://doi.org/10.1111/ejss.12909

#### Carbon Stock Calculation

**Methodology** (Howard et al. 2014; IPCC 2014):

```
Carbon Stock (kg C/m²) = ∫[SOC (g/kg) × BD (g/cm³) × 10] dz

Where:
  SOC = Soil organic carbon concentration (g C / kg dry soil)
  BD  = Bulk density (g dry soil / cm³)
  dz  = Depth increment (cm)
  10  = Unit conversion factor
```

**Bulk Density Handling:**
- Measured BD used when available
- Ecosystem-specific defaults (Morris et al. 2016) when missing:
  - Saltmarsh (EM): 0.52 g/cm³
  - Seagrass (SG): 0.89 g/cm³
  - Mangrove (FL): 0.38 g/cm³

**References:**
- Howard, J., Hoyt, S., Isensee, K., Pidgeon, E., & Telszewski, M. (2014). Coastal Blue Carbon: Methods for Assessing Carbon Stocks and Emissions Factors in Mangroves, Tidal Salt Marshes, and Seagrass Meadows. Conservation International, Intergovernmental Oceanographic Commission of UNESCO, International Union for Conservation of Nature. Arlington, Virginia, USA.
- IPCC (2014). 2013 Supplement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories: Wetlands. Hiraishi, T., Krug, T., Tanabe, K., et al. (eds). IPCC, Switzerland.
- Morris, J.T., Barber, D.C., Callaway, J.C., et al. (2016). "Contributions of organic and inorganic matter to sediment volume and accretion in tidal wetlands at steady state." Earth's Future, 4(4), 110-121. https://doi.org/10.1002/2015EF000334

#### Uncertainty Quantification

**Conservative Approach** (IPCC 2006, VM0033):
- Propagation of measurement, spatial, and model uncertainties
- Lower 95% confidence bound for crediting
- Monte Carlo simulation when analytical propagation infeasible

**Sources of Uncertainty:**
1. **Measurement:** Lab analytical precision (typically 2-5% for SOC)
2. **Spatial:** Kriging variance or RF quantile spread
3. **Model:** Cross-validation RMSE, variogram uncertainty
4. **Temporal:** Inter-annual variability (if multi-year data)

**References:**
- IPCC (2006). IPCC Guidelines for National Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land Use. IGES, Japan.

---

## Workflow Architecture

The workflow is organized into modular parts with sequential dependencies:

### Part 1: Setup and Data Preparation

**Module P1_0a:** Package Installation
- Installs all required R packages
- Checks dependencies and versions

**Module P1_0b:** Directory Structure
- Creates standardized folder hierarchy
- Validates configuration file
- Checks for required input data

**Module P1_01:** Data Preparation and QA/QC
- Loads and validates field core data
- Merges location and sample datasets
- Performs quality assurance checks
- Validates VM0033 sample size requirements
- Handles bulk density (measured vs. estimated)
- Calculates depth profile completeness

### Part 2: Core Analysis Workflow

**Module P2_02:** Exploratory Data Analysis
- Generates summary statistics by stratum
- Creates depth profile visualizations
- Identifies outliers and data quality issues
- Produces diagnostic plots

**Module P2_03:** Depth Harmonization
- Implements equal-area spline functions
- Standardizes to VM0033 depth intervals (7.5, 22.5, 40, 75 cm)
- Extrapolates to maximum depth (100 cm)
- Validates mass balance
- Exports harmonized profiles for spatial modeling

**Module P2_04:** Spatial Predictions - Kriging
- Automated variogram modeling
- Universal kriging predictions
- Cross-validation assessment
- Uncertainty quantification via kriging variance
- Sample size power analysis

**Module P2_05:** Spatial Predictions - Random Forest
- Integration with Google Earth Engine covariates
- Random Forest model training
- Area of Applicability assessment
- Spatial cross-validation
- Variable importance analysis

**Module P2_06:** Carbon Stock Calculation
- Aggregates depth-specific predictions
- Calculates total carbon stocks (0-100 cm)
- Compares Kriging vs. Random Forest estimates
- Generates carbon stock maps
- Exports summary tables by stratum

**Module P2_07:** MMRV Reporting
- Generates VM0033 verification package
- Creates comprehensive HTML reports
- Exports spatial data for GIS verification
- Provides sampling recommendations
- Produces quality assurance documentation

### Part 3: Bayesian Analysis (Optional)

**Module P3_0c:** Bayesian Prior Setup
- Configures global prior distributions
- Validates prior parameters by ecosystem type

**Module P3_06c:** Posterior Estimation
- Combines local likelihood with global prior
- Bayesian updating via conjugate normal framework
- Posterior predictive distributions
- Credible interval calculation

**Module P3_07b:** Comprehensive Standards Report
- Multi-standard compliance assessment
- Compares frequentist vs. Bayesian results
- Generates action plan for verification

### Part 4: Transfer Learning (Optional)

**Module P4_3b:** Global Data Harmonization
- Processes global core database
- Harmonizes to VM0033 depths
- Exports for covariate extraction

**Module P4_05:** Transfer Learning Modeling
- Instance weighting for global sample similarity
- Hierarchical mixed-effects modeling
- Bias correction for local conditions
- Bootstrap uncertainty quantification

**Module P4_Transfer_Visualizations:** Diagnostic Plots
- Maps of predictions and uncertainty
- Validation scatter plots
- Variable importance charts

### Part 5: Scenario Modeling

**Module P5_01:** Spatial Scenario Definition
- Defines management zones
- Assigns restoration actions to zones
- Calculates baseline carbon stocks per zone

**Module P5_02:** Trajectory Modeling
- Projects future carbon stocks under restoration
- Models recovery trajectories (exponential, linear, chronosequence)
- Supports multiple restoration actions per zone
- Generates spatially-explicit predictions for verification periods

**Module P5_03:** VM0033 Crediting Calculator
- Calculates additionality (Project - Baseline)
- Tests statistical significance
- Applies leakage deduction
- Applies permanence risk buffer
- Generates credit issuance schedule

**Module P5_04:** Multi-Scenario Comparison (Planned)
- Compares alternative restoration strategies
- Optimizes spatial allocation of actions
- Cost-benefit analysis for carbon crediting

### Master Scripts

**MasterScript.R:** Sequential execution controller
- Runs complete workflow from setup through reporting
- Handles Part 2 (standard) or Part 3 (Bayesian) branches
- Logs all operations

**P5_scenario_config.R:** Centralized scenario configuration
- Defines restoration actions and parameters
- Manages baseline methods
- Controls trajectory modeling settings

---

## Required Inputs

### 1. Field Data (Mandatory)

#### A. Core Locations (`data_raw/core_locations.csv`)

Spatial metadata for each sediment core.

**Required Columns:**
| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `core_id` | Character | Unique core identifier | "SITE_001" |
| `longitude` | Numeric | WGS84 longitude (decimal degrees) | -123.4567 |
| `latitude` | Numeric | WGS84 latitude (decimal degrees) | 48.1234 |
| `stratum` | Character | Ecosystem type | "EM", "SG", "FL" |

**Optional Columns:**
| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `core_type` | Character | Sampling method | "HR" (high-resolution) or "Composite" |
| `scenario_type` | Character | Temporal category | "BASELINE", "PROJECT", "REFERENCE" |
| `collection_date` | Date | Field collection date | "2024-06-15" |
| `site_name` | Character | Descriptive location | "North Bay Marsh" |

**Valid Stratum Codes:**
- `EM` = Emergent Marsh (saltmarsh)
- `SG` = Seagrass
- `FL` = Forested (mangrove)
- `TF` = Tidal Flat (mudflat)

**Template:**
```csv
core_id,longitude,latitude,stratum,core_type,scenario_type
SITE_001,-123.4567,48.1234,EM,HR,BASELINE
SITE_002,-123.4589,48.1256,EM,HR,BASELINE
SITE_003,-123.4601,48.1278,SG,Composite,BASELINE
```

#### B. Core Samples (`data_raw/core_samples.csv`)

Depth profile data with laboratory measurements.

**Required Columns:**
| Column | Type | Description | Units | Example |
|--------|------|-------------|-------|---------|
| `core_id` | Character | Links to core_locations | - | "SITE_001" |
| `depth_top_cm` | Numeric | Top of sample interval | cm | 0 |
| `depth_bottom_cm` | Numeric | Bottom of sample interval | cm | 10 |
| `soc_g_kg` | Numeric | Soil organic carbon concentration | g C / kg dry soil | 45.2 |

**Optional Columns:**
| Column | Type | Description | Units | Example |
|--------|------|-------------|-------|---------|
| `bulk_density_g_cm3` | Numeric | Dry bulk density | g/cm³ | 0.52 |
| `sample_id` | Character | Laboratory tracking ID | - | "LAB_2024_001" |
| `fraction_om` | Numeric | Organic matter fraction | 0-1 | 0.15 |
| `c_n_ratio` | Numeric | Carbon to nitrogen ratio | - | 12.5 |

**Quality Thresholds (Configurable in `blue_carbon_config.R`):**
- SOC: 1 - 600 g/kg (flags values outside range)
- Bulk Density: 0.05 - 2.0 g/cm³
- Depth: 0 - 100 cm (VM0033 maximum)

**Template:**
```csv
core_id,depth_top_cm,depth_bottom_cm,soc_g_kg,bulk_density_g_cm3
SITE_001,0,5,48.3,0.54
SITE_001,5,10,52.1,0.51
SITE_001,10,20,46.8,0.49
SITE_001,20,30,38.2,0.48
```

**Notes on Bulk Density:**
- If not provided, ecosystem-specific defaults are applied (Morris et al. 2016)
- Workflow transparently reports which values are measured vs. estimated
- Measured BD strongly recommended for VM0033 verification

### 2. Remote Sensing Covariates (Optional, for Random Forest)

Raster datasets exported from Google Earth Engine or other platforms.

**Directory Structure:**
```
covariates/
├── optical/
│   ├── landsat_ndvi.tif
│   ├── landsat_ndwi.tif
│   └── sentinel2_red_edge.tif
├── sar/
│   ├── sentinel1_vv.tif
│   └── sentinel1_vh.tif
├── topographic/
│   ├── dem_elevation.tif
│   ├── slope.tif
│   └── tidal_range.tif
└── derived/
    ├── distance_to_channel.tif
    └── tidal_inundation_frequency.tif
```

**Recommended Covariates (Meyer & Pebesma 2021):**

**Optical (Landsat 8/9, Sentinel-2):**
- NDVI (Normalized Difference Vegetation Index)
- NDWI (Normalized Difference Water Index)
- Red Edge bands (Sentinel-2)
- Blue/Green bands (sediment proxy)

**SAR (Sentinel-1):**
- VV polarization (surface roughness)
- VH polarization (vegetation structure)
- VV/VH ratio

**Topographic:**
- Elevation (LiDAR-derived DEM preferred)
- Slope
- Topographic wetness index

**Tidal/Hydrologic:**
- Distance to nearest channel
- Tidal inundation frequency
- Salinity (if available)

**Requirements:**
- All rasters must have identical:
  - Coordinate reference system (CRS)
  - Spatial extent
  - Pixel resolution
- Use `gdalwarp` for alignment if needed

**Example GEE Export Script** (see `CoastalBlueCarbon_LargeScaleCovariateExtraction.ipynb` for full workflow):
```javascript
// Google Earth Engine JavaScript
var studyArea = ee.FeatureCollection('users/yourname/study_area');

var covariates = ee.Image.cat([
  ee.Image('LANDSAT/LC08/C02/T1_L2').median().normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI'),
  ee.Image('COPERNICUS/S1_GRD').select('VV').median().rename('S1_VV'),
  ee.Image('USGS/SRTMGL1_003').select('elevation')
]);

Export.image.toDrive({
  image: covariates,
  description: 'blue_carbon_covariates',
  region: studyArea,
  scale: 10, // 10m resolution
  crs: 'EPSG:32610' // UTM Zone 10N (adjust for your area)
});
```

### 3. Scenario-Specific Inputs (For Part 5)

#### Management Zones (Optional)
`scenarios/management_zones.shp` - Polygon shapefile defining spatial units for restoration actions.

If not provided, workflow creates grid-based zones automatically.

#### Restoration Actions
Configured in `P5_scenario_config.R`:
```r
RESTORATION_ACTIONS <- list(
  restore_hydrology = list(
    name = "Hydrological Restoration",
    trajectory_method = "exponential",
    k_rate = 0.15,  # recovery rate constant
    max_improvement_pct = 80
  ),
  plant_vegetation = list(
    name = "Revegetation",
    trajectory_method = "linear",
    recovery_years = 15,
    max_improvement_pct = 70
  )
)
```

### 4. Configuration File

`blue_carbon_config.R` - Controls all workflow parameters.

**Key Settings:**
```r
# Project Metadata
PROJECT_NAME <- "My Blue Carbon Project"
MONITORING_YEAR <- 2024

# VM0033 Compliance
VM0033_MIN_CORES <- 3        # Minimum cores per stratum
CONFIDENCE_LEVEL <- 0.95     # 95% confidence intervals
STANDARD_DEPTHS <- c(7.5, 22.5, 40, 75)  # VM0033 midpoints

# Coordinate Systems
INPUT_CRS <- 4326            # WGS84
PROCESSING_CRS <- 32610      # UTM Zone 10N (adjust for study area)

# Ecosystem-Specific Bulk Density Defaults (g/cm³)
BD_DEFAULTS <- list(
  EM = 0.52,  # Saltmarsh
  SG = 0.89,  # Seagrass
  FL = 0.38   # Mangrove
)

# Bayesian Analysis (Optional)
USE_BAYESIAN <- FALSE  # Set TRUE to enable Part 3
```

---

## Expected Outputs

### 1. Data Processing Outputs

**Location:** `data_processed/`

| File | Description | Format |
|------|-------------|--------|
| `cores_clean_bluecarbon.rds` | Cleaned field data with QA flags | R binary |
| `cores_clean_bluecarbon.csv` | Cleaned field data (human-readable) | CSV |
| `cores_harmonized_bluecarbon.rds` | Depth-harmonized profiles at VM0033 intervals | R binary |
| `cores_summary_by_stratum.csv` | Summary statistics per ecosystem stratum | CSV |
| `core_totals.csv` | Integrated carbon stocks (0-100 cm) per core | CSV |

### 2. Diagnostic Outputs

**Location:** `diagnostics/`

**Data Preparation:**
- `diagnostics/data_prep/vm0033_compliance_report.csv` - Sample size validation
- `diagnostics/data_prep/core_depth_completeness.csv` - Profile coverage by core
- `diagnostics/qaqc/bd_transparency_report.csv` - Bulk density measured vs. estimated

**Spatial Modeling:**
- `diagnostics/variograms/*.png` - Variogram plots per depth (Kriging)
- `diagnostics/crossvalidation/kriging_cv_results.csv` - Kriging validation metrics
- `diagnostics/crossvalidation/rf_cv_results.csv` - Random Forest validation metrics

**Harmonization:**
- `diagnostics/harmonization/spline_fits_by_core/*.png` - Visual validation of spline fits
- `diagnostics/harmonization/mass_balance_check.csv` - Carbon mass conservation

### 3. Spatial Predictions

**Location:** `outputs/predictions/`

**Kriging Outputs:**
- `kriging/carbon_stock_7.5cm.tif` - Carbon stock at 7.5 cm depth
- `kriging/carbon_stock_22.5cm.tif` - Carbon stock at 22.5 cm depth  
- `kriging/carbon_stock_40cm.tif` - Carbon stock at 40 cm depth
- `kriging/carbon_stock_75cm.tif` - Carbon stock at 75 cm depth
- `kriging/uncertainty_7.5cm.tif` - Prediction standard error at 7.5 cm
- *(repeated for each depth)*

**Random Forest Outputs:**
- `rf/carbon_stock_rf_7.5cm.tif` - RF carbon stock at 7.5 cm depth
- `rf/aoa_7.5cm.tif` - Area of Applicability mask at 7.5 cm
- `rf/di_7.5cm.tif` - Dissimilarity Index (extrapolation risk) at 7.5 cm
- *(repeated for each depth)*

**Raster Format:**
- GeoTIFF (`.tif`)
- Units: kg C/m² for carbon stocks
- CRS: Matches `PROCESSING_CRS` in config
- Resolution: Determined by covariate rasters or 10m default

### 4. Carbon Stock Summaries

**Location:** `outputs/carbon_stocks/`

| File | Description | Columns |
|------|-------------|---------|
| `carbon_stocks_kriging.csv` | Stratum-level stocks from Kriging | stratum, mean_stock_Mg_ha, se_stock_Mg_ha, lower_95_ci, upper_95_ci |
| `carbon_stocks_rf.csv` | Stratum-level stocks from Random Forest | stratum, mean_stock_Mg_ha, se_stock_Mg_ha, lower_95_ci, upper_95_ci |
| `carbon_stocks_method_comparison.csv` | Side-by-side comparison | stratum, kriging_mean, rf_mean, difference_pct |

**Units:**
- Mg C/ha = metric tonnes carbon per hectare
- Conversion: 1 Mg C/ha = 0.1 kg C/m²

**Map Outputs:**
- `outputs/carbon_stocks/maps/total_carbon_stock_0-100cm.tif` - Integrated stock (full profile)

### 5. MMRV Verification Package

**Location:** `outputs/mmrv_reports/`

**Primary Report:**
- `vm0033_verification_package.html` - Comprehensive HTML report with:
  - Project overview and metadata
  - Carbon stocks by stratum (conservative estimates)
  - Model performance metrics (R², RMSE)
  - QA/QC summary
  - Compliance checklist
  - Spatial data package description

**Supporting Files:**
- `vm0033_summary_tables.xlsx` - Excel workbook with all tables
- `sampling_recommendations.csv` - Sample size guidance for future monitoring
- `standards_compliance_summary.csv` - Multi-standard assessment

**Spatial Export Package:**
- `spatial_exports/carbon_stocks_*.tif` - All prediction rasters
- `spatial_exports/uncertainty_*.tif` - Uncertainty rasters
- `spatial_exports/aoa_*.tif` - Area of Applicability masks
- `spatial_exports/README.txt` - File descriptions and metadata

### 6. Scenario Modeling Outputs (Part 5)

**Location:** `scenarios/{SCENARIO_NAME}/`

**Structure per Scenario:**
```
scenarios/BASELINE/
├── baseline_stocks_total.tif          # Current carbon stocks
├── baseline_stocks_by_depth.tif       # Multi-band (one per depth)
├── management_zones.shp               # Spatial units
├── zone_summaries.csv                 # Baseline stocks per zone
├── scenario_definition.yaml           # Configuration metadata
└── tables/
    └── trajectory_summary.csv         # Time series projections

scenarios/PROJECT_SCENARIO/
├── project_stocks_total_year_5.tif    # Projected stocks at year 5
├── project_stocks_total_year_10.tif   # Projected stocks at year 10
├── zone_trajectories.csv              # Zone-specific time series
└── figures/
    └── recovery_curves.png            # Trajectory visualizations
```

**VM0033 Crediting Output:**
- `scenarios/crediting/vm0033_credit_schedule.csv` - Issuance schedule by verification period
  - Columns: `year`, `gross_additional_tCO2e`, `conservative_additional_tCO2e`, `leakage_deduction_tCO2e`, `permanence_buffer_tCO2e`, `creditable_tCO2e`

### 7. Model Objects (for reproducibility)

**Location:** `outputs/models/`

- `kriging/variogram_model_7.5cm.rds` - Fitted variogram parameters
- `rf/rf_model_7.5cm.rds` - Trained Random Forest model
- `transfer/transfer_model_depth_7.5.rds` - Transfer learning model (if Part 4 used)

### 8. Logs

**Location:** `logs/`

**Per-Module Logs:**
- `data_prep_YYYY-MM-DD.log` - Module P1_01 execution log
- `harmonization_YYYY-MM-DD.log` - Module P2_03 execution log
- `kriging_YYYY-MM-DD.log` - Module P2_04 execution log
- `rf_predictions_YYYY-MM-DD.log` - Module P2_05 execution log
- `mmrv_reporting_YYYY-MM-DD.log` - Module P2_07 execution log

---

## Implementation Guide

### Step-by-Step Execution

#### Prerequisites

1. **R Installation:** R ≥ 4.0.0, RStudio recommended
2. **System Dependencies:** GDAL, PROJ, GEOS (typically via `sf` package)
3. **Disk Space:** ~5 GB for full workflow with covariates
4. **Memory:** 8 GB RAM minimum, 16 GB recommended

#### Phase 1: Setup (First-Time Only)

**Step 1:** Clone or download workflow repository
```bash
cd ~/Desktop
mkdir MyBlueCarbon
cd MyBlueCarbon
# Copy all workflow scripts to this directory
```

**Step 2:** Install R packages
```r
source("P1_0a_install_packages.R")
```

**Expected Duration:** 5-15 minutes  
**Output:** All required packages installed

**Step 3:** Create directory structure
```r
source("P1_0b_setup_directories.R")
```

**Expected Duration:** < 1 minute  
**Output:** Folders created in working directory

**Step 4:** Prepare input data

a. Create `data_raw/core_locations.csv` using template above
b. Create `data_raw/core_samples.csv` using template above
c. Verify with:
```r
file.exists("data_raw/core_locations.csv")  # Should return TRUE
file.exists("data_raw/core_samples.csv")     # Should return TRUE
```

**Step 5:** Configure workflow

Edit `blue_carbon_config.R`:
```r
PROJECT_NAME <- "Your Project Name"
MONITORING_YEAR <- 2024
PROCESSING_CRS <- 32610  # Change to appropriate UTM zone
```

Find your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-

**Step 6:** (Optional) Add remote sensing covariates

If using Random Forest (Module P2_05):
```bash
# Place GeoTIFFs in covariates/ subdirectories
covariates/optical/landsat_ndvi.tif
covariates/sar/sentinel1_vv.tif
covariates/topographic/dem.tif
```

Verify alignment:
```r
library(terra)
ndvi <- rast("covariates/optical/landsat_ndvi.tif")
sar <- rast("covariates/sar/sentinel1_vv.tif")
compareGeom(ndvi, sar, stopOnError = FALSE)  # Should return TRUE
```

#### Phase 2: Core Analysis

**Step 7:** Data preparation and QA/QC
```r
source("P1_01_data_prep_bluecarbon.R")
```

**What it does:**
- Loads and validates field data
- Merges locations with samples
- Performs quality checks
- Validates VM0033 sample size requirements
- Handles bulk density defaults
- Flags outliers

**Expected Duration:** 1-2 minutes  
**Review Outputs:**
- Check `diagnostics/data_prep/vm0033_compliance_report.csv` for sample size adequacy
- Verify `diagnostics/qaqc/qa_report.rds` shows acceptable data quality

**Troubleshooting:**
- If errors about missing cores: Ensure all `core_id` values in samples exist in locations
- If stratum validation fails: Check spelling against `VALID_STRATA` in config

**Step 8:** Exploratory data analysis
```r
source("P2_02_exploratory_analysis_bluecarbon.R")
```

**What it does:**
- Generates summary statistics
- Creates depth profile plots
- Identifies spatial patterns
- Assesses data distribution

**Expected Duration:** 2-5 minutes  
**Review Outputs:**
- `outputs/plots/exploratory/depth_profiles_by_stratum.png` - Visual check of harmonization needs
- `outputs/plots/exploratory/soc_distribution.png` - Identify outliers

**Step 9:** Depth harmonization
```r
source("P2_3a_Depth_Harmonization_Local.R")
```

**What it does:**
- Applies equal-area spline functions
- Harmonizes to VM0033 standard depths (7.5, 22.5, 40, 75 cm)
- Extrapolates to 100 cm depth
- Validates mass balance

**Expected Duration:** 3-10 minutes (depends on number of cores)  
**Review Outputs:**
- `diagnostics/harmonization/mass_balance_check.csv` - Should show close to 100% conservation
- `diagnostics/harmonization/spline_fits_by_core/*.png` - Visual validation (spot-check random cores)

**Critical Check:**
```r
mass_balance <- read.csv("diagnostics/harmonization/mass_balance_check.csv")
summary(mass_balance$conservation_pct)
# Should be centered near 100%, range typically 98-102%
```

**Step 10a:** Spatial predictions - Kriging
```r
source("P2_04_raster_predictions_kriging_bluecarbon.R")
```

**What it does:**
- Automated variogram modeling
- Universal kriging for each depth
- Cross-validation assessment
- Uncertainty via kriging variance
- Sample size power analysis

**Expected Duration:** 5-15 minutes  
**Review Outputs:**
- `diagnostics/variograms/*.png` - Check for spatial structure
- `diagnostics/crossvalidation/kriging_cv_results.csv` - Model R² should be >0.5 for most depths

**When to use:** Always appropriate, especially when:
- No covariates available
- Small sample size (n < 30)
- Strong spatial autocorrelation evident

**Step 10b:** Spatial predictions - Random Forest (if covariates available)
```r
source("P2_05_raster_predictions_rf_bluecarbon.R")
```

**What it does:**
- Integrates remote sensing covariates
- Random Forest model training
- Area of Applicability calculation
- Spatial cross-validation
- Variable importance analysis

**Expected Duration:** 10-30 minutes (depends on raster size)  
**Review Outputs:**
- `diagnostics/crossvalidation/rf_cv_results.csv` - Compare R² to Kriging
- `outputs/predictions/rf/aoa_*.tif` - Check coverage (values 0-1, recommend >0.7 for crediting)

**When to use:** When:
- Covariates available
- Sample size moderate to large (n > 20)
- Environmental gradients well-represented

**Step 11:** Carbon stock calculation
```r
source("P2_06_carbon_stock_calculation_bluecarbon.R")
```

**What it does:**
- Integrates depth-specific predictions
- Calculates total stocks (0-100 cm profile)
- Compares Kriging vs. Random Forest (if both run)
- Generates carbon stock maps
- Exports summary tables by stratum

**Expected Duration:** 5-10 minutes  
**Review Outputs:**
- `outputs/carbon_stocks/carbon_stocks_method_comparison.csv` - Methods should agree within ~20%
- `outputs/carbon_stocks/maps/total_carbon_stock_0-100cm.tif` - Visual check for spatial patterns

**Step 12:** MMRV reporting and verification package
```r
source("P2_07_mmrv_reporting_bluecarbon.R")
```

**What it does:**
- Compiles all results into verification package
- Generates VM0033-compliant HTML report
- Creates Excel summary tables
- Exports spatial data for GIS verification
- Provides sampling recommendations for future monitoring

**Expected Duration:** 5-10 minutes  
**Primary Output:** Open `outputs/mmrv_reports/vm0033_verification_package.html` in web browser

**Verification Checklist:**
1. All strata meet VM0033 minimum sample size (n ≥ 3)
2. Conservative estimates use lower 95% CI
3. Model performance adequate (R² ≥ 0.5 for most depths)
4. Spatial predictions within Area of Applicability
5. Bulk density handling transparently documented

#### Phase 3: Optional Enhancements

**Bayesian Analysis (if local samples limited):**

Edit `blue_carbon_config.R`:
```r
USE_BAYESIAN <- TRUE
```

Then run:
```r
source("P3_0c_bayesian_prior_setup_bluecarbon.R")
source("P1_01_data_prep_bluecarbon.R")  # Re-run with Bayesian flag
source("P2_02_exploratory_analysis_bluecarbon.R")
source("P2_3a_Depth_Harmonization_Local.R")
source("P2_04_raster_predictions_kriging_bluecarbon.R")
source("P3_06c_bayesian_posterior_estimation_bluecarbon.R")
source("P3_07b_comprehensive_standards_report.R")
```

**Transfer Learning (if very limited local data, n < 10):**

Requires global database and covariates.

```r
source("P4_3b_Depth_Harmonization_Global.R")  # Prepare global data
# Export global cores to GEE, extract covariates
source("P4_05_Transfer_Learning_Modelling.R")
source("P4_Transfer_Learning_Visualizations.R")
```

#### Phase 4: Scenario Modeling (for project crediting)

**Configure scenarios** in `P5_scenario_config.R`:
```r
BASELINE_SCENARIO_NAME <- "BASELINE"
PROJECT_SCENARIO_NAME <- "RESTORATION_SCENARIO"

RESTORATION_ACTIONS <- list(
  restore_hydrology = list(
    trajectory_method = "exponential",
    k_rate = 0.15,
    max_improvement_pct = 80
  )
)

# Assign actions to management zones
ZONE_ACTIONS <- list(
  zone_1 = c("restore_hydrology"),
  zone_2 = c("restore_hydrology", "plant_vegetation")
)
```

**Run scenario workflow:**
```r
source("P5_01_scenario_definition_spatial.R")    # Define baseline and zones
source("P5_02_trajectory_modeling_integrated.R") # Project future stocks
source("P5_03_VM0033_Projections.R")             # Calculate creditable carbon
```

**Review crediting schedule:**
```r
credits <- read.csv("scenarios/crediting/vm0033_credit_schedule.csv")
head(credits)
```

**Expected Output:**
```
  year gross_additional_tCO2e conservative_additional_tCO2e leakage_deduction_tCO2e permanence_buffer_tCO2e creditable_tCO2e
1    5                 12453                         10985                    1099                    1648             8238
2   10                 28921                         25507                    2551                    3826            19130
```

### Automation with MasterScript

For routine execution after setup:
```r
# Standard workflow (frequentist)
source("MasterScript.R")  # Runs Part 1 + Part 2

# Bayesian workflow
# First, edit blue_carbon_config.R: USE_BAYESIAN <- TRUE
source("MasterScript.R")  # Automatically detects Bayesian flag
```

The master script handles:
- Environment clearing between parts
- Sequential module execution
- Error logging
- Progress reporting

---

## Standards Compliance

This workflow is designed to meet requirements of multiple blue carbon standards:

### VM0033 (Verra)

**Methodology:** VM0033 v2.0 - Methodology for Tidal Wetland and Seagrass Restoration

**Compliance Features:**
- Standard depth intervals: 0-15, 15-30, 30-50, 50-100 cm
- Minimum sample sizes: n ≥ 3 cores per stratum (validated in Module P1_01)
- Statistical precision: Workflow calculates achieved precision and recommends additional samples
- Conservative crediting: Lower 95% confidence bound used for all estimates
- Additionality demonstration: Paired t-tests in scenario modeling (Part 5)
- Leakage accounting: Configurable deduction (typically 10%)
- Permanence risk buffer: 10-20% based on risk assessment

**Documentation:** Module P2_07 generates complete verification package

**Reference:** https://verra.org/methodologies/vm0033-methodology-for-tidal-wetland-and-seagrass-restoration-v2-0/

### ORRAA (Ocean Risk and Resilience Action Alliance)

**Principles:**
1. High quality science: Cross-validated models, documented methods
2. Transparency: Open-source code, full uncertainty reporting
3. Stakeholder engagement: Outputs designed for multi-stakeholder review
4. Conservative approach: 95% CI lower bounds, risk buffers

**Compliance:** Module P3_07b includes ORRAA compliance checklist

**Reference:** https://www.oceanriskalliance.org/blue-carbon-guidance

### IPCC Wetlands Supplement

**Methodology Tier:**
- Tier 1: IPCC default factors (not used)
- Tier 2: Country-specific emission factors (supported via stratum averages)
- Tier 3: Site-specific spatial modeling ← **This workflow**

**Key Features:**
- 95% confidence intervals (IPCC Vol. 1, Ch. 3)
- Conservative approach for climate reporting
- Quality assurance procedures
- Transparent bulk density handling

**Reference:** IPCC (2014). 2013 Supplement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories: Wetlands. https://www.ipcc-nggip.iges.or.jp/public/wetlands/

### Canadian Blue Carbon Network

**Applicability:**
- Provincial-level carbon accounting
- Indigenous consultation framework (proponent responsibility)
- Climate action crediting

**Compliance:** Methodology aligns with Canadian protocols

**Reference:** https://bluecarboncanada.ca/

### Additional Standards Supported

- **Gold Standard:** Requires demonstration of sustainable development benefits (not assessed by workflow, proponent responsibility)
- **REDD+:** Wetland restoration can qualify under REDD+ framework with additional social safeguards
- **Sustainable Development Goals (SDGs):** Blue carbon projects contribute to SDG 13 (Climate Action), SDG 14 (Life Below Water), SDG 15 (Life on Land)

---

## References

### Primary Methodologies

**VM0033:**
Verra. (2022). VM0033 Methodology for Tidal Wetland and Seagrass Restoration, v2.0. Retrieved from https://verra.org/methodologies/vm0033-methodology-for-tidal-wetland-and-seagrass-restoration-v2-0/

**IPCC Wetlands:**
IPCC. (2014). 2013 Supplement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories: Wetlands. Hiraishi, T., Krug, T., Tanabe, K., Srivastava, N., Baasansuren, J., Fukuda, M., & Troxler, T.G. (eds). Published: IPCC, Switzerland.

**Blue Carbon Manual:**
Howard, J., Hoyt, S., Isensee, K., Pidgeon, E., & Telszewski, M. (Eds.). (2014). Coastal Blue Carbon: Methods for Assessing Carbon Stocks and Emissions Factors in Mangroves, Tidal Salt Marshes, and Seagrass Meadows. Conservation International, Intergovernmental Oceanographic Commission of UNESCO, International Union for Conservation of Nature. Arlington, Virginia, USA.

### Depth Harmonization

**Equal-Area Splines:**
Bishop, T.F.A., McBratney, A.B., & Laslett, G.M. (1999). Modelling soil attribute depth functions with equal-area quadratic smoothing splines. *Geoderma*, 91(1-2), 27-45. https://doi.org/10.1016/S0016-7061(99)00003-8

Malone, B.P., McBratney, A.B., Minasny, B., & Laslett, G.M. (2009). Mapping continuous depth functions of soil carbon storage and available water capacity. *Geoderma*, 154(1-2), 138-152. https://doi.org/10.1016/j.geoderma.2009.10.007

**Coastal Wetland Application:**
Holmquist, J.R., Windham-Myers, L., Blaauw, M., Crafton, J., Anisfeld, S.C., Ferner, M.C., ... & Megonigal, J.P. (2018). Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States. *Scientific Reports*, 8(1), 9478. https://doi.org/10.1038/s41598-018-26948-7

### Geostatistical Modeling

**Kriging:**
Goovaerts, P. (1997). *Geostatistics for Natural Resources Evaluation*. Oxford University Press. ISBN: 978-0195115383

Webster, R., & Oliver, M.A. (2007). *Geostatistics for Environmental Scientists*, 2nd Edition. John Wiley & Sons. https://doi.org/10.1002/9780470517277

**Variogram Modeling:**
Pebesma, E.J. (2004). Multivariable geostatistics in S: the gstat package. *Computers & Geosciences*, 30(7), 683-691. https://doi.org/10.1016/j.cageo.2004.03.012

### Machine Learning

**Random Forest:**
Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324

**Spatial ML:**
Hengl, T., Nussbaum, M., Wright, M.N., Heuvelink, G.B.M., & Gräler, B. (2018). Random forest as a generic framework for predictive modeling of spatial and spatio-temporal variables. *PeerJ*, 6, e5518. https://doi.org/10.7717/peerj.5518

**Area of Applicability:**
Meyer, H., & Pebesma, E. (2021). Predicting into unknown space? Estimating the area of applicability of spatial prediction models. *Methods in Ecology and Evolution*, 12(9), 1620-1633. https://doi.org/10.1111/2041-210X.13650

### Bayesian Methods

**Transfer Learning:**
Wadoux, A.M.J.-C., Brus, D.J., & Heuvelink, G.B.M. (2021). Accounting for non-stationary variance in geostatistical mapping of soil properties. *Geoderma*, 324, 114138. https://doi.org/10.1016/j.geoderma.2018.03.010

Wadoux, A.M.J.-C., Samuel-Rosa, A., Poggio, L., & Mulder, V.L. (2020). A note on knowledge discovery and machine learning in digital soil mapping. *European Journal of Soil Science*, 71(2), 133-136. https://doi.org/10.1111/ejss.12909

### Carbon Stock Calculation

**Bulk Density:**
Morris, J.T., Barber, D.C., Callaway, J.C., Chambers, R., Hagen, S.C., Hopkinson, C.S., ... & Wigand, C. (2016). Contributions of organic and inorganic matter to sediment volume and accretion in tidal wetlands at steady state. *Earth's Future*, 4(4), 110-121. https://doi.org/10.1002/2015EF000334

**Uncertainty Quantification:**
IPCC. (2006). *2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land Use*. Prepared by the National Greenhouse Gas Inventories Programme, Eggleston H.S., Buendia L., Miwa K., Ngara T. and Tanabe K. (eds). Published: IGES, Japan.

### Blue Carbon Databases

**Global Synthesis:**
Holmquist, J.R., Windham-Myers, L., Bernal, B., Byrd, K.B., Crooks, S., Gonneea, M.E., ... & Megonigal, J.P. (2018). Uncertainty in United States coastal wetland greenhouse gas inventorying. *Environmental Research Letters*, 13(11), 115005. https://doi.org/10.1088/1748-9326/aae157

**Coastal Carbon Network:**
Macreadie, P.I., Costa, M.D.P., Atwood, T.B., Friess, D.A., Kelleway, J.J., Kennedy, H., ... & Serrano, O. (2019). Blue carbon as a natural climate solution. *Nature Reviews Earth & Environment*, 1(1), 49-59. https://doi.org/10.1038/s43017-019-0004-8

### Quality Assurance

**Laboratory Methods:**
Soil Survey Staff. (2014). *Kellogg Soil Survey Laboratory Methods Manual*. Soil Survey Investigations Report No. 42, Version 5.0. R. Burt and Soil Survey Staff (eds.). U.S. Department of Agriculture, Natural Resources Conservation Service.

**Field Protocols:**
Emmer, I., von Unger, M., Needelman, B., Crooks, S., & Emmett-Mattox, S. (2015). *Coastal Blue Carbon in Practice: A Manual for Using the VCS Methodology for Tidal Wetland and Seagrass Restoration*. Restore America's Estuaries, Arlington, VA.

---

## Technical Support

### Common Issues and Solutions

**Issue:** "Configuration file not found"
**Solution:** Ensure `blue_carbon_config.R` is in the working directory. Run `source("P1_0b_setup_directories.R")` first.

**Issue:** "Missing required columns in samples"
**Solution:** Check column names in `core_samples.csv` match exactly: `core_id`, `depth_top_cm`, `depth_bottom_cm`, `soc_g_kg`

**Issue:** "Invalid stratum names detected"
**Solution:** Verify `stratum` column uses codes from `VALID_STRATA` (default: EM, SG, FL, TF)

**Issue:** Kriging predictions show artifacts or discontinuities
**Solution:** Check variogram model fit in `diagnostics/variograms/`. May need to adjust nugget or range manually.

**Issue:** Random Forest predictions outside Area of Applicability
**Solution:** Review AOA rasters. Consider adding more samples in under-represented environmental conditions.

**Issue:** Mass balance conservation < 95%
**Solution:** Indicates potential issues in spline fitting. Check for:
  - Depth intervals with extreme thickness (>50 cm)
  - Missing samples in mid-profile
  - Outlier SOC or BD values

**Issue:** "Cannot allocate vector of size X GB"
**Solution:** Memory error. Reduce raster resolution or use `terra::disagg()` to process in chunks.

### Workflow Modifications

**To change depth intervals:**
Edit `blue_carbon_config.R`:
```r
STANDARD_DEPTHS <- c(5, 15, 30, 50, 80)  # Custom depths
VM0033_THICKNESS <- c(10, 10, 20, 30, 40)  # Corresponding thicknesses
```
Note: Non-VM0033 depths may complicate verification.

**To add custom bulk density defaults:**
Edit `blue_carbon_config.R`:
```r
BD_DEFAULTS <- list(
  EM = 0.52,
  SG = 0.89,
  FL = 0.38,
  TF = 0.65,  # Custom: tidal flat
  CUSTOM = 0.70
)
```

**To exclude outliers:**
Option 1 - Flag in source data:
Add `exclude` column to `core_samples.csv`, set to `TRUE` for outliers.

Option 2 - Filter programmatically:
Edit `P1_01_data_prep_bluecarbon.R`, add after line loading samples:
```r
samples <- samples %>% filter(soc_g_kg <= 400)  # Example threshold
```

### Getting Help

**Documentation:**
- This README (comprehensive overview)
- Individual module headers (detailed parameter descriptions)
- `blue_carbon_config.R` comments (configuration options)

**Workflow Issues:**
Review log files in `logs/` directory for detailed error messages and timestamps.

**Scientific Questions:**
Consult references section for original methodology papers.

---

## Citation

If you use this workflow in a publication, please cite:

**This Workflow:**
[Your Name/Organization]. (2024). Blue Carbon MMRV Workflow v1.0: R-based Implementation for Coastal Wetland Carbon Monitoring, Reporting, and Verification. [Repository URL or DOI].

**Key Dependencies:**
- R Core Team. (2024). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/
- Pebesma, E. (2018). Simple Features for R: Standardized Support for Spatial Vector Data. *The R Journal*, 10(1), 439-446. https://doi.org/10.32614/RJ-2018-009
- Hijmans, R. (2024). terra: Spatial Data Analysis. R package version 1.7-71. https://CRAN.R-project.org/package=terra

**Methodologies:**
- Verra (2022) for VM0033 compliance
- Howard et al. (2014) for carbon stock calculation
- Bishop et al. (1999) / Malone et al. (2009) for depth harmonization
- Meyer & Pebesma (2021) for Area of Applicability
- [Others as appropriate to methods used]

---

## License

This workflow is provided for scientific and conservation purposes. Please consult with legal counsel regarding intellectual property rights for specific applications, particularly commercial carbon credit projects.

The workflow implements published scientific methods and standards that are in the public domain. Individual organizations (Verra, IPCC, etc.) retain copyright over their specific methodologies and should be cited appropriately.

---

## Version History

**v1.0 (November 2024):**
- Initial release
- VM0033 v2.0 compliance
- Kriging and Random Forest spatial modeling
- Bayesian integration (optional)
- Transfer learning (optional)
- Scenario modeling (Part 5)

**Planned Updates:**
- Part 5 Module 04: Multi-scenario optimization
- Integration with CDR.fyi carbon registry
- Automated GEE covariate extraction
- Shiny dashboard for interactive exploration

---

**End of README**

For questions or contributions, please contact: [Your Contact Information]

Last Updated: November 27, 2024
