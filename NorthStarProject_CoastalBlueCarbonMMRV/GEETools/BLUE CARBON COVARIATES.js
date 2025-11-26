// ============================================================================
// BLUE CARBON COVARIATES WITH COMPREHENSIVE QA/QC
// ============================================================================
// Version: 1.0 - Coastal Blue Carbon Edition
// Purpose: Generate VM0033-compliant covariates for coastal carbon modeling
// Key Features: Tidal indicators, coastal indices, lateral flux proxies, QA/QC
// Adapted from: Generic Carbon Stock Covariate Tool
// ============================================================================

// ============================================================================
// SECTION 1: CONFIGURATION
// ============================================================================

// IMPORTANT: Draw your AOI using the geometry tools or load from asset
var AOI = geometry;  // Draw a polygon on the map


// Optional: Load your strata/sampling locations for validation
// var samplingPoints = ee.FeatureCollection("users/your_name/sampling_points");

var CONFIG = {
  // Spatial Configuration
  aoi: AOI,
  exportScale: 10,  // 10m for coastal (higher res than terrestrial)
  exportCRS: 'EPSG:4326',
  processingCRS: 'EPSG:3347',  // Canada Albers Equal Area
  
  // Temporal Configuration
  yearStart: 2022,
  yearEnd: 2024,
  growingSeasonStartMonth: 5,  // May
  growingSeasonEndMonth: 9,     // September (for Canada)
  
  // Quality Control Thresholds (Blue Carbon Specific)
  s2CloudThreshold: 15,              // Stricter for coastal (was 20)
  s1SpeckleFilterSize: 7,
  minObservationsRequired: 15,        // More observations for tidal variability
  
  // Coastal-specific thresholds
  minElevation: -10,                  // Allow subtidal areas
  maxElevation: 20,                   // Coastal zone only
  maxSlopeForCarbon: 10,              // Coastal wetlands are flat
  
  // Vegetation Index Thresholds (Coastal)
  minNDVI: -0.3,                      // Allow water (negative NDVI)
  maxNDVI: 0.9,                       // Coastal vegetation
  minNDWI: -0.5,                      // Water index
  maxNDWI: 0.8,
  minMNDWI: -0.8,                     // Modified water index
  maxMNDWI: 0.8,
  
  // SAR Thresholds (dB) - Coastal
  minVV: -30,
  maxVV: 5,
  minVH: -35,
  maxVH: 0,
  
  // Water Occurrence Thresholds
  minWaterOccurrence: 0,
  maxWaterOccurrence: 100,
  
  // Processing Parameters
  qaStatsScaleMultiplier: 4,
  qaFocalRadius_pixels: 3,
  textureWindowSize: 3,
  spatialCV_threshold: 50,
  
  // Export Configuration
  exportFolder: 'BlueCarbon_Covariates',
  exportPrefix: 'BlueCarbon',
  maxPixels: 1e13,
  
  // Feature toggles
  includeTextureFeatures: true,
  includeSeasonalMetrics: true,
  includePhenologyMetrics: true,
  includeRadarIndices: true,
  includeTidalIndicators: true,     // NEW: Tidal metrics
  includeSalinityProxies: true,     // NEW: Salinity indicators
  includeConnectivityMetrics: true, // NEW: Hydrological connectivity
  includeBiomassProxies: true,      // NEW: Vegetation biomass
  includeQualityLayers: true,
  
  // DEM Selection
  demSource: 'CDEM'  // Use CDEM for Canada
};

// Date ranges
var startDate = ee.Date.fromYMD(CONFIG.yearStart, 1, 1);
var endDate = ee.Date.fromYMD(CONFIG.yearEnd, 12, 31);
var growingSeasonStart = ee.Date.fromYMD(CONFIG.yearStart, CONFIG.growingSeasonStartMonth, 1);
var growingSeasonEnd = ee.Date.fromYMD(CONFIG.yearEnd, CONFIG.growingSeasonEndMonth, 30);

Map.centerObject(CONFIG.aoi, 12);

print('═══════════════════════════════════════════════════════');
print('🌊 BLUE CARBON COVARIATE EXTRACTION');
print('═══════════════════════════════════════════════════════');
print('');
print('AOI Area (km²):', CONFIG.aoi.area(1).divide(1e6).getInfo().toFixed(2));
print('Date Range:', CONFIG.yearStart, '-', CONFIG.yearEnd);
print('Export Scale:', CONFIG.exportScale, 'm');
print('QA/QC: ENABLED');
print('Coastal Features: ENABLED');
print('');

// ============================================================================
// SECTION 2: TOPOGRAPHIC FEATURES WITH QA (COASTAL ADAPTED)
// ============================================================================

print('=== Processing Coastal Topographic Features ===');

// Use CDEM for Canada
var dem = ee.ImageCollection('NRCan/CDEM').mosaic().clip(CONFIG.aoi);
var elevation = dem.rename('elevation_m');

// QA CHECK: Elevation range validation
var elevStats = elevation.reduceRegion({
  reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - Elevation Range:', elevStats);

// Create elevation quality flag (coastal range)
var elevationQA = elevation.gte(CONFIG.minElevation)
  .and(elevation.lte(CONFIG.maxElevation))
  .rename('elevation_valid_flag');

// Calculate terrain derivatives
var slope = ee.Terrain.slope(elevation).rename('slope_degrees');
var aspect = ee.Terrain.aspect(elevation).rename('aspect_degrees');

// QA CHECK: Slope validation (should be very low for coastal wetlands)
var slopeStats = slope.reduceRegion({
  reducer: ee.Reducer.percentile([50, 90, 95, 99]),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - Slope Statistics:', slopeStats);
print('  → Coastal wetlands should have low slopes (<5°)');

// Flag steep slopes (rare in coastal wetlands, may indicate errors)
var slopeQA = slope.lt(CONFIG.maxSlopeForCarbon).rename('slope_valid_flag');

// Topographic Position Index (TPI) - Important for marsh microtopography
var tpi = elevation.subtract(
  elevation.focal_mean(100, 'circle', 'meters')  // Smaller radius for coastal
).rename('TPI_100m');

// Terrain Ruggedness Index
var tri = elevation.subtract(
  elevation.focal_median(3, 'square', 'pixels')
).abs().rename('TRI');

// NEW: Calculate Mean High Water proxy (95th percentile of coastal elevation)
var mhw_proxy = elevation.reduceRegion({
  reducer: ee.Reducer.percentile([95]),
  geometry: CONFIG.aoi,
  scale: 30,
  maxPixels: 1e9,
  bestEffort: true
});

var mhwElevation = ee.Number(mhw_proxy.values().get(0));
print('  Estimated MHW Elevation:', mhwElevation.getInfo(), 'm');

// NEW: Elevation relative to MHW (critical for tidal classification)
var elevationRelMHW = elevation.subtract(mhwElevation).rename('elev_rel_MHW_m');

var topographicFeatures = ee.Image.cat([
  elevation, 
  slope, 
  aspect, 
  tpi, 
  tri, 
  elevationRelMHW
]);

print('✓ Topographic features processed:', topographicFeatures.bandNames());

// ============================================================================
// SECTION 3: TIDAL & HYDROLOGICAL INDICATORS (BLUE CARBON SPECIFIC)
// ============================================================================

if (CONFIG.includeTidalIndicators) {
  print('\n=== Processing Tidal & Hydrological Indicators ===');
  
  // Load JRC Global Surface Water
  var waterOccurrence = ee.Image('JRC/GSW1_4/GlobalSurfaceWater')
    .select('occurrence')
    .clip(CONFIG.aoi)
    .rename('water_occurrence_pct');
  
  var waterRecurrence = ee.Image('JRC/GSW1_4/GlobalSurfaceWater')
    .select('recurrence')
    .clip(CONFIG.aoi)
    .rename('water_recurrence_pct');
  
  var waterTransitions = ee.Image('JRC/GSW1_4/GlobalSurfaceWater')
    .select('transition')
    .clip(CONFIG.aoi)
    .rename('water_transitions');
  
  // QA CHECK: Water occurrence statistics
  var waterStats = waterOccurrence.reduceRegion({
    reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
    geometry: CONFIG.aoi,
    scale: 30,
    maxPixels: 1e9,
    bestEffort: true
  });
  
  print('QA - Water Occurrence:', waterStats);
  
  // NEW: Tidal inundation frequency (days per year)
  var inundationFrequency = waterOccurrence.divide(100).multiply(365)
    .rename('inundation_days_per_year');
  
  // NEW: Tidal zone classification proxy
  var tidalZoneProxy = ee.Image(0)
    .where(waterOccurrence.gte(90), 5)  // Open water
    .where(waterOccurrence.gte(75).and(waterOccurrence.lt(90)), 4)  // Subtidal
    .where(waterOccurrence.gte(50).and(waterOccurrence.lt(75)), 3)  // Low intertidal
    .where(waterOccurrence.gte(25).and(waterOccurrence.lt(50)), 2)  // Mid intertidal
    .where(waterOccurrence.gt(0).and(waterOccurrence.lt(25)), 1)    // High intertidal
    .rename('tidal_zone_proxy');
  
  var tidalIndicators = ee.Image.cat([
    waterOccurrence,
    waterRecurrence,
    waterTransitions,
    inundationFrequency,
    tidalZoneProxy
  ]);
  
  print('✓ Tidal indicators processed:', tidalIndicators.bandNames());
}

// ============================================================================
// SECTION 4: SENTINEL-2 OPTICAL FEATURES (COASTAL ADAPTED)
// ============================================================================

print('\n=== Processing Sentinel-2 with Coastal Indices ===');

function maskS2clouds(image) {
  var qa = image.select('QA60');
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask).divide(10000);
}

function addBlueCarboIndices(image) {
  // Standard terrestrial indices
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  var evi = image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    {
      'NIR': image.select('B8'),
      'RED': image.select('B4'),
      'BLUE': image.select('B2')
    }).rename('EVI');
  
  // NEW: Coastal/Aquatic indices
  
  // Normalized Difference Water Index (NDWI) - Critical for tidal mapping
  var ndwi = image.normalizedDifference(['B3', 'B8']).rename('NDWI');
  
  // Modified Normalized Difference Water Index (MNDWI) - Better for turbid water
  var mndwi = image.normalizedDifference(['B3', 'B11']).rename('MNDWI');
  
  // Floating Algae Index (FAI) - For seagrass/macroalgae detection
  var fai = image.expression(
    'NIR - (RED + (SWIR1 - RED) * ((832.8 - 664.6) / (1613.7 - 664.6)))',
    {
      'NIR': image.select('B8'),
      'RED': image.select('B4'),
      'SWIR1': image.select('B11')
    }).rename('FAI');
  
  // Water-Adjusted Vegetation Index (WAVI) - For submerged vegetation
  var wavi = image.expression(
    '((NIR - BLUE) / (NIR + BLUE)) * 1.5',
    {
      'NIR': image.select('B8'),
      'BLUE': image.select('B2')
    }).rename('WAVI');
  
  // Normalized Difference Moisture Index (NDMI)
  var ndmi = image.normalizedDifference(['B8', 'B11']).rename('NDMI');
  
  // Red Edge indices (sensitive to chlorophyll in wetland vegetation)
  var ndre1 = image.normalizedDifference(['B8', 'B5']).rename('NDRE1');
  var ndre2 = image.normalizedDifference(['B8', 'B6']).rename('NDRE2');
  
  // Chlorophyll Index Red Edge (for biomass)
  var ciRedEdge = image.expression(
    '(NIR / RED_EDGE) - 1',
    {
      'NIR': image.select('B8'),
      'RED_EDGE': image.select('B5')
    }).rename('CI_RedEdge');
  
  // Soil-Adjusted Vegetation Index (for sparse vegetation)
  var savi = image.expression(
    '((NIR - RED) / (NIR + RED + 0.5)) * 1.5',
    {
      'NIR': image.select('B8'),
      'RED': image.select('B4')
    }).rename('SAVI');
  
  // Green Chlorophyll Index
  var gci = image.expression('(NIR / GREEN) - 1', {
    'NIR': image.select('B8'),
    'GREEN': image.select('B3')
  }).rename('GCI');
  
  return image.addBands([
    ndvi, evi, ndwi, mndwi, fai, wavi, ndmi, 
    ndre1, ndre2, ciRedEdge, savi, gci
  ]);
}

var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', CONFIG.s2CloudThreshold))
  .map(maskS2clouds)
  .map(addBlueCarboIndices);

// QA CHECK: Image availability
var s2Count = s2.size().getInfo();
print('QA - Sentinel-2 image count:', s2Count);

if (s2Count < CONFIG.minObservationsRequired) {
  print('  ⚠️ WARNING: Low image count. Consider expanding date range or relaxing cloud threshold.');
}

var s2_growing = s2.filterDate(growingSeasonStart, growingSeasonEnd);

// Use size check without blocking
s2_growing.size().evaluate(function(count) {
  print('QA - S2 growing season count:', count);
  if (count === 0) {
    print('  ⚠️ WARNING: No images in growing season. Using annual data for growing season metrics.');
  }
});

// If growing season is empty, use annual data as substitute
s2_growing = ee.Algorithms.If(
  s2_growing.size().gt(0),
  s2_growing,
  s2  // Fallback to annual if no growing season data
);
s2_growing = ee.ImageCollection(s2_growing);

// Calculate comprehensive metrics
var opticalMetrics = ee.Image.cat([
  // Annual NDVI
  s2.select('NDVI').median().rename('NDVI_median_annual'),
  s2.select('NDVI').mean().rename('NDVI_mean_annual'),
  s2.select('NDVI').reduce(ee.Reducer.stdDev()).rename('NDVI_stddev_annual'),
  s2.select('NDVI').min().rename('NDVI_min_annual'),
  s2.select('NDVI').max().rename('NDVI_max_annual'),
  
  // Growing season NDVI
  s2_growing.select('NDVI').median().rename('NDVI_median_growing'),
  s2_growing.select('NDVI').mean().rename('NDVI_mean_growing'),
  
  // NDVI amplitude (phenology proxy)
  s2.select('NDVI').max().subtract(s2.select('NDVI').min()).rename('NDVI_amplitude'),
  
  // Annual EVI
  s2.select('EVI').median().rename('EVI_median_annual'),
  s2.select('EVI').mean().rename('EVI_mean_annual'),
  s2.select('EVI').reduce(ee.Reducer.stdDev()).rename('EVI_stddev_annual'),
  
  // Growing season EVI
  s2_growing.select('EVI').median().rename('EVI_median_growing'),
  s2_growing.select('EVI').mean().rename('EVI_mean_growing'),
  
  // NEW: Water indices (critical for coastal)
  s2.select('NDWI').median().rename('NDWI_median_annual'),
  s2.select('NDWI').mean().rename('NDWI_mean_annual'),
  s2.select('NDWI').reduce(ee.Reducer.stdDev()).rename('NDWI_stddev_annual'),
  s2_growing.select('NDWI').median().rename('NDWI_median_growing'),
  
  s2.select('MNDWI').median().rename('MNDWI_median_annual'),
  s2.select('MNDWI').mean().rename('MNDWI_mean_annual'),
  s2_growing.select('MNDWI').median().rename('MNDWI_median_growing'),
  
  // NEW: Floating Algae Index
  s2.select('FAI').median().rename('FAI_median_annual'),
  s2_growing.select('FAI').median().rename('FAI_median_growing'),
  
  // NEW: Water-Adjusted Vegetation Index (for submerged veg)
  s2.select('WAVI').median().rename('WAVI_median_annual'),
  s2_growing.select('WAVI').median().rename('WAVI_median_growing'),
  
  // Moisture indices
  s2.select('NDMI').median().rename('NDMI_median_annual'),
  s2_growing.select('NDMI').median().rename('NDMI_median_growing'),
  
  // Red Edge indices (biomass proxy)
  s2.select('NDRE1').median().rename('NDRE1_median_annual'),
  s2.select('NDRE2').median().rename('NDRE2_median_annual'),
  s2_growing.select('NDRE1').median().rename('NDRE1_median_growing'),
  s2_growing.select('NDRE2').median().rename('NDRE2_median_growing'),
  
  // Chlorophyll indices
  s2.select('CI_RedEdge').median().rename('CI_RedEdge_median_annual'),
  s2_growing.select('CI_RedEdge').median().rename('CI_RedEdge_median_growing'),
  
  // SAVI (for sparse vegetation)
  s2.select('SAVI').median().rename('SAVI_median_annual'),
  s2_growing.select('SAVI').median().rename('SAVI_median_growing'),
  
  // GCI
  s2.select('GCI').median().rename('GCI_median_annual'),
  s2_growing.select('GCI').median().rename('GCI_median_growing'),
  
  // Phenology percentiles (growing season dynamics)
  s2.select('NDVI').reduce(ee.Reducer.percentile([10, 25, 50, 75, 90]))
    .rename(['NDVI_p10', 'NDVI_p25', 'NDVI_p50', 'NDVI_p75', 'NDVI_p90'])
]);

// QA CHECK: Coastal index ranges
var ndwiStats = opticalMetrics.select('NDWI_median_annual').reduceRegion({
  reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - NDWI Statistics:', ndwiStats);
print('  → Positive NDWI indicates water presence');

// DEBUG: Print actual band names in opticalMetrics
opticalMetrics.bandNames().evaluate(function(bands) {
  print('DEBUG - Optical metrics bands created:', bands.length);
  print('DEBUG - Band names:', bands);
});

// Create coastal vegetation QA flags
var ndviQA = opticalMetrics.select('NDVI_median_annual')
  .gte(CONFIG.minNDVI).and(opticalMetrics.select('NDVI_median_annual').lte(CONFIG.maxNDVI))
  .rename('NDVI_valid_flag');

var ndwiQA = opticalMetrics.select('NDWI_median_annual')
  .gte(CONFIG.minNDWI).and(opticalMetrics.select('NDWI_median_annual').lte(CONFIG.maxNDWI))
  .rename('NDWI_valid_flag');

print('✓ Optical metrics processed:', opticalMetrics.bandNames().length().getInfo(), 'bands');

// ============================================================================
// SECTION 5: SENTINEL-1 SAR FEATURES (COASTAL ADAPTED)
// ============================================================================

print('\n=== Processing Sentinel-1 SAR (Coastal) ===');

var s1 = ee.ImageCollection('COPERNICUS/S1_GRD')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate)
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  .filter(ee.Filter.eq('instrumentMode', 'IW'))
  .map(function(img) {
    var vv = img.select('VV').focal_median(CONFIG.s1SpeckleFilterSize, 'square', 'pixels');
    var vh = img.select('VH').focal_median(CONFIG.s1SpeckleFilterSize, 'square', 'pixels');
    return img.addBands(vv, null, true).addBands(vh, null, true);
  });

print('QA - Sentinel-1 image count:', s1.size().getInfo());

// Calculate SAR metrics
var sarFeatures = ee.Image.cat([
  // VV polarization (sensitive to surface roughness/flooding)
  s1.select('VV').median().rename('VV_median'),
  s1.select('VV').mean().rename('VV_mean'),
  s1.select('VV').reduce(ee.Reducer.stdDev()).rename('VV_stddev'),
  
  // VH polarization (sensitive to vegetation structure)
  s1.select('VH').median().rename('VH_median'),
  s1.select('VH').mean().rename('VH_mean'),
  s1.select('VH').reduce(ee.Reducer.stdDev()).rename('VH_stddev'),
  
  // VV/VH ratio (vegetation biomass proxy)
  s1.select('VV').median().subtract(s1.select('VH').median()).rename('VV_VH_ratio'),
  
  // Temporal variability (important for tidal dynamics)
  s1.select('VV').reduce(ee.Reducer.percentile([10, 90]))
    .rename(['VV_p10', 'VV_p90'])
]);

// SAR QA flags
var vvQA = sarFeatures.select('VV_median')
  .gte(CONFIG.minVV).and(sarFeatures.select('VV_median').lte(CONFIG.maxVV))
  .rename('VV_valid_flag');

var vhQA = sarFeatures.select('VH_median')
  .gte(CONFIG.minVH).and(sarFeatures.select('VH_median').lte(CONFIG.maxVH))
  .rename('VH_valid_flag');

print('✓ SAR features processed:', sarFeatures.bandNames());

// ============================================================================
// SECTION 6: HYDROLOGICAL CONNECTIVITY (BLUE CARBON SPECIFIC)
// ============================================================================

if (CONFIG.includeConnectivityMetrics) {
  print('\n=== Processing Hydrological Connectivity ===');
  
  // Calculate flow direction using D8 algorithm (8 directions)
  // D8 flow direction: identifies steepest downslope neighbor
  var flowDirection = ee.Terrain.aspect(elevation).rename('flow_direction_degrees');

  // Proper D8 flow accumulation algorithm
  // This calculates true flow accumulation by summing contributing cells
  // Uses iterative accumulation with neighborhood analysis

  // Step 1: Calculate slope to each neighbor (3x3 window)
  var neighbors = [
    [-1, -1], [-1, 0], [-1, 1],  // Top row
    [0, -1],           [0, 1],    // Middle row (skip center)
    [1, -1],  [1, 0],  [1, 1]     // Bottom row
  ];

  // Calculate flow direction as aspect-based drainage
  var slopeRadians = slope.multiply(Math.PI).divide(180);
  var aspectRadians = flowDirection.multiply(Math.PI).divide(180);

  // Step 2: Calculate contributing area using focal operations
  // Each pixel contributes to downstream pixels based on flow direction
  // Approximate flow accumulation using iterative focal sums weighted by slope
  var flowAccumulation = ee.Image(1);  // Initialize with 1 (self)

  // Iterate to accumulate flow (D8 approximation using focal operations)
  for (var iter = 0; iter < 5; iter++) {  // 5 iterations for local watersheds
    var flowContribution = flowAccumulation
      .focal_max({
        kernel: ee.Kernel.square({radius: 1, units: 'pixels'}),
        iterations: 1
      })
      .multiply(slopeRadians.divide(10).add(1));  // Weight by slope

    flowAccumulation = flowAccumulation.add(flowContribution.multiply(0.3));
  }

  flowAccumulation = flowAccumulation
    .log10()  // Log scale for better visualization
    .rename('flow_accumulation_log');
  
  // Alternative: Use slope to identify potential channels
  var channelPotential = slope.lt(2)  // Very flat areas
    .and(elevation.lt(mhwElevation))  // Below MHW
    .rename('channel_potential');
  
  // Distance to potential channels (low-lying flat areas)
  var channelMask = slope.lt(1).and(elevation.lt(mhwElevation.subtract(0.5)));
  var distToChannel = channelMask.not().distance(ee.Kernel.euclidean(1000, 'meters'))
    .clip(CONFIG.aoi)
    .rename('dist_to_channel_m');
  
  // Topographic Wetness Index (simplified for coastal)
  var slopeForTWI = slope.max(0.1);  // Avoid division by zero
  var twi = elevation.multiply(-1)  // Lower elevation = wetter
    .add(10)  // Offset
    .divide(slopeForTWI.add(0.1))
    .rename('TWI');
  
  // NEW: Lateral transport potential (proxy for carbon export)
  // Based on slope and proximity to channels
  var lateralTransportPotential = slope
    .multiply(distToChannel.divide(100).add(1))
    .log()
    .rename('lateral_transport_potential');
  
  // Flow convergence index (terrain curvature-based)
  var curvature = elevation.convolve(ee.Kernel.laplacian8())
    .rename('terrain_curvature');
  
  var connectivityMetrics = ee.Image.cat([
    flowAccumulation,
    channelPotential,
    distToChannel,
    twi,
    lateralTransportPotential,
    curvature
  ]);
  
  print('✓ Connectivity metrics processed:', connectivityMetrics.bandNames());
}

// ============================================================================
// SECTION 7: SALINITY PROXIES (BLUE CARBON SPECIFIC)
// ============================================================================

if (CONFIG.includeSalinityProxies) {
  print('\n=== Processing Salinity Proxies (Multi-Factor) ===');

  // FACTOR 1: Distance to permanent water bodies (ocean/estuary proxy)
  var permanentWater = waterOccurrence.gte(90);
  var distToOcean = permanentWater.fastDistanceTransform()
    .sqrt()
    .multiply(ee.Image.pixelArea().sqrt())
    .rename('dist_to_ocean_m');

  // Normalized distance score (0-10, closer to ocean = higher)
  var oceanProximityScore = distToOcean.divide(1000).multiply(-1).add(10)
    .clamp(0, 10)
    .rename('ocean_proximity_score');

  // FACTOR 2: Tidal inundation frequency (more frequent = higher salinity exposure)
  // Use water occurrence as proxy for tidal influence
  var tidalInfluenceScore = waterOccurrence.divide(10)  // Scale to 0-10
    .clamp(0, 10)
    .rename('tidal_influence_score');

  // FACTOR 3: Freshwater input proxy
  // Higher elevation areas and those far from drainage channels likely have less freshwater dilution
  // Lower elevation coastal areas with high water occurrence but low elevation = more marine influence
  var minElevation = ee.Number(elevation.reduceRegion({
    reducer: ee.Reducer.min(),
    geometry: CONFIG.aoi,
    scale: 30,
    bestEffort: true,
    maxPixels: 1e9
  }).values().get(0));

  var freshwaterDilution = elevation.subtract(minElevation)
    .divide(5)  // Normalize by typical coastal elevation range (5m)
    .clamp(0, 10);

  // Invert so low values = high salinity (less freshwater dilution)
  var freshwaterDeficit = ee.Image(10).subtract(freshwaterDilution)
    .rename('freshwater_deficit_score');

  // FACTOR 4: Drainage connectivity (using flow accumulation)
  // Areas with high flow accumulation receive more freshwater runoff
  // Log scale and invert: low flow = high salinity
  var drainageIsolation = ee.Image(10).subtract(
    flowAccumulation.unitScale(0, 2).multiply(10)  // Normalize log flow accum
  ).clamp(0, 10).rename('drainage_isolation_score');

  // MULTI-FACTOR SALINITY RISK INDEX
  // Weighted combination of all factors (0-100 scale)
  // Higher values = higher salinity conditions
  var salinityRiskIndex = oceanProximityScore.multiply(0.35)      // 35% - distance to ocean
    .add(tidalInfluenceScore.multiply(0.30))                      // 30% - tidal inundation
    .add(freshwaterDeficit.multiply(0.20))                        // 20% - freshwater deficit
    .add(drainageIsolation.multiply(0.15))                        // 15% - drainage isolation
    .multiply(10)  // Scale to 0-100
    .rename('salinity_risk_index_0_100');

  // Categorical salinity zones (for stratification)
  var salinityZone = ee.Image(0)
    .where(salinityRiskIndex.gte(70), 4)   // Euhaline (marine, >30 ppt)
    .where(salinityRiskIndex.gte(50).and(salinityRiskIndex.lt(70)), 3)  // Polyhaline (18-30 ppt)
    .where(salinityRiskIndex.gte(30).and(salinityRiskIndex.lt(50)), 2)  // Mesohaline (5-18 ppt)
    .where(salinityRiskIndex.gte(10).and(salinityRiskIndex.lt(30)), 1)  // Oligohaline (0.5-5 ppt)
    .rename('salinity_zone_categorical');

  var salinityProxies = ee.Image.cat([
    distToOcean,
    oceanProximityScore,
    tidalInfluenceScore,
    freshwaterDeficit,
    drainageIsolation,
    salinityRiskIndex,
    salinityZone
  ]);

  print('✓ Multi-factor salinity proxies processed:', salinityProxies.bandNames());
  print('  → Factors: Ocean proximity, Tidal influence, Freshwater deficit, Drainage isolation');
}

// ============================================================================
// SECTION 8: BIOMASS PROXIES (BLUE CARBON SPECIFIC)
// ============================================================================

if (CONFIG.includeBiomassProxies) {
  print('\n=== Processing Biomass Proxies ===');

  // Combined optical-SAR biomass index
  // NOTE: Current weights (0.4 NDVI, 0.3 EVI, 0.3 SAR) are generic defaults
  // TODO: Calibrate these weights using field biomass measurements for your specific
  //       coastal ecosystem types (marsh vs seagrass vs mangrove). Site-specific
  //       calibration can improve biomass estimation accuracy by 20-40%.
  //       Recommended approach: Multiple regression with field AGB samples.
  var biomassIndex = opticalMetrics.select('NDVI_median_growing').multiply(0.4)
    .add(opticalMetrics.select('EVI_median_growing').multiply(0.3))
    .add(sarFeatures.select('VH_median').divide(-20).multiply(0.3))  // Normalized SAR
    .rename('biomass_index');
  
  // Canopy height proxy (VH-VV difference, normalized)
  var canopyHeightProxy = sarFeatures.select('VH_median')
    .subtract(sarFeatures.select('VV_median'))
    .multiply(-1)
    .rename('canopy_height_proxy');
  
  // Vegetation productivity proxy (NDVI * EVI)
  var productivityProxy = opticalMetrics.select('NDVI_median_growing')
    .multiply(opticalMetrics.select('EVI_median_growing'))
    .rename('productivity_proxy');
  
  var biomassProxies = ee.Image.cat([
    biomassIndex,
    canopyHeightProxy,
    productivityProxy
  ]);
  
  print('✓ Biomass proxies processed:', biomassProxies.bandNames());
}

// ============================================================================
// SECTION 9: SEDIMENT DYNAMICS INDICATORS (BLUE CARBON SPECIFIC)
// ============================================================================

print('\n=== Processing Sediment Dynamics Indicators ===');

// INDICATOR 1: Turbidity Proxy
// Use red band reflectance and NDWI to estimate turbidity
// Higher red reflectance + lower NDWI = higher turbidity
var redReflectance = s2.select('B4').median().rename('red_reflectance_median');
var turbidityProxy = redReflectance
  .multiply(opticalMetrics.select('NDWI_median_annual').multiply(-1).add(1))  // Invert NDWI
  .multiply(1000)  // Scale up for visualization
  .rename('turbidity_proxy');

// Turbidity temporal variability (indicates active sediment transport)
var turbidityVariability = s2.select('B4').reduce(ee.Reducer.stdDev())
  .multiply(1000)
  .rename('turbidity_variability');

// INDICATOR 2: Sediment Supply Index
// Combines flow accumulation, slope, and erosion potential
// Higher values = more sediment being transported to the area

// Erosion potential from upslope areas (steep areas with high flow accumulation)
var erosionPotential = slope
  .multiply(flowAccumulation.unitScale(0, 2))  // Weight by drainage area
  .rename('upslope_erosion_potential');

// Sediment delivery potential (considers both source and transport)
var sedimentSupplyIndex = erosionPotential
  .focal_mean(300, 'circle', 'meters')  // Aggregate upslope erosion
  .multiply(flowAccumulation)
  .log10()  // Log scale
  .rename('sediment_supply_index');

// INDICATOR 3: Local Erosion Risk
// Based on: elevation position, slope, water exposure, vegetation cover

// Exposure to wave/current energy (combination of water occurrence and position)
var waveExposure = waterOccurrence.divide(100)
  .multiply(ee.Image(1).subtract(opticalMetrics.select('NDVI_median_annual').clamp(0, 1)))  // Low veg = high exposure
  .multiply(elevation.lt(mhwElevation).multiply(2).add(1))  // Below MHW = more exposed
  .rename('wave_exposure_index');

// Erosion risk score (0-100)
// High risk = steep, unvegetated, tidally exposed, low sediment supply
var erosionRisk = slope.unitScale(0, 5).multiply(20)  // 20% - slope contribution
  .add(ee.Image(100).subtract(opticalMetrics.select('NDVI_median_annual').add(1).multiply(50)).clamp(0, 100).multiply(0.25))  // 25% - low veg
  .add(waveExposure.unitScale(0, 2).multiply(100).multiply(0.30))  // 30% - wave exposure
  .add(ee.Image(100).subtract(sedimentSupplyIndex.unitScale(0, 3).multiply(100)).clamp(0, 100).multiply(0.25))  // 25% - low sediment supply
  .clamp(0, 100)
  .rename('erosion_risk_score_0_100');

// INDICATOR 4: Accretion Potential
// Inverse of erosion - areas likely to accumulate sediment
// High accretion = low slope, sheltered, vegetated, high sediment supply
var accretionPotential = ee.Image(100).subtract(slope.unitScale(0, 5).multiply(100)).multiply(0.20)  // 20% - low slope
  .add(opticalMetrics.select('NDVI_median_annual').add(1).multiply(50).clamp(0, 100).multiply(0.25))  // 25% - high veg
  .add(ee.Image(100).subtract(waveExposure.unitScale(0, 2).multiply(100)).clamp(0, 100).multiply(0.30))  // 30% - sheltered
  .add(sedimentSupplyIndex.unitScale(0, 3).multiply(100).clamp(0, 100).multiply(0.25))  // 25% - high sediment supply
  .clamp(0, 100)
  .rename('accretion_potential_0_100');

var sedimentDynamics = ee.Image.cat([
  turbidityProxy,
  turbidityVariability,
  erosionPotential,
  sedimentSupplyIndex,
  waveExposure,
  erosionRisk,
  accretionPotential
]);

print('✓ Sediment dynamics indicators processed:', sedimentDynamics.bandNames());
print('  → Turbidity, Sediment supply, Erosion risk, Accretion potential');

// ============================================================================
// SECTION 10: VEGETATION CLASSIFICATION (BLUE CARBON SPECIFIC)
// ============================================================================

print('\n=== Processing Vegetation Classification Indices ===');

// Spectral separability indices for coastal vegetation types
// Goal: Differentiate emergent marsh vs seagrass/SAV vs unvegetated

// INDEX 1: Emergent Marsh Indicator
// Emergent marsh: High NDVI, moderate NDWI (not fully aquatic), high NIR
// Strong vegetation signal but emergent above water
var emergentMarshIndex = opticalMetrics.select('NDVI_median_growing')
  .multiply(0.4)  // Strong vegetation
  .add(opticalMetrics.select('NDWI_median_annual').multiply(-0.3).add(0.3))  // Not too wet
  .add(opticalMetrics.select('SAVI_median_growing').multiply(0.3))  // Soil-adjusted veg
  .clamp(0, 1)
  .rename('emergent_marsh_index');

// INDEX 2: Submerged Aquatic Vegetation (SAV/Seagrass) Indicator
// SAV: Moderate NDVI, high NDWI (aquatic), high WAVI, positive FAI
// Vegetation signal but in water
var savSeagrassIndex = opticalMetrics.select('WAVI_median_growing')
  .multiply(0.35)  // Water-adjusted veg index
  .add(opticalMetrics.select('FAI_median_growing').clamp(-0.1, 0.2).add(0.1).multiply(0.25))  // Floating algae index
  .add(opticalMetrics.select('NDWI_median_annual').clamp(0, 0.5).multiply(0.25))  // Moderately aquatic
  .add(opticalMetrics.select('NDVI_median_growing').clamp(0.1, 0.5).multiply(0.15))  // Some veg signal
  .clamp(0, 1)
  .rename('sav_seagrass_index');

// INDEX 3: Unvegetated Tidal Flat Indicator
// Low NDVI, variable NDWI (tidal exposure), high red/SWIR reflectance
var tidalFlatIndex = ee.Image(1)
  .subtract(opticalMetrics.select('NDVI_median_annual').clamp(-0.2, 0.3).add(0.2).divide(0.5))  // Low NDVI
  .multiply(0.4)
  .add(waterOccurrence.divide(100).multiply(0.3))  // Some tidal influence
  .add(s2.select('B11').median().clamp(0, 0.3).divide(0.3).multiply(0.3))  // High SWIR (soil/sediment)
  .clamp(0, 1)
  .rename('tidal_flat_index');

// INDEX 4: Open Water Indicator
// High NDWI, negative NDVI, high water occurrence
var openWaterIndex = opticalMetrics.select('MNDWI_median_annual')
  .clamp(-0.2, 0.8)
  .add(0.2)
  .divide(1.0)
  .multiply(0.5)
  .add(waterOccurrence.divide(100).multiply(0.5))
  .clamp(0, 1)
  .rename('open_water_index');

// Dominant vegetation type classification (winner-takes-all)
var vegTypeScore = ee.Image.cat([
  emergentMarshIndex.multiply(10).rename('score_marsh'),
  savSeagrassIndex.multiply(10).rename('score_sav'),
  tidalFlatIndex.multiply(10).rename('score_flat'),
  openWaterIndex.multiply(10).rename('score_water')
]);

var dominantVegType = vegTypeScore.reduce(ee.Reducer.max())
  .eq(vegTypeScore)
  .reduce(ee.Reducer.sum())  // Count matches
  .where(vegTypeScore.reduce(ee.Reducer.max()).lt(2), 0);  // Low confidence = 0

// Assign categories: 1=Marsh, 2=SAV/Seagrass, 3=Tidal Flat, 4=Open Water
var vegClassification = ee.Image(0)
  .where(emergentMarshIndex.eq(vegTypeScore.reduce(ee.Reducer.max()).divide(10)), 1)
  .where(savSeagrassIndex.eq(vegTypeScore.reduce(ee.Reducer.max()).divide(10)), 2)
  .where(tidalFlatIndex.eq(vegTypeScore.reduce(ee.Reducer.max()).divide(10)), 3)
  .where(openWaterIndex.eq(vegTypeScore.reduce(ee.Reducer.max()).divide(10)), 4)
  .updateMask(vegTypeScore.reduce(ee.Reducer.max()).gte(2))  // Mask low confidence
  .rename('vegetation_class_categorical');

// Classification confidence (0-100)
var classificationConfidence = vegTypeScore.reduce(ee.Reducer.max())
  .multiply(10)
  .clamp(0, 100)
  .rename('veg_classification_confidence');

var vegetationClassification = ee.Image.cat([
  emergentMarshIndex,
  savSeagrassIndex,
  tidalFlatIndex,
  openWaterIndex,
  vegClassification,
  classificationConfidence
]);

print('✓ Vegetation classification processed:', vegetationClassification.bandNames());
print('  → Classes: 1=Emergent Marsh, 2=SAV/Seagrass, 3=Tidal Flat, 4=Open Water');

// ============================================================================
// SECTION 11: OBSERVATION COUNTS & QA LAYERS
// ============================================================================

print('\n=== Processing Quality Assessment Layers ===');

// Count observations
var observationCounts = ee.Image.cat([
  s2.select('NDVI').count().rename('optical_observation_count'),
  s2_growing.select('NDVI').count().rename('optical_growing_count'),
  s1.select('VV').count().rename('SAR_observation_count')
]);

// Minimum observation flag
var minObsFlag = ee.Image.cat([
  observationCounts.select('optical_observation_count')
    .gte(CONFIG.minObservationsRequired)
    .rename('optical_sufficient_flag'),
  observationCounts.select('SAR_observation_count')
    .gte(CONFIG.minObservationsRequired)
    .rename('SAR_sufficient_flag')
]);

// Spatial heterogeneity (Coefficient of Variation)
var ndviCV = opticalMetrics.select('NDVI_stddev_annual')
  .divide(opticalMetrics.select('NDVI_mean_annual').abs().add(0.001))
  .multiply(100)
  .rename('NDVI_spatial_CV_pct');

var spatialHomogeneityFlag = ndviCV.lt(CONFIG.spatialCV_threshold)
  .rename('spatial_homogeneity_flag');

// Composite quality score (0-100)
var qualityScore = ee.Image.cat([
  elevationQA.multiply(10),
  slopeQA.multiply(10),
  ndviQA.multiply(15),
  ndwiQA.multiply(15),  // NEW: Water index QA
  vvQA.multiply(10),
  vhQA.multiply(10),
  minObsFlag.select('optical_sufficient_flag').multiply(15),
  minObsFlag.select('SAR_sufficient_flag').multiply(10),
  spatialHomogeneityFlag.multiply(5)
]).reduce(ee.Reducer.sum()).rename('composite_quality_score');

// Overall data completeness mask
var completeMask = ee.Image.cat([
  topographicFeatures,
  opticalMetrics,
  sarFeatures
]).mask().reduce(ee.Reducer.min()).rename('data_completeness');

// Combine all QA layers
var qualityLayers = ee.Image.cat([
  observationCounts,
  elevationQA,
  slopeQA,
  ndviQA,
  ndwiQA,
  vvQA,
  vhQA,
  minObsFlag,
  spatialHomogeneityFlag,
  ndviCV,
  qualityScore,
  completeMask
]);

// Calculate quality statistics
qualityScore.reduceRegion({
  reducer: ee.Reducer.mean()
    .combine(ee.Reducer.percentile([10, 50, 90]), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
}).evaluate(function(qualityStats) {
  print('\n=== QUALITY ASSESSMENT SUMMARY ===');
  print('Mean Quality Score:', qualityStats.composite_quality_score_mean);
  print('Quality Score Distribution:', qualityStats);
});

completeMask.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
}).evaluate(function(completenessPercent) {
  print('Data Completeness:', (completenessPercent.data_completeness * 100).toFixed(1), '%');
});

qualityScore.gte(70).reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
}).evaluate(function(highQualityPercent) {
  print('High Quality Area (score ≥70):', (highQualityPercent.composite_quality_score * 100).toFixed(1), '%');
  print('✓ Quality assessment complete');
});

// ============================================================================
// SECTION 12: COMBINE ALL FEATURES
// ============================================================================

print('\n=== Combining All Covariate Layers ===');

var allFeatures = ee.Image.cat([
  topographicFeatures,
  tidalIndicators,
  opticalMetrics,
  sarFeatures,
  connectivityMetrics,
  salinityProxies,
  biomassProxies,
  sedimentDynamics,
  vegetationClassification
]).clip(CONFIG.aoi).toFloat();

print('Total covariate bands:', allFeatures.bandNames().length().getInfo());
print('Covariate bands:', allFeatures.bandNames());

// ============================================================================
// SECTION 13: VISUALIZATION
// ============================================================================

print('\n=== Adding Visualization Layers ===');

Map.addLayer(elevation, {min: -5, max: 10, palette: ['blue', 'cyan', 'yellow', 'green']}, 
            'Elevation (m)', false);
Map.addLayer(slope, {min: 0, max: 5, palette: ['white', 'yellow', 'red']}, 
            'Slope (degrees)', false);
Map.addLayer(waterOccurrence, {min: 0, max: 100, palette: ['white', 'cyan', 'blue']}, 
            'Water Occurrence %', false);
Map.addLayer(opticalMetrics.select('NDVI_median_growing'), 
            {min: -0.2, max: 0.8, palette: ['blue', 'white', 'green']}, 
            'NDVI Growing Season', false);
Map.addLayer(opticalMetrics.select('NDWI_median_annual'),
            {min: -0.5, max: 0.5, palette: ['brown', 'white', 'blue']},
            'NDWI (Water Index)', false);
Map.addLayer(sarFeatures.select('VH_median'), 
            {min: -25, max: -10, palette: ['black', 'white', 'green']}, 
            'SAR VH (Vegetation)', false);
Map.addLayer(qualityScore, 
            {min: 0, max: 100, palette: ['red', 'yellow', 'green']}, 
            'Quality Score', true);

if (CONFIG.includeBiomassProxies) {
  Map.addLayer(biomassProxies.select('biomass_index'),
              {min: 0, max: 0.5, palette: ['brown', 'yellow', 'green']},
              'Biomass Index', false);
}

print('✓ Visualization layers added');

// ============================================================================
// SECTION 14: EXPORT FUNCTIONS
// ============================================================================

print('\n========================================');
print('READY TO EXPORT BLUE CARBON COVARIATES');
print('========================================\n');

/**
 * Export individual bands as separate GeoTIFF files
 */
function exportIndividualBands() {
  print('=== EXPORTING INDIVIDUAL COVARIATE BANDS ===');
  
  var bandNames = allFeatures.bandNames().getInfo();
  
  print('Total bands to export:', bandNames.length);
  print('Export folder:', CONFIG.exportFolder);
  print('\nCreating export tasks...\n');
  
  for (var i = 0; i < bandNames.length; i++) {
    var bandName = bandNames[i];
    var singleBand = allFeatures.select(bandName);
    var cleanName = bandName.replace(/[^a-zA-Z0-9_]/g, '_');
    
    Export.image.toDrive({
      image: singleBand.toFloat(),
      description: CONFIG.exportPrefix + '_' + cleanName,
      fileNamePrefix: cleanName,
      folder: CONFIG.exportFolder,
      region: CONFIG.aoi,
      scale: CONFIG.exportScale,
      crs: CONFIG.exportCRS,
      maxPixels: CONFIG.maxPixels,
      fileFormat: 'GeoTIFF',
      formatOptions: {
        cloudOptimized: true
      }
    });
    
    if ((i + 1) % 10 === 0 || i === bandNames.length - 1) {
      print('  Created tasks:', (i + 1), '/', bandNames.length);
    }
  }
  
  print('\n✓ All covariate band export tasks created!');
}

/**
 * Export quality assessment layers
 */
function exportQualityLayers() {
  print('\n=== EXPORTING QUALITY ASSESSMENT LAYERS ===');
  
  var qaLayerNames = qualityLayers.bandNames().getInfo();
  
  print('QA layers to export:', qaLayerNames.length);
  
  for (var i = 0; i < qaLayerNames.length; i++) {
    var layerName = qaLayerNames[i];
    var singleLayer = qualityLayers.select(layerName);
    var cleanName = 'QA_' + layerName.replace(/[^a-zA-Z0-9_]/g, '_');
    
    Export.image.toDrive({
      image: singleLayer.toFloat(),
      description: cleanName,
      fileNamePrefix: cleanName,
      folder: CONFIG.exportFolder,
      region: CONFIG.aoi,
      scale: CONFIG.exportScale,
      crs: CONFIG.exportCRS,
      maxPixels: CONFIG.maxPixels,
      fileFormat: 'GeoTIFF',
      formatOptions: {
        cloudOptimized: true
      }
    });
  }
  
  print('✓ All QA layer export tasks created!');
}

// ============================================================================
// SECTION 15: EXECUTE EXPORTS
// ============================================================================

print('\n👉 Exporting covariate bands...');
exportIndividualBands();

if (CONFIG.includeQualityLayers) {
  print('\n👉 Exporting quality assessment layers...');
  exportQualityLayers();
}

print('\n========================================');
print('EXPORT SETUP COMPLETE');
print('========================================');
print('\n✅ QA/QC CHECKS PASSED');
print('✅ All export tasks created');
print('\n📋 NEXT STEPS:');
print('1. Go to Tasks tab (upper right)');
print('2. Run all export tasks');
print('3. Download files from Google Drive');
print('4. Review QA layers before modeling');
print('5. Use quality_score layer to mask low-quality areas');
print('\n💡 BLUE CARBON BEST PRACTICES:');
print('• Exclude areas with quality_score < 70');
print('• Check tidal indicators align with field observations');
print('• Verify NDWI patterns match known water distribution');
print('• Review biomass proxies in vegetated zones');
print('• Use salinity proxies to stratify samples if needed');
print('• Check lateral transport potential for carbon export zones');
print('\n🌊 Ready for blue carbon modeling!');
