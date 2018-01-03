// LandSat data
var landsat = ee.ImageCollection("LANDSAT/LC8_L1T")
    .filterDate('2015-01-01', '2016-01-01')
// City bounds
var city = ee.FeatureCollection("users/ssogletree/cities")

// calculate SAVI
var scaled = (function(image) {
  var exp = image.expression('((b5 - b4) / (b5 + b4 + .5))* 1.5',{    
    b5: image.select("B5"),
    b4: image.select("B4")}).rename('SAVI');
  return(exp);
  });
var withSAVI = LS_15.map(scaled); // this still is multiple images, could collapse and get max, like NDVI
// Make a "greenest" pixel composite.
var saviest = withSAVI.qualityMosaic('SAVI');


// Get just the SAVI value band
var val_SAVI = saviest.select('SAVI')

// Get elevation to mask 0
var elevation = ee.Image("USGS/SRTMGL1_003") // elevation data
var zones = ee.Image(0)
//    .where(elevation.gt(200), 200)
//    .where(elevation.gt(400), 400)
//    .where(elevation.gt(600), 600)
//    .mask(elevation.neq(0))
//    .clip(region)  
  
// apply mask    
var wmask = val_SAVI.mask(elevation.neq(0))

// Map over feature collection. 
var citysavi = wmask.reduceRegions({
  collection: city,
  reducer: ee.Reducer.stdDev().combine({
    reducer2: ee.Reducer.minMax(),
    // reducer2: ee.Reducer.stdDev(),
    sharedInputs: true
  }),
  scale: 30 // 30 meters
});

// Print the first to check, just the first 6 elements
// print(citysavi.toList(6))

// export to google drive
Export.table.toDrive({
  collection: citysavi, 
  description: 'city15_savi_mean_20180101', 
  folder: 'RemoteSensingWork', 
  //fileNamePrefix: , 
  fileFormat: 'CSV'
  }); 
