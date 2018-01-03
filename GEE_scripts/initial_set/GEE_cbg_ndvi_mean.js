// LandSat data
var landsat = ee.ImageCollection("LANDSAT/LC8_L1T")
    .filterDate('2015-01-01', '2016-01-01')
// City bounds
var cbg = ee.FeatureCollection("users/ssogletree/cbg_subset")

// calculate NDVI 
var ndvi = landsat.map(function(image) {
  var result = image.normalizedDifference(['B5', 'B4']).rename("ndvi")
  return image.addBands(result);
})

// Make a "greenest" pixel composite.
var greenest = ndvi.qualityMosaic('ndvi')

// Get just the NDVI value band
var val_NDVI = greenest.select('ndvi')

// Get elevation to mask 0
var elevation = ee.Image("USGS/SRTMGL1_003") // elevation data
var zones = ee.Image(0)
//    .where(elevation.gt(200), 200)
//    .where(elevation.gt(400), 400)
//    .where(elevation.gt(600), 600)
//    .mask(elevation.neq(0))
//    .clip(region)  
  
// apply mask    
var wmask = val_NDVI.mask(elevation.neq(0))

// Map over feature collection, get the mean NDVI
var meancbgndvi = wmask.reduceRegions({
  collection: cbg,
  reducer: ee.Reducer.mean(),
  scale: 30 // 30 meters
});

// Print the first to check, just the first 6 elements
// print(meancityndvi.toList(6))


// export to google drive
 Export.table.toDrive({
  collection: meancbgndvi, 
  description: 'cbg15_mean_ndvi_20180101', 
  folder: 'RemoteSensingWork', 
//fileNamePrefix: , 
  fileFormat: 'CSV'
  }); 

print('done')
