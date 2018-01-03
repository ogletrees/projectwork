// LandSat data, TOA dataset. Filter to 2015 May - Sept
var landsat = ee.ImageCollection("LANDSAT/LC8_L1T_TOA")
    .filterDate('2015-05-01', '2016-09-30')
// City bounds
var fc = ee.FeatureCollection("users/ssogletree/cities")
var city = fc.select(['city_st'])

// calculate NDVI, mapping over image collection
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

  
// apply mask    
var wmask = val_NDVI.mask(elevation.neq(0))

// Map over feature collection, get the mean NDVI
var meancityndvi = wmask.reduceRegions({
  collection: city,
  reducer: ee.Reducer.mean().setOutputs(['mean_ndvi']).combine({
		reducer2: ee.Reducer.median().setOutputs(['median_ndvi']),
    sharedInputs: true
  }),
  scale: 30 // 30 meters
});

// Print the first to check, just the first 6 elements
// print(meancityndvi.toList(6))

// drop .geo
var ndviOut = meancityndvi.select(['.*'], null, false);

/* export to google drive */
 Export.table.toDrive({
  collection: ndviOut, 
  description: 'toa_city15_ndvi_mm_20180103', 
  folder: 'RemoteSensingWork', 
//fileNamePrefix: , 
  fileFormat: 'CSV'
  }); 

print('done')
