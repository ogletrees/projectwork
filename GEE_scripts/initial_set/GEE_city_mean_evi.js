// LandSat data
var landsat = ee.ImageCollection("LANDSAT/LC8_L1T_TOA") //var LSTOA = ee.ImageCollection("LANDSAT/LC8_L1T_TOA")
    .filterDate('2015-05-01', '2016-09-01')
// City bounds
var city = ee.FeatureCollection("users/ssogletree/cities")

// calculate EVI
var scaled = (function(image) {
  var exp = image.expression('2.5 * ((b5-b4) / (b5 + 6 * b4-7.5 * b2 + 1))',{ 
	b2: image.select("B2"),   
    b5: image.select("B5"),
    b4: image.select("B4")}).rename('EVI');
  return(exp);
  });
var withEVI = landsat.map(scaled); // this still is multiple images, could collapse and get max, like NDVI
// Make a "greenest" pixel composite.
var EVImax = withEVI.qualityMosaic('EVI');


// Get just the EVI value band
var val_EVI = EVImax.select('EVI')

// Get elevation to mask 0
var elevation = ee.Image("USGS/SRTMGL1_003") // elevation data
var zones = ee.Image(0)
//    .where(elevation.gt(200), 200)
//    .where(elevation.gt(400), 400)
//    .where(elevation.gt(600), 600)
//    .mask(elevation.neq(0))
//    .clip(region)  
  
// apply mask    
var wmask = val_EVI.mask(elevation.neq(0))


var mask = wmask.lt(2);
var maskedComposite = wmask.updateMask(mask);


// Map over feature collection. 
var cityevi = wmask.reduceRegions({
  collection: city,
  reducer: ee.Reducer.mean(),
  scale: 30 // 30 meters
});

// Print the first to check, just the first 6 elements
// print(cityevi.toList(6))

// drop .geo
var eviOut = cityevi.select(['.*'], null, false);

// export to google drive
Export.table.toDrive({
  collection: eviOut, 
  description: 'city15_evi_mean_20180101', 
  folder: 'RemoteSensingWork', 
  //fileNamePrefix: , 
  fileFormat: 'CSV'
  }); 
