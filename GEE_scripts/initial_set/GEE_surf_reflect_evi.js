// Using surface reflectance to obtain EVI
var LS_SR = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR"),
    point = ee.Geometry.Point([-121.96197509765625, 37.37015718405753]);

var LS =  ee.Image(LS_SR
	.filterBounds(point)
	.filterDate('2015-05-01', '2015-09-30')    
    .sort('CLOUD_COVER')
    .first());
//print(LS)

Map.addLayer(LS, {bands: ['B4']}, 'good composite');

var bb2 = LS.select("B2").multiply(.0001)
Map.addLayer(bb2, {min:0, max: .5})

var t2 = LS.select("B10").multiply(.1)
var t3 = t2.subtract(273).multiply(1.8)
var t4 = t3.add(32)
Map.addLayer(t4, {min:0, max:120})

var ll = ee.ImageCollection(LS_SR)
    .filterDate('2015-05-01', '2015-09-30')


// calculate EVI, consider scaling in expression
var scaled = (function(image) {
  var exp = image.expression('2.5 * ((b5-b4) / (b5 + 6 * b4-7.5 * b2 + 1))',{ 
	b2: image.select("B2").multiply(.0001),   
    b5: image.select("B5").multiply(.0001),
    b4: image.select("B4").multiply(.0001)}).rename('EVI');
  return(exp);
  });
var withEVI = ll.map(scaled); // this still is multiple images, could collapse and get max, like NDVI
// Make a "greenest" pixel composite.
var EVImax = withEVI.qualityMosaic('EVI');


// Get just the EVI value band
var val_EVI = EVImax.select('EVI')

var vegPalette = ['red', 'blue', 'yellow', 'green'];
Map.addLayer(val_EVI, {min: -1, max: 1, palette: vegPalette}, 'EVI');
