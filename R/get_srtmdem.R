#'## USER Topographic Correction function: Cosine, CCorrection and Minnaert
#'### Read and Crop ASTER or SRTM DEM
#'### !!! REVER resample do DEM antes de obter slope e aspect !!!
#'### Ensure that when using a extent object there is a mask_ae object to get proj
#'### parameters from.
f_getSRTMDEM <- function(unit = 'radians', roi = mask_ae, lon = lon, lat = lat){
  i.dtm <- getData('SRTM', lon = lon, lat = lat)
  proj.i <- CRS(proj4string(roi))
  ## Extended Area for crop and reproject
  stopifnot(class(roi)[1] %in% c('RasterLayer', "SpatialPolygons",
                                 'SpatialPolygonsDataFrame'))
  if(class(roi)[1] == 'RasterLayer'){
    message("using a RasterLayer")
    #proj.i <- CRS(proj4string(roi))
    i.aepol <- as(extent(roi), 'SpatialPolygons')
    proj4string(i.aepol) <- proj.i # Assign projection   
  } else i.aepol <- roi
  # Extent for WGS84 with a buffer
  i.ae2 <- extent(spTransform(i.aepol, CRS(proj4string(i.dtm)))) + 0.005
  # CROP DEM
  i.dtm <- crop(i.dtm, i.ae2) # Crop to expanded area
  ### Calculate slope and aspect
  slpspct <- raster:::terrain(i.dtm, opt=c('slope', 'aspect'),
                              unit=unit)
  stckdem <- stack(i.dtm, slpspct)
  ### Change Projection to Landsat UTM adjusting for 30m resolution
  i.dem_p <- projectRaster(stckdem, crs = proj.i,
                           res = 30, method ='ngb')
  i.dem_p <- crop(i.dem_p, roi)
  i.dem_pr <- resample(i.dem_p, roi, method = "ngb")
  ### Resample to match Study Site extent (and Landsat crop images)
  stopifnot(compareRaster(i.dem_pr, roi)) # Evaluate rasters 
  i.dem_pr # Return DEM for the AE extent as defined by v.ae
}
#'## Run it to create the DEM for the AE --
#'## SRTM demands a lat, lon to locate the tile
dem.ae <- f.getSRTMDEM(unit = 'radians', roi = mask_ae, lon = 14, lat = -11)

#' Export ROI DEM
writeRaster(dem.ae[[1]], filename = file.path(dir.work, dir.landsat, dir.tif, 'dem_ngb_ae_30m'),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE)
