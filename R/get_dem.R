#'# USER Topographic Correction function: Cosine, CCorrection and Minnaert
#'## ASTGTM2_S12E014_dem.tif ASTER GDEM For Angola (Kumbira)
#'## Read and Crop ASTER or SRTM DEM
#'### !!! REVER resample do DEM antes de obter slope e aspect !!!
f_readDEM <- function(elev = 'userdemname', unit = 'radians', roi = mask.ae,
                      lon = lon, lat = lat){
  vdem <- pmatch(elev, i.dem)
  if (is.na(vdem)) 
    stop("invalid dem")
  if (vdem == -1) 
    stop("unnavailable or typo error")
  i.dfile <- 'userdemname' 
  i.dtm <- raster(file.path(dir.userdem, i.dfile),
                    package = "raster")
  stopifnot(is.na(projection(i.dtm)) != TRUE)
  ## Extended Area for crop and reproject
  stopifnot(class(roi)[1] %in% c('Extent', 'RasterLayer', "SpatialPolygons",
                                 'SpatialPolygonsDataFrame'))
  if(class(roi)[1] == 'Extent'){
    i.aepol <- as(roi, 'SpatialPolygons')
    proj4string(i.aepol) <- CRS(proj4string(mask.ae)) # Assign projection
  } else if(class(roi)[1] == 'RasterLayer'){
    i.aepol <- as(extent(roi), 'SpatialPolygons')
    proj4string(i.aepol) <- CRS(proj4string(mask.ae)) # Assign projection   
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
  i.dem_p <- projectRaster(stckdem, crs = CRS(proj4string(mask.ae)),
                           res = 30, method ='ngb')
  i.dem_p <- crop(i.dem_p, mask.ae)
  i.dem_pr <- resample(i.dem_p, mask.ae, method = "ngb")
  ### Resample to match Study Site extent (and Landsat crop images)
  stopifnot(compareRaster(i.dem_pr, mask.ae)) # Evaluate rasters 
  i.dem_pr # Return DEM for the AE extent as defined by v.ae
}
#'## Run it to create the DEM for the AE --
#'## SRTM demands a lat, lon to locate the tile
dem_ae <- f.ReadDEM(elev = 'userdem', unit = 'radians', roi = mask_ae, lon = 15, lat = -12)

# Export ROI DEM
writeRaster(dem.ae[[1]], filename = file.path(dir.work, dir.fun, dir.tif, 'dem_ngb_ae_30m'),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE)
