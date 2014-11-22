#' Build Mask Raster used to crop images to ROI area -------------------------------
#'# Mask file must be created. It is a mandatory step of the process
#'# considering the way it was setup
#'## 1st: Read a single Band to get the desired extent based on satellite images
#'## Change function arguments for the ROI and polygon to apply mask
band <- raster(file.path(dir.work, dir.landsat,
                         grep(".tif$",
                              list.files(file.path(dir.work, dir.landsat),
                                         all.files = F),
                              ignore.case = TRUE, value = TRUE)[1]),
               package = "raster", values = F)

ae <- readOGR(dsn = 'S:/Bissau/bijagos_landcover/vetor',
              layer = 'jvieira')
proj4string(ae) <- p.wgs84
ae <- spTransform(ae, p.utm28n)

f_createRoiMask <- function(maskpoly = ae, maskvalue = NA, band = band){
  i.band <- band
  i.band <- raster::setValues(i.band, rep(1, ncell(i.band)))
  #dataType(band) # Must be INT2U for Landsat 8. Range of Values: 0 to 65534
  if(is.na(i.band@crs)) stop('Image miss crs information')
  #stopifnot(!is.na(i.band@crs)) # Check raster for a projection
  ## Check polyg for a projection
  if(is.na(sp::proj4string(maskpoly))) stop('polygon miss crs information')
  ## Create Extent object from ae shapefile
  ## EPSG and reprojection of polygon
  ###if(f_epsgcode(maskpoly) != f_epsgcode(i.band))
  maskpoly <- rgdal::spTransform(maskpoly, CRS(proj4string(i.band)))
  #i.roi <- extent(maskpoly)
  # Crop Landsat Scene to AE extent
  #band.1 <- i.band # Raster AE: resolucao 30m (Landast)
  #band.1[] <- 1 # Defalt value
  i.bandae <- raster::crop(i.band, maskpoly) # Crop band to AE Extent
  #i.bandae <- crop(i.band, maskpoly) # Crop band to AE Extent
  ## 2nd: Create the Mask raster to the croped band extent
  #ae.r <- i.bandae # Raster AE: resolucao 30m (Landast)
  #ae.r[] <- 1 # Defalt value
  ## Overlay AE poly to AE Extent raster
  ## Mask will have 1 and NA values
  msk.ae <- raster::mask(i.bandae, maskpoly, updatevalue = NA)
  #dataType(mask_ae) <- "INT1U" 
  ## Evaluate rasters
  stopifnot(compareRaster(msk.ae, i.bandae)) 
  return(msk.ae)
}

mask_ae <- f_createRoiMask(maskpoly = ae, maskvalue = NA, band = band)

plot(mask_ae); summary(mask_ae)
writeRaster(mask.ae, filename = file.path(dir.work, dir.landsat, dir.geotif,
                                          "mask_aeframe1.asc"),
            overwrite = T)
