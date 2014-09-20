#' Build Mask Raster used to crop images to ROI area -------------------------------
#'# Mask file must be created. It is a mandatory step of the process
#'# considering the way it was setup
#'## 1st: Read a single Band to get the desired extent based on satellite images
#'## Change function arguments for the ROI and polygon to apply mask
f.CreateRoiMask <- function(roi = roi, maskpoly = ae, maskvalue = NA){
  x <- grep(".tif$", list.files(file.path(dir.work, dir.fun), all.files = F),
            ignore.case = TRUE, value = TRUE)[1] 
  i.band <- raster(file.path(dir.work, dir.fun, x),
                   package = "raster")
  ##dataType(band) # Must be INT2U for Landsat 8. Range of Values: 0 to 65534
  stopifnot(!is.na(i.band@crs)) # Check raster for a projection
  stopifnot(!is.na(proj4string(maskpoly))) # Check polyg for a projection
  ## Create Extent object from ae shapefile
  if(is.null(roi)){
    i.roi <- extent(maskpoly)
  } else i.roi <- extent(roi)
  # Crop Landsat Scene to AE extent
  i.bandae <- crop(i.band, i.roi) # Crop band to AE Extent
  ## 2nd: Create the Mask raster to the croped band extent
  ae.r <- i.bandae # Raster AE: resolucao 30m (Landast)
  ae.r[] <- 1 # Defalt value
  ## Overlay AE poly to AE Extent raster
  ## Mask will have 1 and NA values
  msk.ae <- mask(ae.r, maskpoly, updatevalue=NA)
  #dataType(mask_ae) <- "INT1U" 
  ## Evaluate rasters
  stopifnot(compareRaster(msk.ae, i.bandae)) 
  msk.ae
}

mask_ae <- f.CreateRoiMask(roi = roi, maskpoly = ae, maskvalue = NA)

plot(mask_ae); summary(mask_ae)
writeRaster(mask.ae, filename = file.path(dir.work, dir.landsat, dir.tif,
                                          "mask_ae.asc"),
            overwrite = T)
