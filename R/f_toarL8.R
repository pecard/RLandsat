## Convert DN to TOA Top of Athmosphere or Planetary Reflectances

f.ToarL8 <- function(roi = roi){
  toa_stk <- stack()
  bands <- list.files(file.path(dir.work, dir.fun), full.names = T, pattern = '.TIF$')  
  for (i in 1:length(bands)) {
    message('processing: ', substrRight(bands[i], 28))
    # Name
    i.fname <- paste0('b', substrBand(bands[i]), '_ae')
    # Read Geotif raster
    i.tmp <- raster(bands[i], package = "raster", dataType = 'FLT4S')
    # Crop and apply mask
    i.crop <- crop(i.tmp, roi)
    # uncorrected TOA Reflectance with Topographic correction with mask overlay
    # uncorrected TOA reflectance
    i.toar <- (i.crop *
                 as.numeric(mtl[grep(paste0("REFLECTANCE_MULT_BAND_",
                                            substrBand(bands[i])),
                                     mtl$GROUP), 2]) +
                 as.numeric(mtl[grep(paste0("REFLECTANCE_ADD_BAND_",
                                            substrBand(bands[i])),
                                     mtl$GROUP), 2])) 
    # Correct for Sun Elevation sin(Theta_SE)
    i.tse <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])*pi/180 # radians
    i.sune <- sin(i.tse)
    i.toa <- i.toar / i.sune  
    i.toa@data@names <- i.fname
    toa_stk <- addLayer(toa_stk, i.toa)
  }
  toa_stk
}

stk_toar <- f.ToarL8(roi = roi)
