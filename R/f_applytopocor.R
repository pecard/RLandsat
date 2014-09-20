# ----------------------------------------------------------------------------------
# Main Function to create radiometric corrected Files ------------------------------
# For original Landsat Product only
# Function arguments:
## write: (TRUE/FALSE): Export RST raster file
## demcorr: ('none', cosine, ccorrection, minnaert). Topographic correction algorithm 
## mask: (TRUE/FALSE) Aply a mask to the ROI extent. Mask will be a polygon.
### Resulting in a 1/NA rasterLayer.
## dem: rasterStack with DEM, slope and aspect layers.

f.topocor <- function(write = F, demcorr = 'none', mask = T,
                     dem = dem.ae, wrformat = 'RST') {
  i.allfiles <- list.files(file.path(dir.work, dir.fun), all.files = F)
  # List of TIF files at dir.fun folder
  i.listtif <- grep(".tif$", i.allfiles, ignore.case = TRUE, value = TRUE) 
  bands <- as.numeric(substr(i.listtif, (nchar(i.listtif) - 4),
                             (nchar(i.listtif) - 4)))
  i.stk.toar <- stack()
  #i.stk.toart <- stack() # topocorr
  i.lstk <- list()
  # SUN Parameters ---
  ## Sun elev in radians
  sun.e <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2]) * (pi/180) 
  ## Sun Zenit in radians
  sun.z <- (90 - as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])) * (pi/180)
  ## Sun Azimuth
  sun.a <- as.numeric(mtl[grep("SUN_AZIMUTH", mtl$GROUP), 2])* (pi/180)
  # DEM Parameters for Topo Correction ---
  #  if(topocor != 'none'){
  il.epsilon <- 1e-06
  # DEM slope and Aspect
  slope <- dem[['slope']]
  aspect <- dem[['aspect']]
  il.ae <- cos(slope) * cos(sun.z) + sin(slope) *
    sin(sun.z) * cos(sun.a - aspect)
  # stopifnot(min(getValues(il.ae), na.rm = T) >= 0)
  il.ae[il.ae <= 0] <- il.epsilon
  #  }
  for (i in 1:length(bands)) {
    message(bands[i])
    # Name
    i.fname <- paste0('b',bands[i],'_ae')
    # Read Geotif raster
    i.tmp <- raster(file.path(dir.work, dir.fun, i.listtif[i]),
                    package = "raster", varname = fname, dataType = 'FLT4S')
    # Crop and apply mask
    i.crop <- crop(i.tmp, extent(mask.ae))
    # uncorrected TOA Reflectance with Topographic correction with mask overlay
    i.toar <- f.TopoCor(x = i.crop, i = bands[i], method = demcorr,
                        slope, aspect, il.ae, sun.e, sun.z, sun.a)
    if(mask == T) {
      i.toar <- i.toar * mask.ae
    } else i.toar <- i.toar
    i.toar@data@names <- i.fname  # Add band name  
    # Create Stack
    if(i < 8) {
      i.stk.toar <- addLayer(i.stk.toar, i.toar)
      #i.stk.toart <- addLayer(i.stk.toart, i.toartmsk)
    }
    # Write IDRISI raster group rgf for uncorrected TOA Reflectance
    if(write == T) {
      dire <- file.path(dir.work, dir.fun, dir.tif)
      stopifnot(file_test("-d", dire))
      ## gdal
      ##writeGDAL(as(i.l8, "SpatialGridDataFrame"),
      ##fname = "D:\\idri.rst", drivername = "RST") 
      message(wrformat, 'raster will be created for ', i.fname, ' at: ',
              file.path(dir.work, dir.fun, dir.tif))
      writeRaster(i.toar, filename = file.path(dir.work, dir.fun, dir.tif,
                                               i.fname),
                  datatype = 'FLT4S', format = wrformat, #'RST',
                  overwrite = TRUE)
      fileConn <- file(file.path(dir.work, dir.fun, dir.tif, "ae_toar.rgf"))
      writeLines(c(length(i.listtif),
                   paste0('b', bands, '_ae')),
                 fileConn)
      close(fileConn)
    }
  }
  i.stk.toar
}
