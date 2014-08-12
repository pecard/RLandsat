# ==================================================================================
# Create Radimoetric and Topographic corrected files from Landsat 8 OLI
# Paulo E. Cardoso 03-03-2014
# Version v8
# R version 3.0.2 rgdal_0.8-16  raster_2.2-16 sp_1.0-14
## Utiliza dados de Digital Elevation Model (DEM) Aster (30m) ou SRTM (90m)
### ASTER GDEM: Usar o Tile ASTGTM2_S12E014
### SRTM v4.1: usar o Tile srtm_39_15
# ==================================================================================

# Packages necessarios
kpacks <- c("raster", "sp", "rgdal", 'rgeos')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

sessionInfo() # basics of used session
# R version 3.0.2 (2013-09-25)
# [1] rgeos_0.3-3   rgdal_0.8-16  raster_2.2-16 sp_1.0-14

# Folders --------------------------------------------------------------------------
## Adjust for Local work paths
dir.work <- 'mfolder' # Alterar para Disco Local
dir.rst   <- 'rst' # Criar no dir.work do disco local
dir.tif <- 'tif'  # Criar no dir.work do disco local
dir.shp <- './Shapefiles_StudySite'
dir.aster <- './aster' # Alterar para o Local Folder
dir.srtm <- '/.srtm' # Alterar para o Local Folder
### Data das Landsat. Define as many as necesary.
### dir.fun must be changed for all analysis

dir.landsat <- 'LC81810682014157LGN00' # Folder with Landast 8 TIF files
#' Satelite parameters and data ----------------------------------------------------
## All subsequente analysis is for a single date
dir.fun <- dir.landsat # Change here to perform all subsequent analysis!

#'# Create a tif folder for raster and images outputs
dir.create(file.path(dir.work, dir.fun, dir.tif))

#'# Saving and Loading R data session ------------------------------------------------
save.image(file.path(dir.work, dir.fun, paste0(dir.fun,'.RData')))
load(file.path(dir.work, dir.fun, paste0(dir.fun,'.RData'))

#'# Landsat 8 Scene Metadata: MTL File ---------------------------------------------
#'## Metadata file contains scene acquisition details and correction parameters
#'## Unzip the MTL file from landsat L8 zipped .tar file
#'## MTL must be with Landsat TIF files, at the exact same folder
mtl <- read.delim(file.path(dir.work, dir.fun,  
                            grep('_MTL.txt',
                                 list.files(file.path(dir.work, dir.fun),
                                            all.files = F),
                                 ignore.case = TRUE, value = TRUE)),
                  sep = '=', stringsAsFactors = F)
mtl[grep("DATE_ACQUIRED", mtl$GROUP), 2]
mtl[grep("LANDSAT_SCENE_ID", mtl$GROUP), 2]

#' Local parameters ----------------------------------------------------------------
#'# Projections
#p.utm28n <- CRS("+init=epsg:32628") # UTM 28N Landsat Images
p.utm33n <- CRS("+init=epsg:32633") # UTM 33N Landsat Images
p.utm33s <- CRS("+init=epsg:32733") # UTM 33S Landsat Images
p.wgs84 <- CRS("+init=epsg:4326") # WGS84 Long Lat

#' Study site frame extent ----------------------------------------------------------
#'## Create rectangular area for image cropping
#'## A projected spatialpolydf will be created and projected to utm33N
ae <- readOGR(dsn = file.path(dir.shp), layer = 'layername')
proj4string(ae) <- p.utm33s # Asign projection WGS84
if(!is.projected(ae)) ae <- spTransform(ae, p.utm33n)
ae <- spTransform(ae, p.utm33n) # if UTM projection is South
#'## Create Extent from ae or provide another Shape for a different extent 
roi <- extent(ae) # a rectangular area covering ae polygon extent

#'---------------TEST ONLY ---------------------------
#'## Smaller subarea of ROI for test purposes --------
roi2 <- as(roi - c(3000, 7000), 'SpatialPolygons')
proj4string(roi2) <- p.utm33n
ae2 <- gIntersection(ae, roi2, byid = TRUE)
plot(roi2, axes = T);plot(ae2, add = T)
#'# Morro Moco Study Area --
roimoco <- extent(c(15.15, 15.18, -12.45, -12.40))
moco <- as(roimoco, 'SpatialPolygons')
proj4string(moco) <- p.wgs84 # Asign projection WGS84
mocoutm <- spTransform(moco, p.utm33n)
#'# --------------------------------------------------
#'----------------------------------------------------

#' Build Mask Raster used to crop images to ROI area --------------------------------
#'# Mask file must be created. It is a mandatory step of the process
#'# considering the way it was setup
#'## 1st: Read a single Band to get the desired extent based on satellite images
#'## Change function arguments for the ROI and polygon to apply mask
f.CreateRoiMask <- function(x = x, roi = roi2, maskpoly = ae2){
  x <- grep(".tif$", list.files(file.path(dir.work, dir.fun), all.files = F),
            ignore.case = TRUE, value = TRUE)[1] 
  i.band <- raster(file.path(dir.work, dir.fun, x),
                   package = "raster")
  #dataType(band) # Must be INT2U for Landsat 8. Range of Values: 0 to 65534
  stopifnot(!is.na(i.band@crs)) # Check projection
  # Create Extent object from ae shapefile
  if(is.null(roi)){
    i.roi <- extent(ae2)
  } else i.roi <- extent(roi)
  # Crop Landsat Scene to AE extent
  i.bandae <- crop(i.band, i.roi) # Crop band to AE Extent
  ## 2nd: Create the Mask raster to the croped band extent
  ae.r <- i.bandae # Raster AE: resolucao 30m (Landast)
  ae.r[] <- 1 # Defalt value
  ## Overlay AE poly to AE Extent raster
  ### Mask will have 1 and NA values
  msk.ae <- mask(ae.r, maskpoly, updatevalue=NA)
  #dataType(mask_ae) <- "INT1U" 
  ## Evaluate rasters
  stopifnot(compareRaster(msk.ae, i.bandae)) 
  msk.ae
}

#'## Run the function
#'## Mask will be a c(1, NA) rasterLayer
if(exists('mask.ae')) remove(mask.ae)
mask.ae <- f.CreateRoiMask(x = x, roi = roi, maskpoly = ae)

plot(mask.ae); summary(mask.ae)
writeRaster(mask.ae, filename = file.path(dir.work, dir.landsat, dir.tif,
                                          "mask_ae.asc"),
            overwrite = T)

#'## USER Topographic Correction function: Cosine, CCorrection and Minnaert
#'### Read and Crop ASTER or SRTM DEM
#'### !!! REVER resample do DEM antes de obter slope e aspect !!!
f.ReadDEM <- function(elev = 'srtm', unit = 'radians', roi = mask.ae, lon = lon, lat = lat){
  i.dem <- c('aster', 'srtm', 'SRTM', 'user')
  vdem <- pmatch(elev, i.dem)
  if (is.na(vdem)) 
    stop("invalid dem")
  if (vdem == -1) 
    stop("unnavailable or typo error")
  if(vdem == 1){
    i.dfile <- 'ASTGTM2_S12E014_dem.tif' # For Angola (Kumbira)
    i.dtm <- raster(file.path(dir.aster, i.dfile),
                    package = "raster")
  } else if(vdem == 2){
    i.dfile <- 'srtm_39_15.tif'
    i.dtm <- raster(file.path(dir.srtm, i.dfile),
                    package = "raster")
  } else if(vdem == 3){
    
    i.dtm <- getData('SRTM', lon = lon, lat = lat)
  }
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
dem.ae <- f.ReadDEM(elev = 'srtm', unit = 'radians', roi = mask.ae, lon = 15, lat = -12)
# Export ROI DEM
writeRaster(dem.ae[[1]], filename = file.path(dir.work, dir.fun, dir.tif, 'dem_ngb_ae_30m'),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE)

#'# Functions for Radiometric and Topographic calibration Landsat 8 ------------------
### According to http://www.gisagmaps.com/landsat-8-atco-guide/, DOS may perform
#### better under some circumnstances.
#### More info at:
#### http://landsat.usgs.gov/Landsat8_Using_Product.php
#### http://landsat.usgs.gov/L8_band_combos.php : Band References
#### ESUN and OLI: http://landsat.usgs.gov/ESUN.php
## DN to uncorrected TOA reflectance: planetary reflectance
## Define the parent frame from where objects will be called

## Convert DN to TOA Top of Athmosphere or Planetary Reflectances
f.ToarL8 <- function(x=x, i=i){
  # uncorrected TOA reflectance
  i.toar <- (x * as.numeric(mtl[grep(paste0("REFLECTANCE_MULT_BAND_", i),
                                     mtl$GROUP), 2]) +
               as.numeric(mtl[grep(paste0("REFLECTANCE_ADD_BAND_", i),
                                   mtl$GROUP), 2])) 
  # Correct for Sun Elevation sin(Theta_SE)
  i.tse <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])*pi/180 # radians
  i.sune <- sin(i.tse)
  i.toa <- i.toar / i.sune  
  i.toa
}

#'# for test purpose only. Do not run outside main function
i.toa <- f.ToarL8(x=i.crop, i=bands[i])
plot(i.toa)

#' Topographic correction -----------------------------------------------------------
# Lu et al 2008. Pixel-based Minnaert Correction..
# Vanonckelen et al 2013. The effect of atmospheric and topographic...

f.TopoCor <- function(x = x, i = i, method = 'minnaert',
                      slope, aspect, il.ae, sun.e, sun.z, sun.a) {
  message('Images will be corrected to planetary TOA reflectances')
  METHODS <- c('none', 'cosine', 'ccorrection', 'minnaert')
  method <- pmatch(method, METHODS)
  itoa <- f.ToarL8(x = x,  i = i)
  if(method == 1){
    message('Message: No Topo correction will be applied')
    xout <- itoa
  } else if(method == 2){
    message('Message: cosine will be applied')
    xout <- itoa * (cos(sun.z)/il.ae)
  } else if (method == 3) {
    message('Message: c_correction will be applied')
    subspl <- sample(1:ncell(itoa), floor(ncell(itoa)*0.50), rep = F)
    band.lm <- lm(as.vector(itoa[subspl]) ~ as.vector(il.ae[subspl]))$coefficients
    #band.lm <- lm(as.vector(i.toa) ~ as.vector(il.ae))$coefficients
    C <- band.lm[1]/band.lm[2]
    xout <- itoa * (cos(sun.z) + C)/(il.ae + C)
  } else if(method == 4) {
    message('Message: Minnaert will be applied')
    targetslope <- atan(0.05)
    if (all(itoa[slope >= targetslope] < 0, na.rm = TRUE)) {
      K <- 1
    } else {
      K <- data.frame(y = as.vector(itoa[slope >= targetslope]), 
                      x = as.vector(il.ae[slope >= targetslope])/cos(sun.z))
      K <- K[!apply(K, 1, function(x) any(is.na(x))), ]
      K <- K[K$x > 0, ]
      K <- K[K$y > 0, ]
      K <- lm(log10(K$y) ~ log10(K$x))
      K <- coefficients(K)[[2]]
      if (K > 1) 
        K <- 1
      if (K < 0) 
        K <- 0
    }
    xout <-(itoa * cos(sun.z))/((il.ae * cos(sun.z))^K)
  }
  xout
}

#'# for test purpose only. Do not run outside main function
ltest <- f.TopoCor(x = i.crop, i = 1, method = 'minnaert') # Test only

# ----------------------------------------------------------------------------------
# Main Function to create radiometric corrected Files ------------------------------
# For original Landsat Product only
# Function arguments:
## write: (TRUE/FALSE): Export RST raster file
## demcorr: ('none', cosine, ccorrection, minnaert). Topographic correction algorithm 
## mask: (TRUE/FALSE) Aply a mask to the ROI extent. Mask will be a polygon.
### Resulting in a 1/NA rasterLayer.
## dem: rasterStack with DEM, slope and aspect layers.

f.idrisidata <- function(write = F, demcorr = 'none', mask = T,
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

# RUN function to get rasterStack with processed bands -----------------------------
l8files <- f.idrisidata(write = T, wrformat = 'ENVI', demcorr = 'minnaert', mask = T)

# Export rasterStack to a BSQ TIF File
writeRaster(l8files, filename=file.path(dir.work, dir.landsat, dir.tif,
                                        "stack201400606.tif"),
            options="INTERLEAVE=BAND", overwrite=TRUE)
# Plot it
plotRGB(l8files, 6, 4, 2, stretch = 'hist')
