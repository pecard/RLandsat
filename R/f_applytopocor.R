#' ----------------------------------------------------------------------------------
#' Main Function to create radiometric corrected Files
#' For original Landsat Product only
#' Function arguments:
#' write: (TRUE/FALSE): Export RST raster file
#' demcorr: ('none', cosine, ccorrection, minnaert). Topographic correction algorithm 
#' mask: (TRUE/FALSE) Aply a mask to the ROI extent. Mask will be a polygon.
#' Resulting in a 1/NA rasterLayer.
#' dem: rasterStack with DEM, slope and aspect layers.
#' Topographic correction -----------------------------------------------------------
#' Lu et al 2008. Pixel-based Minnaert Correction..
#' Vanonckelen et al 2013. The effect of atmospheric and topographic...
#' Available methods: 'none', 'cosine', 'ccorrection', 'minnaert'

f_topoCor <- function(stk = stk, method = 'minnaert', dem = dem.ae) {
  stk_topo <- raster::stack()
  METHODS <- c('none', 'cosine', 'ccorrection', 'minnaert')
  method <- pmatch(method, METHODS)
  #' SUN Parameters ---
  ##' Sun elev in radians
  sun.e <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2]) * (pi/180) 
  ##' Sun Zenit in radians
  sun.z <- (90 - as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])) * (pi/180)
  ##' Sun Azimuth
  sun.a <- as.numeric(mtl[grep("SUN_AZIMUTH", mtl$GROUP), 2])* (pi/180)
  #' DEM Parameters for Topo Correction ---
  il.epsilon <- 1e-06
  #' DEM slope and Aspect
  slope <- raster::dem[['slope']]
  aspect <- raster::dem[['aspect']]
  il.ae <- cos(slope) * cos(sun.z) + sin(slope) *
    sin(sun.z) * cos(sun.a - aspect)
  if(method == 1){
    message('Message: No Topo correction will be applied')
    stk_out <- stk
  } else if(method == 2){
    message('Message: cosine will be applied')
    stk_out <- stk * (cos(sun.z)/il.ae)
  } else if (method == 3) {
    message('Message: c_correction will be applied')
    if (samp == 1){
      subspl <- 1:ncell(stk[[1]])
    } else{
      subspl <- sample(1:ncell(stk), floor(ncell(stk)* 0.50), rep = F)}
    for(i in 1:nlayers(stk)){      
      bi <- stk[[i]]
      band.lm <- lm(bi[subspl] ~ il.ae[subspl])$coefficients
      #band.lm <- lm(as.vector(i.toa) ~ as.vector(il.ae))$coefficients
      C <- band.lm[1]/band.lm[2]
      xout <- bi * (cos(sun.z) + C)/(il.ae + C)
      stk_topo <- addLayer(stk_topo, xout)
      stk_out <- stk_topo
    }
  } else if(method == 4) {
    message('Message: Minnaert correction will be applied')
    targetslope <- atan(0.05)
    for(i in 1:nlayers(stk)){
      bi <- stk[[i]]
      if (all(bi[slope >= targetslope] < 0, na.rm = TRUE)) {
        K <- 1
      } else {
        K <- data.frame(y = as.vector(bi[slope >= targetslope]), 
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
      xout <-(bi * cos(sun.z))/((il.ae * cos(sun.z))^K)
      stk_topo <- addLayer(stk_topo, xout)
      stk_out <- stk_topo
    }
  }
  stk_out
}

stk_topoc <- f_TopoCor(stk = stk_dos1, method = 'minnaert', dem = dem.ae) # Test only
plot(ltest)
