#' Apply ROI crop to DOS1 bands
#' Song et al 2000: DOS1 Works just fine for land change detection
#' ! call internal f_substrBand function
pathto_dos1 <- 'S:/Raster/Landsat/LC82040522014078LGN00/qgis/dos1'
f_stkDOS1 <- function(roi = ae, fpath = pathto_dos1){
  bands <- list.files(fpath, full.names = T,
                      pattern = '.TIF$') 
  stopifnot(length(bands) != 0)
  i.stk <- stack(bands)
  i.stk <- crop(i.stk, roi)
  names(i.stk) <- paste0('b', f_substrBand(bands), '_ae')
  return(i.stk)
}

stk_dos1 <- f_stkDOS1(roi = ae)
#stk_dos1mar2014 <- f.stkDOS1(roi = roi)
plot(stk_dos1)

