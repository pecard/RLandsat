#' Apply ROI crop to DOS1 bands
#' Song et al 2000: DOS1 Works just fine for land change detection
#' ! uses substrBand function
f_stkDOS1 <- function(roi = roi){
  bands <- list.files(file.path(dir.work, dir.fun), full.names = T,
                      pattern = '.TIF$') 
  stopifnot(length(bands) != 0)
  i.stk <- stack(bands)
  i.stk <- crop(i.stk, roi)
  names(i.stk) <- paste0('b', f_substrBand(bands), '_ae')
  return(i.stk)
}

stk_dos1 <- f_stkDOS1(roi = aeframe[1, ])
#stk_dos1mar2014 <- f.stkDOS1(roi = roi)
plot(stk_dos1)

