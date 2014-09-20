#' Apply ROI crop to DOS1 bands
f.stkDOS1 <- function(roi = roi){
  bands <- list.files(file.path(dir.work, dir.fun), full.names = T,
                      pattern = '.TIF$')  
  i.stk <- stack(bands)
  i.stk <- crop(i.stk, roi)
  names(i.stk) <- paste0('b', substrBand(bands), '_ae')
  return(i.stk)
}

stk_dos1 <- f.stkDOS1(roi = roi)
stk_dos1mar2014 <- f.stkDOS1(roi = roi)
plot(stk_dos1)
