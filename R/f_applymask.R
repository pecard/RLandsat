#' Apply mask to image stack -------------------------------------------------------
#' 
f.applmask <- function(stk = stk, mask = mask){
  stk_mask <- stk * mask
  names(stk_mask) <- names(stk)
  stk_mask
}

stktoar_mskmar2014 <- f.applmask(stk = stk_dos1mar2014, mask = mask_ae)
stktoar_msk <- f.applmask(stk = stk_dos1, mask = mask_ae)
