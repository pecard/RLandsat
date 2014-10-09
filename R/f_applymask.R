#' Apply mask to image stack -------------------------------------------------------
#' 
f.applmask <- function(stk = stk, mask = mask){
  stk_mask <- stk * mask
  names(stk_mask) <- names(stk)
  stk_mask
}

stk_mask <- f.applmask(stk = stk_topoc, mask = mask_ae)
plot(stk_mask)
