#' Apply mask to image stack -------------------------------------------------------
#' 
f_applmask <- function(stk = stk, mask = mask){
  stk_mask <- stk * mask
  names(stk_mask) <- names(stk)
  stk_mask
}

stk_mask <- f_applmask(stk = stk_dos1, mask = mask_ae)

plot(stk_mask[[1]])
click(stk_mask[[1]])
summary(stk_mask)
