# PCA on Bands 1:6 and retain first 3 Components with > 99% expl var ---------------
f.Pca <- function(stk = stk, corr = F, comps = 2){
  pcalist <- list()
  xdf <- as.data.frame(stk)
  pca1 <-  princomp(xdf, cor = corr) 
  pcalist[[1]] <- pca1
  pcastk <- stack()
  for(i in 1:comps){
    pcax <- matrix(pca1$scores[ , i], nrow = nrow(stk), ncol = ncol(stk),
                   byrow = TRUE)
    pcax <- raster(pcax, xmn = stk@extent@xmin, ymn = stk@extent@ymin,
                   xmx = stk@extent@xmax, ymx = stk@extent@ymax,
                   crs = CRS(proj4string(mask_ae)))
    pcastk <- addLayer(pcastk, pcax)
  }
  pcalist[[2]] <- pcastk
  return(pcalist)
}
#'# Provide the stack object for analysis
pca_obj <- f.Pca(stk = stk_kmeans, corr = F, comps = 2)
stkpca <- pca_obj[[2]]
plot(pca_obj[[1]])

summary(pca_obj[[1]])
