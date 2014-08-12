#'==================================================================================================
#' Unsupervised Classification with R
#'## http://blog.remote-sensing-conservation.org/unsupervised-classifcation-in-r/
#'## http://www.digital-geography.com/unsupervised-classification-of-a-landsat-image-in-r-the-whole-story-or-part-two/
#'## http://stackoverflow.com/questions/10075122/ordering-clustered-points-using-kmeans-and-r
#'
#'# Cluster with k-means -------------------------------------------------------------
#'## http://link.springer.com/chapter/10.1007/978-3-642-24466-7_7
#'## http://link.springer.com/article/10.1007/s00357-010-9049-5
#'
#'# To go further with k-means
#'## validations: http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
#'## Other sources: http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Clustering-and-Data-Mining-in-R
#'
#'# Particularly relevant: parallel processing
#'## http://www.glennklockwood.com/di/R-para.php
#'
#'# Limitations of raster::calc (from ?calc)
#'## _not_ do, for Raster object x: calc(x, function(x)scale(x, scale=FALSE))
#'## Because the mean value of each chunk will likely be different.
#'## Rather do something like m <- cellStats(x, 'mean'); x - m
#'===================================================================================

#'# Utiliza o stack l8files produzidos pela funcao Landasat_CreateIdrisiFiles_vx.R

#' ADD Vegetation Index -------------------------------------------------------------
#' x: stack object, Default will be the l8files object, with bands 1 to 7
f.VegIndex <- function(x = x, index = 'ndvi'){
  x <- x
  #istk <- x[[1:7]]
  # Vegetation Index (Lansdat 8 OLI)
  ## NDVI = (5-4)/(5+4)
  ## LSWI = (5-6)/(5+6)
  ## NBR - (5-7)/(5+7)
  i.index <- c('ndvi', 'lswi', 'nbr')
  index <- pmatch(index, i.index)
  if (is.na(index)) stop("invalid vegetation index")
  if (index == -1) stop("ambiguous index")
  if(index == 1) {
    out.ind <- (x[[5]]-x[[4]])/(x[[5]]+x[[4]])
  } else if(index == 2){
    out.ind <- (x[[5]]-x[[6]])/(x[[5]]+x[[6]])
  } else if(index == 3){
    out.ind <- (x[[5]]-x[[7]])/(x[[5]]+x[[7]])      
  }
  out.ind
}

#' Specify sensible and meaningful object names
vegind <- f.VegIndex(x = l8files, index = 'ndvi') # stack Bands + VegIndex
vegind2 <- f.VegIndex(x = l830_01_2014bija, index = 'ndvi') # stack Bands + VegIndex
vegind_dif <- vegind30_01_2014bij - vegind03_05_2013bij

plot(vegind_dif)
plot(vegind)

## Export Vegetation Index ---------------------------------------------------------
## 30-01-2014
writeRaster(vegind_dif_bij, file.path(dir.work, dir.tif30012014, dir.rst,
                                      paste0('ndvibij2014-2013.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)
writeRaster(vegind_dif_bij, file.path(dir.work, dir.tif30012014, dir.rst,
                                      paste0('ndvi2013_2014_2')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)

## 03-05-2013
writeRaster(vegind03_05_2013, file.path(dir.work, dir.tif03052013, dir.rst,
                                        paste0('ndvi03052013.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)

#' Build Stacks ---------------------------------------------------------------------
#' Replace NA's with a small real number to run kmeans.
## l8files (1,2,3,4,5,6,7) #
stkfile <- l8files # Sem NDVI
#stk20140214s <- raster::scale(stk20140214)
stkfile <-  reclassify(stkfile, matrix(c(NA, -0.01), nrow = 1))

## Com NDVI
stk30_01_2014 <- l830_01_2014[[2:7]]
stk30_01_2014 <- addLayer(stk30_01_2014, vegind_dif)
stk30_01_2014 <- raster::scale(stk30_01_2014)
stk30_01_2014 <-  reclassify(stk30_01_2014, cbind(NA, NA, -99))

# PCA on Bands 1:6 and retain first 3 Components with > 99% expl var ---------------
f.Pca <- function(x=x, cor = F){
  xdf <- as.data.frame(x)
  pca1 <-  princomp(xdf, cor=cor) 
  pcastk <- stack()
  for(i in 1:3){
    pcax <- matrix(pca1$scores[ ,i], nrow = nrow(x), ncol = ncol(x),
                   byrow = TRUE)
    pcax <- raster(pcax, xmn=x@extent@xmin, ymn=x@extent@ymin,
                   xmx=x@extent@xmax, ymx=x@extent@ymax,
                   crs = CRS(proj4string(mask.ae)))
    pcastk <- addLayer(pcastk, pcax)
  }
  pcastk
}
#'# Provide the stack object for analysis
stkpca <- f.Pca(x=stackobject, cor = F)
plot(stkpca)

#' base::kmeans ---------------------------------------------------------------------
f.Kmeans <- function(x = x, ncl = num.clss, niter.max = 5, nstarts = 5){
  xdf <- as.data.frame(x)
  #xdf <- scale(xdf)
  ikm <- kmeans(xdf, ncl, iter.max = niter.max, nstart = nstarts)
  il8m <- matrix(ikm$cluster, nrow = nrow(x), ncol = ncol(x),
                 byrow = TRUE)
  i.kraster <- raster(il8m, xmn=x@extent@xmin, ymn=x@extent@ymin,
                      xmx=x@extent@xmax, ymx=x@extent@ymax,
                      crs = CRS(proj4string(mask.ae)))
  i.kraster
}

#' Run kmeans function of selected stack object: create a ikmeans raster file with
#'# classes = num.clss
num.clss <- 6
ikmeans <- f.Kmeans(x = stkfile, ncl = num.clss,
                           niter.max = 100, nstarts = 200)
plot(ikmeans)
writeRaster(ikmeans, file.path(dir.work, dir.landsat, dir.tif,
                                      paste0('ikmeans', num.clss,'kumbira','.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -9999)

# Flexcluster based on neural gas algorithm ----------------------------------------
f.NgasKmeans <- function(x = x, ncl = num.clss){
  xdf <- as.data.frame(x)
  #xdf <- scale(xdf)
  ikm <- cclust(xdf, ncl, )
  il8m <- matrix(ikm$cluster, nrow = nrow(x), ncol = ncol(x),
                 byrow = TRUE)
  i.kraster <- raster(il8m, xmn=x@extent@xmin, ymn=x@extent@ymin,
                      xmx=x@extent@xmax, ymx=x@extent@ymax,
                      crs = CRS(proj4string(mask.ae)))
  i.kraster
}
num.clss <- 10
ngkmeanspca20140214 <- f.Kmeans(x = stkpca, ncl = num.clss)
plot(ngkmeanspca20140214)
writeRaster(ngkmeanspca20140214, file.path(dir.work, dir.landsat, dir.rst,
                                           paste0('ngkmeans20140214ae_', num.clss,'.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -9999)
            
