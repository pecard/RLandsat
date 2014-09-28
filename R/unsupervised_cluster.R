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
f.VegIndex <- function(stk = stk, index = 'ndvi'){
  #istk <- stk[[1:7]]
  # Vegetation Index (Lansdat 8 OLI)
  ## NDVI = (5-4)/(5+4)
  ## LSWI = (5-6)/(5+6)
  ## NBR - (5-7)/(5+7)
  i.index <- c('ndvi', 'lswi', 'nbr')
  index <- pmatch(index, i.index)
  if (is.na(index)) stop("invalid vegetation index")
  if (index == -1) stop("ambiguous index")
  if(index == 1) {
    out.ind <- (stk$b5_ae - stk$b4_ae)/(stk$b5_ae + stk$b4_ae)
  } else if(index == 2){
    out.ind <- (stk$b5_ae-stk$b6_ae)/(stk$b5_ae+stk$b6_ae)
  } else if(index == 3){
    out.ind <- (stk$b5_ae-stk$b7_ae)/(stk$b5_ae+stk$b7_ae)      
  }
  out.ind
}

#' Specify sensible and meaningful object names
vegindmar2014 <- f.VegIndex(stk = stktoar_mskmar2014, index = 'ndvi') # stack Bands + VegIndex
vegindnov2013 <- f.VegIndex(stk = stktoar_msk, index = 'ndvi') # stack Bands + VegIndex
vegind_dif <- vegindmar2014 - vegindnov2013

plot(vegind_dif)
plot(vegind1)

## Export Vegetation Index ---------------------------------------------------------
## 30-01-2014
writeRaster(vegind1, file.path(dir.work, dir.landsat, dir.tif,
                               'ndvi.rst'),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)
writeRaster(vegind_dif,
            file.path(dir.work, dir.landsat, dir.tif,
                      paste0('ndvi2014_2013_2')),
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
stk_kmeans <- stktoar_mskmar2014 # Sem Veg Index
stk_kmeans <- addLayer(stk_kmeans, vegind_dif)
#stk20140214s <- raster::scale(stk20140214)
stk_kmeans <-  reclassify(stk_kmeans, matrix(c(NA, -0.01), nrow = 1))

#' base::kmeans ---------------------------------------------------------------------
f.Kmeans <- function(x = x, ncl = num.clss, niter.max = 5, nstarts = 5){
  xdf <- as.data.frame(x)
  #xdf <- scale(xdf)
  ikm <- kmeans(xdf, ncl, iter.max = niter.max, nstart = nstarts)
  il8m <- matrix(ikm$cluster, nrow = nrow(x), ncol = ncol(x),
                 byrow = TRUE)
  i.kraster <- raster(il8m, xmn=x@extent@xmin, ymn=x@extent@ymin,
                      xmx=x@extent@xmax, ymx=x@extent@ymax,
                      crs = CRS(proj4string(mask_ae)))
  i.kraster
}

#' Run kmeans function of selected stack object: create a ikmeans raster file with
#'# classes = num.clss
<<<<<<< HEAD
num.clss <- 17
ikmeans <- f.Kmeans(x = stkpca, ncl = num.clss, niter.max = 25, nstarts = 25)
=======
num.clss <- 6
ikmeans <- f.Kmeans(x = stkfile, ncl = num.clss,
                           niter.max = 100, nstarts = 500)
>>>>>>> 62725d2043b482c4d0c87a2d510173e656e22c70
plot(ikmeans)
writeRaster(ikmeans, file.path(dir.work, dir.landsat, dir.tif,
                               paste0('ikmeans', num.clss,'_pnjvp_pca_vegdig','.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -9999)

write.table(freq(ikmeans), file = 'clipboard', sep = '\t')

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


# Paralell
beginCluster()
endCluster()
