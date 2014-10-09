#' WEKA Data
#' Build WEKA files for classification
library(raster)
library(foreign)
library(RWeka)

dir.landsat <- 'LC82040522014078LGN00/qgis/dos1' # Folder with Landast 8 TIF files
dir.work <- 'S:/Raster/Landsat' # Alterar para Disco Local

bands <- list.files(file.path(dir.work, dir.fun), full.names = T,
                    pattern = '.TIF$')  
stkDOS1 <- stack(bands)
roi <- extent(c(358977, 368475, 1360958, 1370051))
stkroi <- crop(stkDOS1, roi)
plot(stkroi)

foreign::write.arff(as.matrix(stk_mask),
           file = file.path(dir.work, dir.landsat, dir.tif,
                            'weka_stk_mask.arff'))

datafrom_weka <- foreign::read.arff(file.path(dir.work, dir.landsat, dir.tif,
                                    'xmeans_1-7b.arff'))

mclust <- matrix(as.numeric(datafrom_weka$Cluster),
                 nrow = nrow(stk_pca),
                 ncol = ncol(stk_pca),
                 byrow = TRUE)
rclust <- raster(mclust, xmn = stk_pca@extent@xmin, ymn = stk_pca@extent@ymin,
                 xmx = stk_pca@extent@xmax, ymx = stk_pca@extent@ymax,
                 crs = CRS(proj4string(stk_pca)))
plot(rclust)

writeRaster(rclust,
            filename = file.path(dir.work, dir.landsat, dir.tif,
                                 'weka_6xmeans_1-7b.rst'),
            format = 'RST',
            overwrite = TRUE)