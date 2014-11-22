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

# Export the PCA
foreign::write.arff(as.matrix(stk_pca),
                    file = file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                     'pca1_3_stk_pca.arff'))
# Export to WEKA the mask stack with NAs
foreign::write.arff(as.matrix(stk_mask),
                    file = file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                     'stk_mask.arff'))

raster::writeRaster(stk_mask, file = file.path(dir.work, dir.landsat, 'geotif',
                                               'b1_7_stk_mask.tif'),
                    Format = 'GTiff')
raster::writeRaster(stk_dos1, file = file.path(dir.work, dir.landsat, 'geotif',
                                               'b1_7_stk_dos1.tif'),
                    Format = 'GTiff')

datafrom_weka <- foreign::read.arff(file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                              'jvieira_xmeans14_2.arff'))

mclust <- matrix(as.numeric(datafrom_weka$Cluster),
                 nrow = nrow(stk_pca),
                 ncol = ncol(stk_pca),
                 byrow = TRUE)
rclust <- raster(mclust, xmn = stk_pca@extent@xmin, ymn = stk_pca@extent@ymin,
                 xmx = stk_pca@extent@xmax, ymx = stk_pca@extent@ymax,
                 crs = CRS(proj4string(stk_pca)))
plot(rclust)

writeRaster(rclust,
            filename = file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                 'weka_14xmeans2_bands.rst'),
            format = 'RST',
            overwrite = TRUE)