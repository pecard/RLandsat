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

#' Read Data From Weka Output and build classified raster
weka_file <- 'jvieira_canopy_auto4_pca.arff'
datafrom_weka <- foreign::read.arff(file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                              weka_file))

mclust <- matrix(as.numeric(datafrom_weka$Cluster),
                 nrow = nrow(stk_pca),
                 ncol = ncol(stk_pca),
                 byrow = TRUE)
rclust <- raster::raster(mclust, xmn = stk_pca@extent@xmin, ymn = stk_pca@extent@ymin,
                 xmx = stk_pca@extent@xmax, ymx = stk_pca@extent@ymax,
                 crs = CRS(proj4string(stk_pca)))
plot(rclust)

raster::writeRaster(rclust,
            filename = file.path('S:/Raster/Landsat/LC82040522014078LGN00/weka/jvieira',
                                 weka_file),
            format = 'RST',
            overwrite = TRUE)

#' RWEKA
WPM("install-package", "XMeans") 
WPM("load-package", "XMeans") 

wow_f <- WOW(L=10,H=15)
mclust <- matrix(as.numeric(XMeans(as.data.frame(stk_pca),
                                         Weka_control(I=100, M=1000, J=1000, L=10, H=15, S=10))$class_ids),
                 nrow = nrow(stk_pca),
                 ncol = ncol(stk_pca),
                 byrow = TRUE)
rclust <- raster::raster(mclust, xmn = stk_pca@extent@xmin, ymn = stk_pca@extent@ymin,
                         xmx = stk_pca@extent@xmax, ymx = stk_pca@extent@ymax,
                         crs = CRS(proj4string(stk_pca)))
plot(rclust)
