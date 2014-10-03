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

write.arff(as.matrix(stkroi),
           file = paste0("S:/Raster/Landsat/LC82040522014078LGN00/qgis/dos1/rst/",
                         'b1-7.arff'))

k12 <- read.arff(paste0("S:/Raster/Landsat/LC82040522014078LGN00/qgis/dos1/rst/",
                        'kmeans12.arff'))

mclust <- matrix(as.numeric(k12$Cluster), nrow = nrow(stkroi), ncol = ncol(stkroi),
                 byrow = TRUE)
rclust <- raster(mclust, xmn = stkroi@extent@xmin, ymn = stkroi@extent@ymin,
                 xmx = stkroi@extent@xmax, ymx = stkroi@extent@ymax,
                 crs = CRS(proj4string(stkroi)))
plot(rclust)

writeRaster(rclust,
            filename = file.path('S:/Raster/Landsat/LC82040522014078LGN00/qgis/dos1/rst',
                                 'k12.rst'),
            format = 'RST',
            overwrite = TRUE)