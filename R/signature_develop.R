#'##################################################################################
#'## Create Signature files based on trainning sites ###
#' R version 3.0.2 rgdal_0.8-16  raster_2.2-16 sp_1.0-14

#'## Used for Kumbira Forest as reference site
#'## 
#'##################################################################################

#'# Load/Install Packages ##########################################################
kpacks <- c("raster", "sp", "rgdal", 'rgeos', 'ggplot2', 'dplyr', 'reshape',
            'plyr')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)
#'##################################################################################

#'save and load session ############################################################
save.image('sign_develop.RData')
load('sign_develop.RData')
#'# Projections ####################################################################
#'# Kumbira lies into GRID UTM 33S but original Landsat will be registere to
#'# UTM 33N
p.utm33n <- CRS("+init=epsg:32633") # UTM 33N Landsat Images
p.wgs84 <- CRS("+init=epsg:4326") # WGS84 Long Lat
#'##################################################################################

#'# Import your ROI for image cropping/extract #####################################
training <- readOGR(dsn = file.path('.../SIG'),
                    layer = 'Trainning_Areas')
proj4string(training) <- p.wgs84 # Asign projection WGS84
training_utm <- spTransform(training, p.utm33n) # project to 33N
training_utm@data$class <- as.character(training_utm@data$class) # remove factors
#'# Reclass original classes to ncl 
training_utm$ncl <- training_utm$class
training_utm$ncl[grep('Bare', training_utm$ncl)] <- 'Bare soil'
training_utm$ncl[grep('2 Forest', training_utm$ncl)] <- 'Secondary forest'
training_utm$ncl[grep('Good Forest', training_utm$ncl)] <- 'Forest'
#training_utm <- training_utm[, 5] # remove variables
unique(training_utm$ncl)
#'##################################################################################

#'# Read TIF files into a rasterStack ##############################################
# For WRS2 Row 181 Path 68: UTM 33N
filewd <- '../Landsat/LC81810682014045LGN00'
allfiles <- list.files(file.path(filewd), all.files = F)
# List of TIF files at dir.fun folder
tifs <- grep(".envi$", allfiles, ignore.case = TRUE, value = TRUE) 
etm_stk <- stack(file.path(filewd, tifs))
#'##################################################################################

#' Create signature for each band ##################################################
l8wavelength <- read.table(file.path(getwd(), 'Data', 'Landsat8wavelengths.txt'),
                           sep = '\t', header = T, dec = '.', stringsAsFactors = F)
sig_file <- extract(etm_stk, training_utm, df = T)
sigs <- melt(sig_file, id.var = 'ID')
sigs <- merge(sigs,
              training_utm@data[, c(1, 5)],
              by.x = 'ID', by.y = 'id')
sigs <- tbl_df(sigs)
sigs_iqr <- ddply(sigs, .(ncl, variable),
                  function(x) quantile(x$value, probs = c(0.20, 0.80)))
sigs <- left_join(sigs, sigs_iqr, by = c('ncl', 'variable'))
names(sigs)[5:6] <- c('q1', 'q2')
#'##################################################################################

#' Purify original signature based on quantiles 25% and 75% ########################
qsigs <- sigs %.%
  filter(value > q1 & value < q2) %.%
  mutate(class = as.character(ncl), band = as.character(variable)) %.%
  select(class, band, value) %.%
  arrange(class, band) 
qsigs <- ddply(qsigs, .(class, band), transform, newcl = paste(class, seq_along(band)))

group_by(class, band) %.%
  mutate(newcl = paste(class, seq_along(band)))


#' Plots and exploratory analysis ##################################################
qsigs %.%
  dplyr:::group_by(ncl) %.%
  dplyr:::summarise(count = n())
ggplot(qsigs,aes(x=band,y=value, colour = class)) +
  stat_summary(aes(group=class), fun.y=mean, geom="line", size = 0.8, linetype = 1) +
  labs(list(x=NULL, y = 'TOA Reflectance')) +
  theme_bw() +
  theme(legend.key=element_rect(fill='transparent', colour='transparent'))
  
