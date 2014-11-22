#'##################################################################################
#'## Create Signature files based on trainning sites ###
#' R version 3.0.2 rgdal_0.8-16  raster_2.2-16 sp_1.0-14

#'## Used for Kumbira Forest as reference site
#'## 
#'##################################################################################

#'# Load/Install Packages ##########################################################
kpacks <- c("raster", "sp", "rgdal", 'rgeos', 'ggplot2',
            'dplyr', 'reshape2')
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
training <- readOGR(dsn = file.path('S:/Bissau/bijagos_landcover/vetor'),
                    layer = 'areas_treino_jv')
proj4string(training) <- p.wgs84 # Asign projection WGS84
training_utm <- spTransform(training, p.utm28n) # project to 28N
training_utm@data$nivel2 <- as.character(training_utm@data$nivel2) # remove factors

#'# Reclass original classes to ncl 
training_utm$ncl <- training_utm$class
training_utm$ncl[grep('Bare', training_utm$ncl)] <- 'Bare soil'
training_utm$ncl[grep('2 Forest', training_utm$ncl)] <- 'Secondary forest'
training_utm$ncl[grep('Good Forest', training_utm$ncl)] <- 'Forest'
#training_utm <- training_utm[, 5] # remove variables
unique(training_utm$nivel2)
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
l8wavelength <- read.table('S:/Raster/Landsat/Landsat8wavelengths.txt',
                           sep = '\t', header = T, dec = '.',
                           stringsAsFactors = F)
sig_file <- extract(stk_dos1, training_utm, df = T)
names(sig_file) <- training_utm@data$nivel2
#sig_filedf <- sig_file %>% lapply(as.data.frame) %>% rbind_all()
sigs <- select(tbl_df(melt(sig_file, level = 1)), -1)
names(sigs) <- c('banda', 'valor', 'classe')
#sigs <- merge(sigs,
#              training_utm@data[, c(1, 5)],
#              by.x = 'ID', by.y = 'id')
sigs_iqr <- sigs %>%
  group_by(classe, banda) %>%
  summarise(quantile(valor, c(0.10, 0.90)))
  ddply(sigs, .(classe, banda),
                  function(x) quantile(x$value, probs = c(0.10, 0.90)))
sigs <- left_join(sigs, sigs_iqr, by = c('L1', 'X2'))
names(sigs)[5:6] <- c('q1', 'q2')
#'##################################################################################

#' Purify original signature based on quantiles 25% and 75% ########################
qsigs <- sigs %>%
  filter(value > q1 & value < q2) %>%
  mutate(class = as.character(L1), band = as.character(X2)) %>%
  select(class, band, value) %>%
  arrange(class, band) 
qsigs <- ddply(qsigs, .(class, band), transform, newcl = paste(class, seq_along(band)))

group_by(class, band) %>%
  mutate(newcl = paste(class, seq_along(band)))


#' Plots and exploratory analysis ##################################################
qsigs %>%
  filter(band %in% c('b5_ae','b6_ae', 'b7_ae'))%>%
  dplyr:::group_by(class, band) %>%
  dplyr:::summarise(count = n(),
                    mean_toa = mean(value))

ggplot(qsigs, aes(x = band, y=value, colour = class)) +
  geom_boxplot(aes(group=class))
  stat_summary(aes(group=class), fun.y=mean, geom="line", size = 1, linetype = 1) +
  labs(list(x=NULL, y = 'TOA Reflectance')) +
  theme_bw() +
  theme(legend.key=element_rect(fill='transparent', colour='transparent'))
  
