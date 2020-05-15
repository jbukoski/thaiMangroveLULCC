# 03_c_quantification.R
# This file back-models carbon to historical areas of mangrove extent in Thailand.

#-----------------------------------
# Conceptual steps

# Back-modeling of carbon to historical mangroves areas in Thailand
# Steps:
#  1. Load in data
#     a. SOC data - Sanderman et al.
#     b. Biomass data - Simard et al.
#     c. Chongwat data - ?
#  2. Identify ecologically meaningful regions for deriving averages
#     a. SOC k-means clustering to identify zones
#     b. Examine height data - how best to do this?
#  3. Summarize data by chongwat
#  4. Map average values onto map
#     a. By chongwat

#-----------------------------------

print("Begin Step 3. backmodeling of carbon stocks to historical mangrove extent...")

#-----------------------------------
# Load in libaries

library(cluster)
library(factoextra)
library(gdalUtils)
library(NbClust)
library(raster)
library(rgdal)
library(sf)
library(sp)
library(spsurvey)
library(tidyverse)

#---------------------------------
# define directories

raw_dir <- "./data/raw/"
proc_dir <- "./data/processed/"
scratch_dir <- "./data/scratch/"

#--------------------------------

#######################
## SOC Back-modeling ##
#######################

#---------------------------------
# SOC data - k-means clustering analysis
# Load in necessary data

dstrcts <- read_sf(paste0(proc_dir, "shapefiles/districts_mg/districts_mg.shp"))
soc <- raster(paste0(raw_dir, "rasters/Mangrove_soc_Thailand.tif"))
agb <- raster(paste0(raw_dir, "rasters/Mangrove_agb_Thailand.tif"))

#--------------------------------
# Derive average soc and biomass values for each district

dstrcts$ADM2_ID <- 1:nrow(dstrcts)
dstrcts <- dplyr::select(dstrcts, ADM1_EN, ADM2_EN, ADM2_ID, geometry)
dstrcts_sp <- as(dstrcts, "Spatial")

dstrct_avgs <- data.frame("ADM2_ID" = 1:nrow(dstrcts_sp), 
                          "SOC_AVG" = NA, "SOC_SD" = NA,
                          "AGB_AVG" = NA, "AGB_SD" = NA)

for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  
  soc_crop <- crop(soc, shp)
  soc_dat <- raster::extract(soc_crop, shp, df = T)
  dstrct_avgs$SOC_AVG[i] <- mean(soc_dat$Mangrove_soc_Thailand, na.rm = T)
  dstrct_avgs$SOC_SD[i] <- sd(soc_dat$Mangrove_soc_Thailand, na.rm = T)
  
  rm(soc_dat, soc_crop)
  gc()
  
}

for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  
  agb_crop <- crop(agb, shp)
  agb_dat <- raster::extract(agb_crop, shp, df = T)
  dstrct_avgs$AGB_AVG[i] <- mean(agb_dat$Mangrove_agb_Thailand, na.rm = T)
  dstrct_avgs$AGB_SD[i] <- sd(agb_dat$Mangrove_agb_Thailand, na.rm = T)
  
  rm(agb_crop, agb_dat)
  gc()
  
}

dstrcts_c <- dstrcts_sp %>% 
  st_as_sf() %>%
  left_join(dstrct_avgs, by = "ADM2_ID") %>%
  arrange(ADM1_EN, ADM2_EN) %>%
  group_by(ADM1_EN) %>%
  mutate(SOC_AVG = ifelse(is.nan(SOC_AVG), mean(SOC_AVG, na.rm = T), SOC_AVG),
         SOC_SD = ifelse(is.na(SOC_SD), sqrt(sum(SOC_SD^2, na.rm = T)), SOC_SD),
         AGB_AVG = ifelse(is.nan(AGB_AVG), mean(AGB_AVG, na.rm = T), AGB_AVG),
         AGB_SD = ifelse(is.na(AGB_SD), sqrt(sum(AGB_SD^2, na.rm = T)), AGB_SD)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(ECO_AVG = sum(AGB_AVG, SOC_AVG),
         ECO_SD = sqrt(sum(AGB_SD^2, SOC_SD^2))) %>%
  ungroup() %>%
  st_as_sf() %>%
  dplyr::select(ADM1_EN, ADM2_EN, ADM2_ID, AGB_AVG, AGB_SD, SOC_AVG, SOC_SD,
                ECO_AVG, ECO_SD, geometry)

st_write(dstrcts_c, dsn = paste0(proc_dir, "shapefiles/dstrcts_c"), layer = "dstrcts_c", driver = "ESRI Shapefile")

rm(list = ls())

#--------------
# Intersect the districts C data with historic mangrove extent

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)

mg2000 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2000"))
mg2014_ls <- st_read(paste0(proc_dir, "shapefiles/dstrct_losses_2014"))
mg2014_gn <- st_read(paste0(proc_dir, "shapefiles/dstrct_gains_2014"))

mg2000_c <- mg2000 %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  mutate(aqua_c_ls = aqucltr * ECO_AVG,
         agri_c_ls = agrcltr * ECO_AVG,
         mine_c_ls = mines * ECO_AVG,
         abnd_c_ls = abandnd * ECO_AVG,
         salt_c_ls = slt_frm * ECO_AVG,
         urbn_c_ls = urban * ECO_AVG,
         mdflt_c_ls = mudflts * AGB_AVG)

mg2000_c_df <- mg2000_c %>%
  st_set_geometry(NULL)

mg2014_ls_c <- mg2014_ls %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  mutate(aqua_c_ls = aqucltr * ECO_AVG,
         agri_c_ls = agrcltr * ECO_AVG,
         abnd_c_ls = abandnd * ECO_AVG,
         salt_c_ls = slt_frm * ECO_AVG,
         urbn_c_ls = urban * ECO_AVG,
         mdflt_c_ls = mudflts * AGB_AVG)

mg2014_ls_c_df <- mg2014_ls_c %>%
  st_set_geometry(NULL)

mg2014_gn_c <- mg2014_gn %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  mutate(aqua_c_ls = aqucltr * AGB_AVG,
         agri_c_ls = agrcltr * AGB_AVG,
         abnd_c_ls = abandnd * AGB_AVG,
         salt_c_ls = slt_frm * AGB_AVG,
         urbn_c_ls = urban * AGB_AVG,
         mdflt_c_ls = mudflts * AGB_AVG)

mg2014_gn_c_df <- mg2014_gn_c %>%
  st_set_geometry(NULL)

ttl_ls_2000 <- colSums(mg2000_c_df[, 20:26], na.rm = T)
ttl_ls_2014 <- colSums(mg2014_ls_c_df[, 21:26], na.rm = T)
ttl_gn_2014 <- colSums(mg2014_gn_c_df[, 20:25], na.rm = T)

loss2000 <- sum(ttl_ls_2000[1:6])
loss2014 <- sum(ttl_ls_2014[1:5])
gain2014 <- sum(ttl_gn_2014[1:5])





#-------------------------------------------
# Intersect the provinces C data with historic mangrove extent

crs102028 <- "+proj=aea +lat_1=7 +lat_2=-32 +lat_0=-15 +lon_0=125 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

mg_historic <- st_read(paste0(in_dir, "shapefiles/dissolved_102028.shp"))
mg_2000 <- st_read(paste0(in_dir, "shapefiles/mg2000_102028.shp"))
mg_2014 <- st_read(paste0(in_dir, "shapefiles/mg2014_102028.shp"))

chngwts_c_102028 <- chngwts_c %>%
  st_transform(crs102028)

mg_hstrc_chngwts <- st_intersection(chngwts_c_102028, mg_historic)










# Union and buffer the chongwats by 10 km 

albersSEAsia <- CRS(" +proj=aea +lat_1=7 +lat_2=-32 +lat_0=-15 +lon_0=125 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
epsg4326 <- CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

cngwt <- st_read("./data/processed/cstl_prvncs")

cngwt_union <- st_union(cngwt) %>%
  st_transform(crs = albersSEAsia)

cngwt_bffr <- st_buffer(cngwt_union, 5000) %>%
  st_transform(crs = epsg4326)

adm0 <- read_sf("./data/raw/tha_admbnda_adm0_rtsd_20190221.shp")
sea_adm0 <- read_sf("./data/raw/se_asia.shp")

# Geomorphology data

tsm <- raster("../../ch1_c_estimation/analysis/data/raw/site_map/tha_mean_tsm.tif")
tdl <- raster("../../ch1_c_estimation/analysis/data/raw/site_map/m2_4326_a.tif")

tdl_new <- resample(tdl, tsm, method = "bilinear")

gmrphStack <- stack(tsm, tdl_new)

gmrphStackMskd <- mask(gmrphStack, as(cngwt_bffr, "Spatial"))

gmrph_dat <- raster::extract(gmrphStack, as(cngwt_bffr, "Spatial"), cellnumbers = T) %>%
  as.data.frame()  %>%
  rename(tsm = "tha_mean_tsm",
         tdl = "m2_4326_a",
         id = "cell")

# Identify optimal number of clusters using "elbow" method

maxClstrs <- 20

optClstrs <- data.frame(clstrs = seq(1, maxClstrs, 1),
                        tot_wss = NA)

for(i in 1:maxClstrs) {
  
  kmeansClass <- kmeans(na.omit(gmrph_dat[ , c(2, 3)]), i, nstart = 30)
  optClstrs$tot_wss[i] <- kmeansClass$tot.withinss
  
}

fviz_nbclust(na.omit(gmrph_dat[, c(2,3)]), kmeans, method = 'wss', k.max = 25, nstart = 30)
fviz_nbclust(na.omit(gmrph_dat[, c(2,3)]), kmeans, method = 'silhouette', k.max = 25, nstart = 30)

plot(optClstrs)

# Classify coastline based on optimal number of clusters

idx <- na.omit(gmrph_dat[ , c(1, 2, 3)])

kmeansClass <- kmeans(na.omit(gmrph_dat[ , c(2, 3)]), 4, nstart = 30)

idx <- cbind(idx, kmeansClass$cluster)

classed_dat <- gmrph_dat %>%
  left_join(idx, by = c("id")) %>%
  rename(class = "kmeansClass$cluster") %>%
  rename(tsm = "tsm.x",
         tdl = "tdl.x") %>%
  dplyr::select(id, tsm, tdl, class)

classes <- gmrphStack[[1]]

values(classes) <- 0

dat <- values(classes)

vals <- dat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  left_join(classed_dat, by = "id") %>%
  pull(class)


values(classes) <- vals
classesCrpd <- crop(classes, as(cngwt_bffr, "Spatial"))
classesMskd <- mask(classesCrpd, as(cngwt_bffr, "Spatial"))

classesMskd_df <- as.data.frame(classesMskd, xy = T, na.rm = T)

ggplot(sea_adm0) +
  geom_sf(fill = "#F2F2F2") +
  geom_sf(data = adm0, aes(geometry = geometry), fill = "#E5E5E5") +
  geom_raster(data = classesMskd_df, aes(x = x, y = y, fill = factor(tha_mean_tsm))) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(fill = "Cluster") +
  xlim(c(98, 105)) +
  ylim(c(5, 15)) +
  ggtitle("no. clusters = 4") +
  theme(legend.position = "bottom")


# Stratify height data

library(BAMMtools)   # For Jenks Breaks

simard_agb <- raster("../../ch1_c_estimation/analysis/data/raw/modeled_datasets/Mangrove_agb_Thailand.tif")
simard_height <- raster("~/Desktop/mangrove_c_model_data/CMS_Global_Map_Mangrove_Canopy_1665/data/Mangrove_hmax95_Thailand.tif")

hgt_resample <- raster(nrow = 28062/3, ncol = 20575/3, crs = epsg4326, ext = extent(simard_height))

simard_height_rsmpl <- resample(simard_height, hgt_resample, method = "bilinear")


# For height
m_hgt <- c(0.5, 8.5, 1, 8.5, 15.3, 2, 15.3, 22.1, 3)
rclmat_hgt <- matrix(m_hgt, ncol = 3, byrow = T)
rc <- reclassify(simard_height, rclmat_hgt)

# For biomass
m <- c(57, 99, 1, 99, 139, 2, 139, 185, 3)
rclmat <- matrix(m, ncol = 3, byrow = T)
rc <- reclassify(simard_agb, rclmat)

simard_height_df_rs <- as.data.frame(simard_height_rsmpl, xy = T, na.rm = T)
colnames(simard_height_df_rs) <- c("hgt", "x", "y")

simard_height_df <- as.data.frame(simard_height, xy = T, na.rm = T)
colnames(simard_height_df) <- c("hgt", "x", "y")

getJenksBreaks(simard_height_df$hgt, 5)

#0.8485  6.7880 11.8790 16.9700 22.0610

ggplot(simard_height_df, aes(hgt)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  xlab("Mean mangrove canopy height, 30 x 30 m (m)") +
  ylab("Count")

ggplot(sea_adm0) +
  geom_sf(fill = "#F2F2F2") +
  geom_sf(data = adm0, aes(geometry = geometry), fill = "#E5E5E5") +
  geom_raster(data = simard_height_df, aes(x = x, y = y, fill = hgt)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(fill = "Cluster") +
  #coord_sf(xlim = c(99.85, 100.05), ylim = c(13.2, 13.4)) +
  xlim(c(99.85, 100.05)) +
  ylim(c(13.2, 13.4)) +
  ggtitle("no. clusters = 4") +
  theme(legend.position = "bottom")




