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

library(gdalUtils)
library(gdata)
library(ggpubr)
library(ggthemes)
library(gridExtra)
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


keep(proc_dir, raw_dir, scratch_dir, sure = T)
   
#------------------------------

###################################################################
## Calculate carbon stock losses and gains at the national scale ##
###################################################################

# Load in data

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)

mg2000 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2000")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

mg2014 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

mg2014_ls <- st_read(paste0(proc_dir, "shapefiles/dstrct_losses_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  mutate(mines = NA)

mg2014_gn <- st_read(paste0(proc_dir, "shapefiles/dstrct_gains_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))


# Build a helper function to apply varying rates of stock change

calcCarbon <- function(sf_obj, agb_rate, soc_rate) {
  
  df <- sf_obj %>%
    mutate(aqua_c = (aqucltr * AGB_AVG * agb_rate) + (aqucltr * SOC_AVG * soc_rate),
           agri_c = (agrcltr * AGB_AVG * agb_rate) + (agrcltr * SOC_AVG * soc_rate),
           mine_c = (mines * AGB_AVG * agb_rate) + (mines * SOC_AVG * soc_rate),
           abnd_c = (abandnd * AGB_AVG * agb_rate) + (abandnd * SOC_AVG * soc_rate),
           salt_c = (slt_frm * AGB_AVG * agb_rate) + (slt_frm * SOC_AVG * soc_rate),
           urbn_c = (urban * AGB_AVG * agb_rate) + (urban * SOC_AVG * soc_rate)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(aqua_c, agri_c, mine_c, abnd_c, salt_c, urbn_c)

  ttls <- colSums(df, na.rm = T)
  ttl <- sum(ttls)

  return(ttls)
  
}

# Calculate high, medium and low loss rates for pre 1960 - 2000

mg2000_hls <- calcCarbon(mg2000, agb_rate = 1, soc_rate = 0.67)
mg2000_mls <- calcCarbon(mg2000, agb_rate = 0.82, soc_rate = 0.54)
mg2000_lls <- calcCarbon(mg2000, agb_rate = 0.47, soc_rate = 0.41)

# High loss, varying gains for 2000 - 2014

mg2014_hls_hgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.98, soc_rate = 0.19)
mg2014_hls_mgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.75, soc_rate = 0.1)
mg2014_hls_lgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.41, soc_rate = 0.05)

# Medium loss, varying gains for 2000 - 2014

mg2014_mls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.98, soc_rate = 0.19)
mg2014_mls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.75, soc_rate = 0.1)
mg2014_mls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.41, soc_rate = 0.05)

# Low loss, varying gains for 2000 - 2014

mg2014_lls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.98, soc_rate = 0.19)
mg2014_lls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.75, soc_rate = 0.1)
mg2014_lls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.41, soc_rate = 0.05)

carbonTable <- rbind(mg2000_hls, mg2000_mls, mg2000_lls,
                     mg2014_hls_hgn, mg2014_hls_mgn, mg2014_hls_lgn,
                     mg2014_mls_hgn, mg2014_mls_mgn, mg2014_mls_lgn,
                     mg2014_lls_hgn, mg2014_lls_mgn, mg2014_lls_lgn) %>%
  as.data.frame() %>%
  mutate(year = c(rep("2000", 3), rep("2014", 9)),
         loss = c("hgh", "med", "low", rep("hgh", 3), rep("med", 3), rep("low", 3)),
         gain = c(rep(NA, 3), rep(c("hgh", "med", "low"), 3)))


# Calculate net change for comparison

mg2000_df <- mg2000 %>%
  st_set_geometry(NULL) %>%
  select(ADM2_EN, mangrov)

mg2014_df <- mg2014 %>%
  st_set_geometry(NULL) %>%
  select(ADM2_EN, mangrov)

net <- mg2014_df %>%
  left_join(mg2000_df, by = "ADM2_EN", suffix = c("_14", "_00")) %>%
  replace(is.na(.), 0) %>%
  mutate(mangrov_net = mangrov_14 - mangrov_00) %>%
  select(ADM2_EN, mangrov_net) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN")) %>%
  mutate(net_mg_c_ls = (mangrov_net * AGB_AVG * 0.82) + (mangrov_net * SOC_AVG * 0.54)) %>%
  select(ADM2_EN, net_mg_c_ls)

sum(net$net_mg_c_ls)


mg2014_ls_df <- mg2014_ls %>%
  st_set_geometry(NULL) %>%
  select(aqucltr, agrcltr, mangrov, abandnd, slt_frm, urban) %>%
  summarize_all(~sum(., na.rm = T))


mg2014_gn_df <- mg2014_gn %>%
  st_set_geometry(NULL) %>%
  select(aqucltr, agrcltr, mangrov, abandnd, slt_frm, urban) %>%
  summarize_all(~sum(., na.rm = T))


#---------------------------------------------
# Potential carbon gains from restoration

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)

mg2014_rstr <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aqucltr, agrcltr, abandnd, 
         AGB_AVG, AGB_SD, SOC_AVG, SOC_SD)

calcRestorationCarbon <- function(df, rstr_rate, agb_rate, soc_rate) {
  
  df_rstr <- df %>%
    mutate(aqucltr_rstr = aqucltr * rstr_rate,
           agrcltr_rstr = agrcltr * rstr_rate,
           abandnd_rstr = abandnd) %>%
    rowwise() %>%
    mutate(total = aqucltr_rstr + agrcltr_rstr + abandnd_rstr) %>%
    ungroup() %>%
    mutate(agb_rstr = total * agb_rate * AGB_AVG,
           soc_rstr = total * soc_rate * SOC_AVG) %>%
    dplyr::select(total, agb_rstr, soc_rstr)
    
  ttls <- colSums(df_rstr, na.rm = T)
  ttl <- sum(ttls)
  
  return(ttl)
  
}

agb <- 0.41
soc <- 0.05

calcRestorationCarbon(mg2014_rstr, 0.001, agb, soc)
calcRestorationCarbon(mg2014_rstr, 0.01, agb, soc)
calcRestorationCarbon(mg2014_rstr, 0.02, agb, soc)
calcRestorationCarbon(mg2014_rstr, 0.05, agb, soc)
calcRestorationCarbon(mg2014_rstr, 0.1, agb, soc)

#------------------
# Scrap code?
# Recreate Sasmito et al., 2020, Figure 5a

yr <- c(0, 10, 20, 30, 40, 50, 60)
agb <- c(0, 45, 90, 125, 140, 140, 140)
  
dat <- data.frame(yr = yr, agb = agb)

# y = a / (1 + b e-kx )

model <- nls(dat$agb ~ a / (1 + b * exp(1) ^ (-k * dat$yr)), start=list(a = 140, b = 13, k = 0.1) )

# a = 142.5455, b = 13.2424, k = 0.1447

# For 15 years, in percent

(142.5455 / (1 + 13.2424 * exp(1) ^ (-0.1447 * 15) )) / 140  * 100

# For 15 years, in percent

(142.5455 / (1 + 13.2424 * exp(1) ^ (-0.1447 * 25) )) / 140 * 100

# For 40 years, in percent 

(142.5455 / (1 + 13.2424 * exp(1) ^ (-0.1447 * 40) )) / 140 * 100


#---------------------------------
# Uncertainty - how will I calculate uncertainty for modeled data.
# Bootstrapping?



