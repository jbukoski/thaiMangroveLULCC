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
#     a. Derive mean values
#     b. Derive standard deviation values
#  4. Assign average values onto map
#     a. By chongwat

#-----------------------------------

print("Begin Step 3. backmodeling of carbon stocks to historical mangrove extent...")

#-----------------------------------
# Load in libaries

library(e1071)
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

agb_rmse <- 148.0   # Cross-validation RMSE value reported for SE Asia in Simard et al., 2019 SI File (rather high!)
soc_rmse <- 10.9    # Cross-validation RMSE value reported in Sanderman et al., 2018

for(i in 1:nrow(dstrcts_sp)) {
      
  shp <- dstrcts_sp[i, ]
    
  soc_crop <- crop(soc, shp)
  soc_dat <- raster::extract(soc_crop, shp, df = T)
  
  dstrct_avgs$SOC_AVG[i] <- mean(soc_dat$Mangrove_soc_Thailand, na.rm = T)
  dstrct_soc_sd <- sd(soc_dat$Mangrove_soc_Thailand, na.rm = T)
  
  dstrct_avgs$SOC_SD[i] <- sqrt(dstrct_soc_sd^2 + soc_rmse^2)
  
  rm(soc_dat, soc_crop)
  gc()
  
}

for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  
  agb_crop <- crop(agb, shp)
  agb_dat <- raster::extract(agb_crop, shp, df = T)
  
  dstrct_avgs$AGB_AVG[i] <- mean(agb_dat$Mangrove_agb_Thailand, na.rm = T)
  dstrct_agb_sd <- sd(agb_dat$Mangrove_agb_Thailand, na.rm = T)
  
  dstrct_avgs$AGB_SD[i] <- sqrt(dstrct_agb_sd^2 + agb_rmse^2)
  
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

st_write(dstrcts_c, dsn = paste0(proc_dir, "shapefiles/dstrcts_c"), 
         layer = "dstrcts_c", driver = "ESRI Shapefile", append = FALSE)

gdata::keep(proc_dir, raw_dir, scratch_dir, sure = T)
   
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
           aqua_c_sd = (aqucltr * AGB_SD * agb_rate) + (aqucltr * SOC_SD * soc_rate),
           agri_c = (agrcltr * AGB_AVG * agb_rate) + (agrcltr * SOC_AVG * soc_rate),
           agri_c_sd = (agrcltr * AGB_SD * agb_rate) + (agrcltr * SOC_SD * soc_rate),
           mine_c = (mines * AGB_AVG * agb_rate) + (mines * SOC_AVG * soc_rate),
           mine_c_sd = (mines * AGB_SD * agb_rate) + (mines * SOC_SD * soc_rate),
           abnd_c = (abandnd * AGB_AVG * agb_rate) + (abandnd * SOC_AVG * soc_rate),
           abnd_c_sd = (abandnd * AGB_SD * agb_rate) + (abandnd * SOC_SD * soc_rate),
           salt_c = (slt_frm * AGB_AVG * agb_rate) + (slt_frm * SOC_AVG * soc_rate),
           salt_c_sd = (slt_frm * AGB_SD * agb_rate) + (slt_frm * SOC_SD * soc_rate),
           urbn_c = (urban * AGB_AVG * agb_rate) + (urban * SOC_AVG * soc_rate),
           urbn_c_sd = (urban * AGB_SD * agb_rate) + (urban * SOC_SD * soc_rate)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(aqua_c, aqua_c_sd, agri_c, agri_c_sd,
                  mine_c, mine_c_sd, abnd_c, abnd_c_sd,
                  salt_c, salt_c_sd, urbn_c, urbn_c_sd)

  ttls <- colSums(df, na.rm = T)
  ttl <- sum(ttls)

  return(ttls)
  
}

# Calculate high, medium and low loss rates

mg2000_hls <- calcCarbon(mg2000, agb_rate = 1, soc_rate = 0.67)
mg2000_mls <- calcCarbon(mg2000, agb_rate = 0.82, soc_rate = 0.54)
mg2000_lls <- calcCarbon(mg2000, agb_rate = 0.47, soc_rate = 0.41)

mg2014_hls <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67)
mg2014_mls <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54)
mg2014_lls <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41)

mg2014_hgn <- calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.19)
mg2014_mgn <- calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.1)
mg2014_lgn <- calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.05)

# High loss, varying gains for 2000 - 2014

mg2014_hls_hgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.19)
mg2014_hls_mgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.1)
mg2014_hls_lgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.05)

# Medium loss, varying gains for 2000 - 2014

mg2014_mls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.19)
mg2014_mls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.1)
mg2014_mls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.05)

# Low loss, varying gains for 2000 - 2014

mg2014_lls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.19)
mg2014_lls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.1)
mg2014_lls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.05)

carbonTable <- rbind(mg2000_hls, mg2000_mls, mg2000_lls,
                     mg2014_hls, mg2014_mls, mg2014_lls,
                     mg2014_hgn, mg2014_mgn, mg2014_lgn,
                     mg2014_hls_hgn, mg2014_hls_mgn, mg2014_hls_lgn,
                     mg2014_mls_hgn, mg2014_mls_mgn, mg2014_mls_lgn,
                     mg2014_lls_hgn, mg2014_lls_mgn, mg2014_lls_lgn) %>%
  as.data.frame() %>%
  mutate(year = c(rep("2000", 3), rep("2014", 15)),
         loss = c("hgh", "med", "low", rep(NA, 3), "high", "med", "low", rep("hgh", 3), rep("med", 3), rep("low", 3)),
         gain = c(rep(NA, 3), "high", "med", "low", rep(NA, 3), rep(c("hgh", "med", "low"), 3)))

carbonTable %>%
  mutate(ttl = (aqua_c + agri_c + mine_c + abnd_c + salt_c + urbn_c) / 1000000,
         ttl_sd = (aqua_c_sd + agri_c_sd + mine_c_sd + abnd_c_sd + salt_c_sd + urbn_c_sd) / 1000000)


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
  mutate(net_mg_c_ls = (mangrov_net * AGB_AVG * 0.82) + (mangrov_net * SOC_AVG * 0.54),
         net_mg_c_ls_sd = (mangrov_net * AGB_SD * 0.82) + (mangrov_net * SOC_SD * 0.54)) %>%
  select(ADM2_EN, net_mg_c_ls, net_mg_c_ls_sd)

sum(net$net_mg_c_ls)
sum(net$net_mg_c_ls_sd)


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
# Model biomass recovery using Sasmito 2020 data

library(nls2)
library(propagate)

# Data, provided by Sigit.

dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

# Model form is: y = a / (1 + b * e^-kx )

model <- nls(agb ~ a / (1 + b * exp(1) ^ (-k * yr)), start=list(a = 140, b = 13, k = 0.1), data = dat)

predictDat <- seq(0, max(dat$yr), by = 2)

prop1 <- predictNLS(model, newdata = data.frame(yr = predictDat))

prop_df <- data.frame(yr = predictDat,
                      prdct = prop1$summary$Prop.Mean.1,
                      lwr = prop1$summary$`Prop.2.5%`,
                      upr = prop1$summary$`Prop.97.5%`)

eq1_mdlRuns <- write_csv(prop_df, "./data/processed/eq1_mdlRuns.csv")

# Estimate biomass recovered at 15 years

yr14val <- (138.7840 / (1 + 17.8151 * exp(1) ^ (-0.1765 * 14) ))

# Estimate biomass recovered at 40 years

yr50val <- (138.7840 / (1 + 17.8151 * exp(1) ^ (-0.1765 * 50) ))

mean_rate <- 100 * yr14val / yr50val
low_rate <- 100 * prop_df[prop_df$yr == 14, 2] / yr50val
high_rate <- 100 * prop_df[prop_df$yr == 14, 3] / yr50val



