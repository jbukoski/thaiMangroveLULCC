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
library(EnvStats)
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
                          "SOC_AVG" = NA, "SOC_SE" = NA,
                          "AGB_AVG" = NA, "AGB_SE" = NA)

agb_rmse <- 148.0 * 0.47  # Cross-validation RMSE value reported for SE Asia in Simard et al., 2019 SI File (rather high!)
soc_rmse <- 109.0    # Cross-validation RMSE value reported in Sanderman et al., 2018

for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  soc_crop <- crop(soc, shp)
  soc_dat <- raster::extract(soc_crop, shp, df = T)
  
  means <- c()
  
  for(j in 1:100) {
    
    soc_mean <- mean(soc_dat$Mangrove_soc_Thailand, na.rm = T)
    soc_sd <- sqrt(sd(soc_dat$Mangrove_soc_Thailand, na.rm = T)^2 + soc_rmse^2)
    
    if(!is.na(soc_mean)) {
      
      sim_soc <- rnorm(100, mean = mean(soc_dat$Mangrove_soc_Thailand, na.rm = T), sd = soc_sd)
      sim_soc <- sim_soc[sim_soc > 0]
      gammaParams <- egamma(sim_soc)
      means[j] <- mean(stats::rgamma(100, shape = gammaParams$parameters[1], scale = gammaParams$parameters[2]))  
      
    } else {

      means <- NA
      
    }
    
  }
  
  dstrct_avgs$SOC_AVG[i] <- mean(means)
  dstrct_avgs$SOC_SE[i] <- plotrix::std.error(means)
  
  rm(soc_dat, soc_crop, means)
  gc()
  
}


for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  agb_crop <- crop(agb, shp)
  agb_dat <- raster::extract(agb_crop, shp, df = T)
  
  means <- c()
  
  for(j in 1:100) {
    
    agb_mean <- mean(agb_dat$Mangrove_agb_Thailand, na.rm = T)
    agb_sd <- sqrt(sd(agb_dat$Mangrove_agb_Thailand, na.rm = T)^2 + agb_rmse^2)
    
    if(!is.na(agb_mean)) {
      
      sim_agb <- rnorm(100, mean = mean(agb_dat$Mangrove_agb_Thailand, na.rm = T), sd = agb_sd)
      sim_agb <- sim_agb[sim_agb > 0]
      gammaParams <- egamma(sim_agb)
      means[j] <- mean(stats::rgamma(100, shape = gammaParams$parameters[1], scale = gammaParams$parameters[2]))  
      
    } else {
      
      means <- NA
      
    }
    
  }
  
  dstrct_avgs$AGB_AVG[i] <- mean(means) * 0.47
  dstrct_avgs$AGB_SE[i] <- plotrix::std.error(means)
  
  rm(agb_crop, agb_dat)
  gc()
  
}

dstrcts_c <- dstrcts_sp %>% 
  st_as_sf() %>%
  left_join(dstrct_avgs, by = "ADM2_ID") %>%
  arrange(ADM1_EN, ADM2_EN) %>%
  group_by(ADM1_EN) %>%
  mutate(SOC_AVG = ifelse(is.nan(SOC_AVG), mean(SOC_AVG, na.rm = T), SOC_AVG),
         SOC_SE = ifelse(is.na(SOC_SE), sqrt(sum(SOC_SE^2, na.rm = T)), SOC_SE),
         AGB_AVG = ifelse(is.nan(AGB_AVG), mean(AGB_AVG, na.rm = T), AGB_AVG),
         AGB_SE = ifelse(is.na(AGB_SE), sqrt(sum(AGB_SE^2, na.rm = T)), AGB_SE)) %>%
  ungroup() %>%
  st_as_sf() %>%
  dplyr::select(ADM1_EN, ADM2_EN, ADM2_ID, AGB_AVG, AGB_SE, SOC_AVG, SOC_SE, geometry)

st_write(dstrcts_c, dsn = paste0(proc_dir, "shapefiles/dstrcts_c"), 
         layer = "dstrcts_c", driver = "ESRI Shapefile", append = FALSE)

gdata::keep(proc_dir, raw_dir, scratch_dir, sure = T)

#---------------------------------------------------

####################################################
## Model biomass recovery using Sasmito 2020 data ##
####################################################

library(nls2)
library(propagate)
library(readxl)

# Data, provided by Sigit (what a guy!)

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

mean_AGB_rate <- 100 * yr14val / yr50val
low_AGB_rate <- 100 * prop_df[prop_df$yr == 14, 3] / yr50val
high_AGB_rate <- 100 * prop_df[prop_df$yr == 14, 4] / yr50val


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

calcCarbon <- function(sf_obj, agb_rate, soc_rate, nodata = F) {
  
  if(!("nodata" %in% colnames(sf_obj))) {
    
    sf_obj <- sf_obj %>%
      mutate(nodata = NA)
    
  }
  
  df <- sf_obj %>%
    st_set_geometry(NULL) %>%
    mutate(aqua_c = (aqucltr * AGB_AVG * agb_rate) + (aqucltr * SOC_AVG * soc_rate),
           aqua_c_se = (aqucltr * AGB_SE * agb_rate) + (aqucltr * SOC_SE * soc_rate),
           agri_c = (agrcltr * AGB_AVG * agb_rate) + (agrcltr * SOC_AVG * soc_rate),
           agri_c_se = (agrcltr * AGB_SE * agb_rate) + (agrcltr * SOC_SE * soc_rate),
           abnd_c = (abandnd * AGB_AVG * agb_rate) + (abandnd * SOC_AVG * soc_rate),
           abnd_c_se = (abandnd * AGB_SE * agb_rate) + (abandnd * SOC_SE * soc_rate),
           salt_c = (slt_frm * AGB_AVG * agb_rate) + (slt_frm * SOC_AVG * soc_rate),
           salt_c_se = (slt_frm * AGB_SE * agb_rate) + (slt_frm * SOC_SE * soc_rate),
           urbn_c = (urban * AGB_AVG * agb_rate) + (urban * SOC_AVG * soc_rate),
           urbn_c_se = (urban * AGB_SE * agb_rate) + (urban * SOC_SE * soc_rate), 
           na_c = (nodata * AGB_AVG * agb_rate) + (nodata * SOC_AVG * soc_rate),
           na_c_se = (nodata * AGB_SE * agb_rate) + (nodata * SOC_SE) * soc_rate) %>%
      dplyr::select(aqua_c, aqua_c_se, agri_c, agri_c_se, abnd_c, abnd_c_se,
                    salt_c, salt_c_se, urbn_c, urbn_c_se, na_c, na_c_se)

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

mg2014_hgn <- calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.43)
mg2014_mgn <- calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.26)
mg2014_lgn <- calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.08)

# High loss, varying gains for 2000 - 2014

mg2014_hls_hgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.43)
mg2014_hls_mgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.26)
mg2014_hls_lgn <- calcCarbon(mg2014_ls, agb_rate = 1, soc_rate = 0.67) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.08)

# Medium loss, varying gains for 2000 - 2014

mg2014_mls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.43)
mg2014_mls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.26)
mg2014_mls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.82, soc_rate = 0.54) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.08)

# Low loss, varying gains for 2000 - 2014

mg2014_lls_hgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.65, soc_rate = 0.43)
mg2014_lls_mgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.40, soc_rate = 0.26)
mg2014_lls_lgn <- calcCarbon(mg2014_ls, agb_rate = 0.47, soc_rate = 0.41) - calcCarbon(mg2014_gn, agb_rate = 0.24, soc_rate = 0.08)

carbonTable <- rbind(mg2000_hls, mg2000_mls, mg2000_lls,
                     mg2014_hls, mg2014_mls, mg2014_lls,
                     mg2014_hgn, mg2014_mgn, mg2014_lgn,
                     mg2014_hls_hgn, mg2014_hls_mgn, mg2014_hls_lgn,
                     mg2014_mls_hgn, mg2014_mls_mgn, mg2014_mls_lgn,
                     mg2014_lls_hgn, mg2014_lls_mgn, mg2014_lls_lgn) %>%
  as.data.frame() %>%
  mutate(year = c(rep("2000", 3), rep("2014", 15)),
         loss = c(rep(c("hgh", "med", "low"), 2), rep(NA, 3), rep("hgh", 3), rep("med", 3), rep("low", 3)),
         gain = c(rep(NA, 6), rep(c("hgh", "med", "low"), 4))) %>%
  mutate(ttl = (aqua_c + agri_c + abnd_c + salt_c + urbn_c + na_c) / 1000000,
         ttl_se = (aqua_c_se + agri_c_se + abnd_c_se + salt_c_se + urbn_c_se + na_c_se) / 1000000)

smryDat2000 <- carbonTable[1:9, ] %>%
  dplyr::select(year, loss, gain, ttl, ttl_se)

smryDat2014 <- carbonTable[10:18, ] %>%
  dplyr::select(year, loss, gain, ttl, ttl_se)


# Calculate net change for comparison

mg2000_df <- mg2000 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(ADM2_EN, mangrov)

mg2014_df <- mg2014 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(ADM2_EN, mangrov)

net <- mg2014_df %>%
  left_join(mg2000_df, by = "ADM2_EN", suffix = c("_14", "_00")) %>%
  replace(is.na(.), 0) %>%
  mutate(mangrov_net = mangrov_14 - mangrov_00) %>%
  dplyr::select(ADM2_EN, mangrov_net) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN")) %>%
  mutate(net_mg_c_ls_hgh = (mangrov_net * AGB_AVG * 1) + (mangrov_net * SOC_AVG * 0.67),
         net_mg_c_ls_se_hgh = (mangrov_net * AGB_SE * 1) + (mangrov_net * SOC_SE * 0.67),
         net_mg_c_ls_med = (mangrov_net * AGB_AVG * 0.82) + (mangrov_net * SOC_AVG * 0.54),
         net_mg_c_ls_se_med = (mangrov_net * AGB_SE * 0.82) + (mangrov_net * SOC_SE * 0.54),
         net_mg_c_ls_low = (mangrov_net * AGB_AVG * 0.47) + (mangrov_net * SOC_AVG * 0.41),
         net_mg_c_ls_se_low = (mangrov_net * AGB_SE * 0.47) + (mangrov_net * SOC_SE * 0.41)) %>%
  dplyr::select(ADM2_EN, net_mg_c_ls_hgh:net_mg_c_ls_se_low)

net_ttls <- as.data.frame(rbind(colSums(select(net, - ADM2_EN), na.rm = T) / 1000000))

net_total <- net_ttls$net_mg_c_ls_med
net_total_se <- net_ttls$net_mg_c_ls_se_med

smryDat2000 <- smryDat2000 %>%
  mutate(net_ttl = ifelse(loss == "hgh", net_ttls$net_mg_c_ls_hgh * -1, 
                          ifelse(loss == "med", net_ttls$net_mg_c_ls_med * -1, net_ttls$net_mg_c_ls_low * -1)),
         net_ttl_se = ifelse(loss == "hgh", net_ttls$net_mg_c_ls_se_hgh * -1,
                             ifelse(loss == "med", net_ttls$net_mg_c_ls_se_med * -1, net_ttls$net_mg_c_ls_se_low * -1)))

smryDat2014 <- smryDat2014 %>%
  mutate(net_ttl = ifelse(loss == "hgh", net_ttls$net_mg_c_ls_hgh * -1, 
                          ifelse(loss == "med", net_ttls$net_mg_c_ls_med * -1, net_ttls$net_mg_c_ls_low * -1)),
         net_ttl_se = ifelse(loss == "hgh", net_ttls$net_mg_c_ls_se_hgh * -1,
                             ifelse(loss == "med", net_ttls$net_mg_c_ls_se_med * -1, net_ttls$net_mg_c_ls_se_low * -1)))

allPrdsSmry <- data.frame(year = c("pre-1960 - 2000", "2000-2014, LULCC", "2000-2014, LULCC", "2000-2014, net"),
                          carbon = c(-34.56, -7.02, 3.93, -1.8),
                          error = c(-0.30, -0.06, 0.03, -0.02),
                          net = c(NA, -3.09, -3.09, -1.8),
                          style = c("Loss", "Loss", "Gain", "Net"))


write_csv(allPrdsSmry, "./data/processed/allPrdsSmry.csv")
write_csv(smryDat2014, "./data/processed/carbonSummary2014.csv")

#---------------------------------------------

#############################################
## Potential carbon gains from restoration ##
#############################################

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)

mg2014_rstrn <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID")) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aqucltr, agrcltr, abandnd, 
         AGB_AVG, AGB_SE, SOC_AVG, SOC_SE)

calcRestorationCarbon <- function(df, rstr_rate, agb_rate, soc_rate) {
  
  # df <- mg2014_rstrn
  # rstr_rate <- 0.01
  # agb_rate <- .40
  # soc_rate <- .10
  
  df_rstr <- df %>%
    mutate(aqucltr_rstr = aqucltr * rstr_rate,
           agrcltr_rstr = agrcltr * rstr_rate,
           abandnd_rstr = abandnd) %>%
    rowwise() %>%
    mutate(total = aqucltr_rstr + agrcltr_rstr + abandnd_rstr) %>%
    ungroup() %>%
    mutate(agb_rstr = total * agb_rate * AGB_AVG,
           agb_rstr_se = total * agb_rate * AGB_SE,
           soc_rstr = total * soc_rate * SOC_AVG,
           soc_rstr_se = total * soc_rate * SOC_SE) %>%
    dplyr::select(total, agb_rstr, agb_rstr_se, soc_rstr, soc_rstr_se)
    
  ttls <- colSums(df_rstr, na.rm = T)
  ttl <- sum(ttls[2] + ttls[4]) / 1000000
  ttl_se <- sum(ttls[3] + ttls[5]) / 1000000
  
  return(c(ttl, ttl_se))
  
}

agb <- 0.24  # high 65%, med 40%, low 24%
soc <- 0.08  # high 43%, med 26%, low 8%

calcRestorationCarbon(mg2014_rstrn, 0.001, agb, soc)
calcRestorationCarbon(mg2014_rstrn, 0.01, agb, soc)
calcRestorationCarbon(mg2014_rstrn, 0.02, agb, soc)
calcRestorationCarbon(mg2014_rstrn, 0.05, agb, soc)
calcRestorationCarbon(mg2014_rstrn, 0.1, agb, soc)

#----------------------------------------
# Do your own dishes

rm(list = ls())

gc()
