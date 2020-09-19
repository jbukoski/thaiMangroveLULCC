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

print("Begin Step 3. modeling of district-level carbon stocks...")

#-----------------------------------
# Load in libaries

library(doParallel)
library(e1071)
library(EnvStats)
library(foreach)
library(gdalUtils)
library(gdata)
library(ggpubr)
library(ggthemes)
library(geostatsp)
library(gridExtra)
library(RandomFields)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
library(SpaDES)
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

agb_rmse <- 148.0  # Cross-validation RMSE value reported for SE Asia in Simard et al., 2019 SI File (rather high!)
soc_rmse <- 109.0  # Cross-validation RMSE value reported in Sanderman et al., 2018

# Source helper function to process data

source("./scripts/helperFunctions.R")

# Build cluster and set up log files.

cl <- makeCluster(3)
registerDoParallel(cl)
clusterEvalQ(cl, sink(paste0("~/Desktop/log_", Sys.getpid(), ".txt")))

libs <- c("raster", "rgdal", "rgeos", "sp", "sf", 
          "tidyverse", "geostatsp", "RandomFields", "SpaDES")

d1 <- calcDistrictCarbon(dstrcts, agb, agb_rmse, 1)
d2 <- calcDistrictCarbon(dstrcts, agb, agb_rmse, 2)
d3 <- calcDistrictCarbon(dstrcts, agb, agb_rmse, 3)

v1 <- as.numeric(d1[2:81])
v2 <- as.numeric(d1[2:81])
v3 <- as.numeric(d1[2:81])

ses <- data.frame(n = numeric(), se = numeric())

for(j  in 3:80) {
  
  s1 <- sample(v1, j)
  s2 <- sample(v2, j)
  s3 <- sample(v3, j)
  
  se1 <- plotrix::std.error(s1)
  se2 <- plotrix::std.error(s2)
  se3 <- plotrix::std.error(s3)
  
  out <- c(j, se1, se2, se3)
  
  ses <- rbind(ses, out)
  
}

colnames(ses) <- c("n", "d1", "d2", "d3")

write_csv(ses, "./data/processed/rsf_stderrs.csv")


# Calculate district level C averages and standard errors (took approximately 3 hrs distributed across 3 cores)

agb_vals <- foreach(i = 1:nrow(dstrcts), .packages = libs, .combine = "rbind") %dopar% { calcDistrictCarbon(dstrcts, agb, agb_rmse, i) }
soc_vals <- foreach(i = 1:nrow(dstrcts), .packages = libs, .combine = "rbind") %dopar% { calcDistrictCarbon(dstrcts, soc, soc_rmse, i) }

write_csv(as.data.frame(soc_vals), "./data/processed/raw_soc_runs.csv")

stopCluster(cl)

# Clean up datasets and write to file

agb_vals_df <- t(agb_vals) %>%
  as_tibble() %>%
  slice(2:41)

colnames(agb_vals_df) <- t(agb_vals)[1, ]

agb_vals_df <- as_data_frame(sapply(agb_vals_df, as.numeric))

agb_stats <- tibble(dstrct_cd = integer(), AGB_AVG = double(), AGB_SE = double())

for(i in 1:ncol(agb_vals_df)) {
  
  avg <- mean(pull(agb_vals_df, i), na.rm = T)
  se <- plotrix::std.error(pull(agb_vals_df, i), na.rm = T)
  if(is.finite(avg)) { avg <- avg } else {avg <- NA}
  
  vals <- tibble(dstrct_cd = i, AGB_AVG = avg, AGB_SE = se)
  
  agb_stats <- bind_rows(agb_stats, vals)
  
}

agb_vals_df <- cbind(pull(as_tibble(agb_vals), V1), agb_stats) %>%
  mutate(AGB_AVG = AGB_AVG * 0.47,
         AGB_SE = AGB_SE * 0.47) %>%
  rename(ADM2_EN = "pull(as_tibble(agb_vals), V1)",
         ADM2_CD = dstrct_cd)

write_csv(agb_vals_df, "./data/processed/agb_vals.csv")

#---------------------
# Repeat for SOC values

soc_vals_df <- t(soc_vals) %>%
  as_tibble() %>%
  slice(2:41)

colnames(soc_vals_df) <- t(soc_vals)[1, ]

soc_vals_df <- as_tibble(sapply(soc_vals_df, as.numeric))

soc_stats <- tibble(dstrct_cd = integer(), SOC_AVG = double(), SOC_SE = double())

for(i in 1:ncol(soc_vals_df)) {
  
  avg <- mean(pull(soc_vals_df, i), na.rm = T)
  se <- plotrix::std.error(pull(soc_vals_df, i), na.rm = T)
  if(is.finite(avg)) { avg <- avg } else {avg <- NA}
  
  vals <- tibble(dstrct_cd = i, SOC_AVG = avg, SOC_SE = se)
  
  soc_stats <- bind_rows(soc_stats, vals)
  
}

soc_vals_df <- cbind(pull(as_tibble(soc_vals), V1), soc_stats) %>%
  rename(ADM2_EN = "pull(as_tibble(soc_vals), V1)",
         ADM2_CD = dstrct_cd)

write_csv(soc_vals_df, "./data/processed/soc_vals.csv")


# Join the district C summaries to the dstrcts shapefile and write it to file.

dstrct_avgs <- agb_vals_df %>%
  left_join(soc_vals_df)

dstrcts_c <- dstrcts_sp %>% 
  st_as_sf() %>%
  left_join(dstrct_avgs, by = "ADM2_EN") %>%
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

calcRestorationCarbon(mg2014_rstrn, 0.001, agb, soc) * 10/14
calcRestorationCarbon(mg2014_rstrn, 0.01, agb, soc) * 10/14
calcRestorationCarbon(mg2014_rstrn, 0.02, agb, soc) * 10/14
calcRestorationCarbon(mg2014_rstrn, 0.05, agb, soc) * 10/14
calcRestorationCarbon(mg2014_rstrn, 0.1, agb, soc) * 10/14


#----------------------------------------
# Do your own dishes

rm(list = ls())

gc()
