# 02_lulcc_analysis.R

# This script performs the land use and land cover change analysis using 
# two datasets.
#   a. Department of Marine and Coastal Resources land use and land cover
#      change dataset in Thai mangroves
#   b. Global Mangrove Watch raster of binary mangrove extent, clipped to
#      Thailand's borders.

# The objective is to produce a land use and land cover change matrix for
# land use and land cover change categories in Thailand.

#-----------------------------

print("Running Step 2. land use and land cover change analysis...")

#-----------------------------
# Load necessary libraries

library(raster)
library(rgdal)
library(sf)
library(sp)
library(spsurvey)
library(tidyverse)

#-----------------------------

raw_dir <- "./data/raw/"
in_dir <- "./data/processed/"
table_dir <- "./tables/"

#-----------------------------
# Cross-tabling of rasterized DMCR data at a national scale

mg2000 <- raster(paste0(in_dir, "rasters/mg2000.tif"))
mg2014 <- raster(paste0(in_dir, "rasters/mg2014.tif"))

ct <- crosstab(mg2000, mg2014, useNA = T)
ct_df <- as.data.frame.matrix(ct)

# Cross-tabling of rasterized DMCR data at the province scale

prvncs <- st_read(paste0(in_dir, "cstl_prvncs/cstl_prvncs.shp"))%>%
  mutate(ADM1_ID = row_number()) %>%
  dplyr::select(ADM1_EN, ADM1_ID, region)

prvnc_df <- prvncs 
st_geometry(prvnc_df) <- NULL

# Set up data frames to write to with for loop.

loss_codes <- data.frame(province = NA, code = factor(c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, NA)))
gain_codes <- data.frame(province = NA, code = factor(c(1, 2, 3, 4, 5, 6, 7, 9, 10, NA)))

losses <- data.frame(province = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                     other_forest = NA, mudflats = NA, abandoned = NA, sand = NA, 
                     salt_farms = NA, urban = NA, water = NA, nodata = NA)

gains <- data.frame(province = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                    other_forest = NA, mudflats = NA, mines = NA, abandoned = NA,
                    salt_farms = NA, urban = NA, nodata = NA)

for(i in 1:nrow(prvncs)) {
  
  prvnc <- prvncs[i, ]
  
  mg2000_crp <- crop(mg2000, prvnc)
  mg2014_crp <- crop(mg2014, prvnc)
  
  ct <- crosstab(mg2000_crp, mg2014_crp, useNA = T)
  ct_df <- data.frame(ct)
  
  ct_loss <- filter(ct_df, mg2000 == 3)
  ct_gain <- filter(ct_df, mg2014 == 3)
  
  # Extract losses & gains vectors

  loss_vctr <- loss_codes %>% 
    left_join(ct_loss, by = c("code" = "mg2014")) %>%
    pull(Freq)

  gain_vctr <- gain_codes %>%
    left_join(ct_gain, by = c("code" = "mg2000")) %>%
    pull(Freq)
  
  # Write to final tables
  
  losses[i , ] <- c(i, loss_vctr)
  gains[i, ] <- c(i, gain_vctr)
  
  rm(prvnc, mg2000_crp, mg2014_crp, ct, ct_df, 
     ct_loss, ct_gain, loss_vctr, gain_vctr)
  
}

#Produce final table for losses

losses <- losses %>%
  left_join(prvnc_df, by = c("province" = "ADM1_ID")) %>%
  select(ADM1_ID = province, ADM1_EN, region, aquaculture, agriculture,
         mangrove, other_forest, mudflats, abandoned, sand, salt_farms,
         urban, water, nodata)

losses_total <- losses %>% 
  summarize(
    ADM1_ID = 9999,
    ADM1_EN = "Total",
    region = "all",
    aquaculture = sum(aquaculture, na.rm = T),
    agriculture = sum(agriculture, na.rm = T),
    mangrove = sum(mangrove, na.rm = T),
    other_forest = sum(other_forest, na.rm = T),
    mudflats = sum(mudflats, na.rm = T),
    abandoned = sum(abandoned, na.rm = T),
    sand = sum(sand, na.rm = T),
    salt_farms = sum(salt_farms, na.rm = T),
    urban = sum(urban, na.rm = T),
    water = sum(water, na.rm = T),
    nodata = sum(nodata, na.rm = T)
)

prvnc_losses <- bind_rows(losses, losses_total)

# Produce final table for gains

gains <- gains %>%
  left_join(prvnc_df, by = c("province" = "ADM1_ID")) %>%
  select(ADM1_ID = province, ADM1_EN, region, aquaculture, agriculture,
         mangrove, other_forest, mudflats, mines, abandoned, salt_farms, 
         urban, nodata)

gains_total <- gains %>% 
  summarize(
    ADM1_ID = 9999,
    ADM1_EN = "Total",
    region = "all",
    aquaculture = sum(aquaculture, na.rm = T),
    agriculture = sum(agriculture, na.rm = T),
    mangrove = sum(mangrove, na.rm = T),
    other_forest = sum(other_forest, na.rm = T),
    mudflats = sum(mudflats, na.rm = T),
    mines = sum(mines, na.rm = T),
    abandoned = sum(abandoned, na.rm = T),
    salt_farms = sum(salt_farms, na.rm = T),
    urban = sum(urban, na.rm = T),
    nodata = sum(nodata, na.rm = T)
  )

prvnc_gains <- bind_rows(gains, gains_total)

# Calculate areas for mangrove gains and losses for each province 
# Reproject to Albers projection then extract pixel size

epsg102028 <- CRS("+proj=aea +lat_1=7 +lat_2=-32 +lat_0=-15 +lon_0=125 
                  +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

mg2014_102028 <- projectRaster(mg2014, crs=epsg102028)
pxlSize <- xres(mg2014_102028) * yres(mg2014_102028)


prvnc_losses_ha <- prvnc_losses
prvnc_losses_ha[, 4:14] <- round(prvnc_losses[, 4:14] * pxlSize / 10000, 2)

prvnc_gains_ha <- prvnc_gains
prvnc_gains_ha[, 4:13] <- round(prvnc_gains[, 4:13] * pxlSize / 10000, 2)
  

