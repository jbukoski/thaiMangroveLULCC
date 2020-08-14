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

library(magrittr)
library(raster)
library(rgdal)
library(sf)
library(sp)
library(spsurvey)
library(tidyverse)

#-----------------------------

raw_dir <- "./data/raw/"
proc_dir <- "./data/processed/"
table_dir <- "./tables/"

mg2000 <- raster(paste0(proc_dir, "rasters/mg2000.tif"))
mg2014 <- raster(paste0(proc_dir, "rasters/mg2014.tif"))

#-----------------------------
# Cross-tabling of rasterized DMCR data at a national scale

ct <- crosstab(mg2000, mg2014, useNA = T)
ct_df <- as.data.frame.matrix(ct)

# Compute numbers in thousands of hectares

ct_df_ha <- (ct_df * 937 / 10000 / 1000)
ct_df_ha[nrow(ct_df_ha), ncol(ct_df_ha)] <- 0
ct_df_ha$ttl <- rowSums(ct_df_ha)

new_row <- tibble(ct_df_ha[1, ])* 0

clean_ct <- round(bind_rows(ct_df_ha[1:8,], new_row, ct_df_ha[9,]), 1) %>%
  rbind(colSums(.)) %>%
  set_colnames(c("Aquaculture", "Agriculture", "Mangroves", "Other Forest",
                 "Mudflats", "Abandoned Land", "Salt Farms", "Developed Areas", 
                 "Water", "NA", "Totals (2000)")) %>%
  set_rownames(c("Aquaculture", "Agriculture", "Mangroves", "Other Forest",
                 "Mudflats", "Abandoned Land", "Salt Farms", "Developed Areas", 
                 "Water", "NA", "Totals (2014)"))

write_csv(clean_ct, "./data/processed/clean_lulcc_results.csv")

#-----------------------------
# Cross-tabling of rasterized DMCR data at the province scale

dstrcts <- read_sf(paste0(proc_dir, "shapefiles/districts_mg/districts_mg.shp")) %>%
  mutate(ADM2_ID = row_number()) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN)

dstrct_df <- dstrcts 
st_geometry(dstrct_df) <- NULL

# Set up data frames to write to with for loop.

loss_codes <- data.frame(district = NA, code = factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, NA)))
gain_codes <- data.frame(district = NA, code = factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, NA)))

losses <- data.frame(district = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                     other_forest = NA, mudflats = NA, abandoned = NA, salt_farms = NA, 
                     urban = NA, water = NA, nodata = NA)

gains <- data.frame(district = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                    other_forest = NA, mudflats = NA, abandoned = NA, salt_farms = NA, 
                    urban = NA, water = NA, nodata = NA)

for(i in 1:nrow(dstrcts)) {
  
  dstrct <- dstrcts[i, ]
  
  mg2000_crp <- crop(mg2000, dstrct)
  mg2014_crp <- crop(mg2014, dstrct)
  
  mg2000_msk <- mask(mg2000_crp, dstrct)
  mg2014_msk <- mask(mg2014_crp, dstrct)
  
  ct <- crosstab(mg2000_msk, mg2014_msk, useNA = T)
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
  
  rm(dstrct, mg2000_crp, mg2014_crp, ct, ct_df, 
     ct_loss, ct_gain, loss_vctr, gain_vctr)
  
}

#Produce final table for losses

losses <- losses %>%
  left_join(dstrct_df, by = c("district" = "ADM2_ID")) %>%
  select(ADM2_ID = district, ADM2_EN, ADM1_EN, aquaculture, agriculture,
         mangrove, other_forest, mudflats, abandoned, salt_farms,
         urban, water, nodata)

losses_total <- losses %>% 
  summarize(
    ADM2_ID = 9999,
    ADM2_EN = "Total",
    ADM1_EN = "all",
    aquaculture = sum(aquaculture, na.rm = T),
    agriculture = sum(agriculture, na.rm = T),
    mangrove = sum(mangrove, na.rm = T),
    other_forest = sum(other_forest, na.rm = T),
    mudflats = sum(mudflats, na.rm = T),
    abandoned = sum(abandoned, na.rm = T),
    salt_farms = sum(salt_farms, na.rm = T),
    urban = sum(urban, na.rm = T),
    water = sum(water, na.rm = T),
    nodata = sum(nodata, na.rm = T)
)

dstrct_losses <- bind_rows(losses, losses_total)

# Produce final table for gains

gains <- gains %>%
  left_join(dstrct_df, by = c("district" = "ADM2_ID")) %>%
  select(ADM2_ID = district, ADM2_EN, ADM1_EN, aquaculture, agriculture,
         mangrove, other_forest, mudflats, abandoned, salt_farms, 
         urban, water, nodata)

gains_total <- gains %>% 
  summarize(
    ADM2_ID = 9999,
    ADM2_EN = "Total",
    ADM1_EN = "all",
    aquaculture = sum(aquaculture, na.rm = T),
    agriculture = sum(agriculture, na.rm = T),
    mangrove = sum(mangrove, na.rm = T),
    other_forest = sum(other_forest, na.rm = T),
    mudflats = sum(mudflats, na.rm = T),
    abandoned = sum(abandoned, na.rm = T),
    salt_farms = sum(salt_farms, na.rm = T),
    urban = sum(urban, na.rm = T),
    nodata = sum(nodata, na.rm = T)
  )

dstrct_gains <- bind_rows(gains, gains_total)

# Calculate areas for mangrove gains and losses for each province 
# Reproject to Albers projection then extract pixel size

epsg102028 <- CRS("+proj=aea +lat_1=7 +lat_2=-32 +lat_0=-15 +lon_0=125 
                  +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#mg2014_102028 <- projectRaster(mg2014, crs=epsg102028)
#pxlSize <- xres(mg2014_102028) * yres(mg2014_102028)

pxlSize <- 927 # Hard code it to save time, but lines above produce same result

dstrct_losses_ha <- dstrct_losses
dstrct_losses_ha[, 4:13] <- round(dstrct_losses[, 4:13] * pxlSize / 10000, 2)

dstrct_gains_ha <- dstrct_gains
dstrct_gains_ha[, 4:13] <- round(dstrct_gains[, 4:13] * pxlSize / 10000, 2)

dstrcts_loss_sf <- dstrcts %>%
  left_join(dstrct_losses_ha, by = c("ADM1_EN", "ADM2_ID", "ADM2_EN")) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aquaculture, agriculture, mangrove,
                other_forest, mudflats, abandoned, salt_farms, urban, 
                water, nodata, geometry)

dstrcts_gain_sf <- dstrcts %>%
  left_join(dstrct_gains_ha, by = c("ADM1_EN", "ADM2_ID", "ADM2_EN")) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aquaculture, agriculture, mangrove, 
                other_forest, mudflats, abandoned, salt_farms, urban,
                water, nodata, geometry)
  

# Write out to file

st_write(dstrcts_loss_sf, dsn = paste0(proc_dir, "shapefiles/dstrct_losses_2014"), 
         layer = "dstrct_losses_2014", driver = "ESRI Shapefile", append = FALSE)

st_write(dstrcts_gain_sf, dsn = paste0(proc_dir, "shapefiles/dstrct_gains_2014"), 
         layer = "dstrct_gains_2014", driver = "ESRI Shapefile", append = FALSE)

rm(list = ls())

#----------------------------------
# Calculate losses in historically forested mangrove regions (1960 - 2000)

# Define Directories

raw_dir <- "./data/raw/"
proc_dir <- "./data/processed/"
table_dir <- "./tables/"


mg2000 <- raster(paste0(proc_dir, "rasters/mg2000.tif"))
mg2014 <- raster(paste0(proc_dir, "rasters/mg2014.tif"))

dstrcts <- read_sf(paste0(proc_dir, "shapefiles/districts_mg")) %>%
  mutate(ADM2_ID = row_number()) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN)

dstrct_df <- dstrcts 
st_geometry(dstrct_df) <- NULL


loss_codes_00 <- data.frame(district = NA, code = c(1:9))
loss_codes_14 <- data.frame(district = NA, code = c(1:9))

losses_2000 <- data.frame(district = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                          other_forest = NA, mudflats = NA, abandoned = NA, salt_farms = NA,
                          urban = NA, water = NA)

losses_2014 <- data.frame(district = NA, aquaculture = NA, agriculture = NA, mangrove = NA,
                          other_forest = NA, mudflats = NA, abandoned = NA, salt_farms = NA,
                          urban = NA, water = NA)


areaRaster <- area(mg2000)

for(i in 1:nrow(dstrcts)) {
  
  shp <- dstrcts[i, ]
  
  cropDat_00 <- crop(mg2000, shp)
  clipDat_00 <- mask(cropDat_00, shp)
  
  cropDat_14 <- crop(mg2014, shp)
  clipDat_14 <- mask(cropDat_14, shp)
  
  crop_areaRast <- crop(areaRaster, shp)
  
  zonalSums_00 <- as.data.frame(zonal(crop_areaRast, clipDat_00, fun = "sum"))
  zonalSums_14 <- as.data.frame(zonal(crop_areaRast, clipDat_14, fun = "sum"))
  
  vctr_00 <- loss_codes_00 %>% 
    left_join(zonalSums_00, by = c("code" = "zone")) %>%
    pull(sum)
  
  vctr_14 <- loss_codes_14 %>% 
    left_join(zonalSums_14, by = c("code" = "zone")) %>%
    pull(sum)
  
  losses_2000[i, ] <- c(i, vctr_00 * 100)  # Multiply by 100 to convert from km^2 to ha
  losses_2014[i, ] <- c(i, vctr_14 * 100)
  
}

losses_00 <- losses_2000 %>%
  left_join(dstrct_df, by = c("district" = "ADM2_ID")) %>%
  select(ADM2_ID = district, ADM2_EN, ADM1_EN, aquaculture, agriculture,
         mangrove, other_forest, mudflats, abandoned, salt_farms, urban)

losses_14 <- losses_2014 %>%
  left_join(dstrct_df, by = c("district" = "ADM2_ID")) %>%
  select(ADM2_ID = district, ADM2_EN, ADM1_EN, aquaculture, agriculture,
         mangrove, other_forest, mudflats, abandoned, salt_farms, urban, water)

losses_ttl_00 <- losses_00 %>% 
  summarize(ADM2_ID = 9999, ADM2_EN = "Total", ADM1_EN = "all",
            aquaculture = sum(aquaculture, na.rm = T),
            agriculture = sum(agriculture, na.rm = T),
            mangrove = sum(mangrove, na.rm = T),
            other_forest = sum(other_forest, na.rm = T),
            mudflats = sum(mudflats, na.rm = T),
            abandoned = sum(abandoned, na.rm = T),
            salt_farms = sum(salt_farms, na.rm = T),
            urban = sum(urban, na.rm = T))

losses_ttl_14 <- losses_14 %>% 
  summarize(ADM2_ID = 9999, ADM2_EN = "Total", ADM1_EN = "all",
            aquaculture = sum(aquaculture, na.rm = T),
            agriculture = sum(agriculture, na.rm = T),
            mangrove = sum(mangrove, na.rm = T),
            other_forest = sum(other_forest, na.rm = T),
            mudflats = sum(mudflats, na.rm = T),
            abandoned = sum(abandoned, na.rm = T),
            salt_farms = sum(salt_farms, na.rm = T),
            urban = sum(urban, na.rm = T),
            water = sum(water, na.rm = T))


dstrct_ttls_2000 <- bind_rows(losses_00, losses_ttl_00) %>%
  rowwise() %>%
  mutate(total = sum(aquaculture, agriculture, mangrove, other_forest, mudflats, 
                     abandoned, salt_farms, urban, na.rm = T)) %>%
  ungroup()

dstrct_ttls_2014 <- bind_rows(losses_14, losses_ttl_14) %>%
  rowwise() %>%
  mutate(total = sum(aquaculture, agriculture, mangrove, other_forest, mudflats, 
                     abandoned, salt_farms, urban, water, na.rm = T)) %>%
  ungroup()

# Convert to an SF object and clean up.
  
dstrct_ttls_2000_sf <- dstrcts %>%
  left_join(dstrct_ttls_2000, by = c("ADM1_EN", "ADM2_ID", "ADM2_EN")) %>%
  arrange(ADM1_EN, ADM2_EN) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aquaculture, agriculture, mangrove, 
                other_forest, mudflats, abandoned, salt_farms, urban, total, geometry)

dstrct_ttls_2014_sf <- dstrcts %>%
  left_join(dstrct_ttls_2014, by = c("ADM1_EN", "ADM2_ID", "ADM2_EN")) %>%
  arrange(ADM1_EN, ADM2_EN) %>%
  dplyr::select(ADM1_EN, ADM2_ID, ADM2_EN, aquaculture, agriculture, mangrove, 
                other_forest, mudflats, abandoned, salt_farms, urban, water, total, geometry)

# Write out to file.

st_write(dstrct_ttls_2000_sf, dsn = paste0(proc_dir, "shapefiles/dstrct_ttls_2000"), 
         layer = "dstrct_ttls_2000", driver = "ESRI Shapefile", append = FALSE)

st_write(dstrct_ttls_2014_sf, dsn = paste0(proc_dir, "shapefiles/dstrct_ttls_2014"), 
         layer = "dstrct_ttls_2014", driver = "ESRI Shapefile", append = FALSE)

#--------------------------------
# Clean up a bit

rm(list = ls())
gc()
