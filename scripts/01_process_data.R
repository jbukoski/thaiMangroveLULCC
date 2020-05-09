# 01_process_data.R

# A file to pre-process all of the datasets, might need to call bash scripts.
#
# Section 1. - Processing of DMCR 2002 and 2014 Land Use and Land Cover datasets


#-----------------------------------

print("Begin Step 1. processing of raw data...")

#----------------------

raw_dir <- "./data/raw/"
out_dir <- "./data/processed/"

#-----------------------------------
# Load in libaries

library(raster)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)

#-----------------------------
# Section 1

mg2000 <- st_read(paste0(in_dir, "shapefiles/MG_TYPE_43.shp")) %>%
  dplyr::select(-OBJECTID_1, -GZONE_NAME, -Shape_Leng)

mg2014 <- st_read(paste0(in_dir, "shapefiles/MG_TYPE_57.shp")) %>%
  dplyr::select(-OBJECTID, -lu_name, -Shape_Leng)

extent(mg2014) == extent(mg2000)

# Resolve differences in codes, adding missing land cover types first

w_dat <- st_sf("AREA_RAI" = 0, "Shape_Area" = 0, "CODE" = "W",
               geometry = st_sfc(st_multipolygon(), crs = "EPSG:32647"))

s_dat <- st_sf("AREA_RAI" = 0, "Shape_Area" = 0, "CODE" = "S",
               geometry = st_sfc(st_multipolygon(), crs = "EPSG:32647"))

mi_dat <- st_sf("AREA_RAI" = 0, "Shape_Area" = 0, "CODE" = "Mi",
                geometry = st_sfc(st_multipolygon(), crs = "EPSG:32647"))

mg2000 <- rbind(mg2000, w_dat, s_dat)
mg2014 <- rbind(mg2014, mi_dat)

# Collapsing common land cover types

adjust_codes <- function(tibb) {
  
  return (tibb %>%
    mutate(code_crctr = as.character(CODE)) %>%
    mutate(code_new = ifelse(code_crctr %in% c("FB", "FE", "FP"), "FnM", 
                             ifelse(code_crctr %in% c("Ur", "Pt"), "UrP", code_crctr))) %>%
    group_by(code_new) %>%
    summarize(area_rai = sum(AREA_RAI))
  )

}

mg2000_new <- adjust_codes(mg2000)
mg2014_new <- adjust_codes(mg2014)

# Reproject to equal area CRS (Albers SE Asia) and calculate areas

epsg102028 <- CRS("+proj=aea +lat_1=7 +lat_2=-32 +lat_0=-15 +lon_0=125 
                  +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

mg2000_albers <- st_transform(mg2000_new, epsg102028) %>%
  mutate(area_ha = st_area(geometry) / 10000,
         code_num = 1:nrow(mg2000_new)) %>%
  dplyr::select(code = code_new, code_num, area_rai, area_ha)

mg2014_albers <- st_transform(mg2014_new, epsg102028) %>%
  mutate(area_ha = st_area(geometry) / 10000,
         code_num = 1:nrow(mg2014_new)) %>%
  dplyr::select(code = code_new, code_num, area_rai, area_ha)

# Write to file

st_write(mg2000_albers, paste0(out_dir, "shapefiles/mg2000_102028.shp"), append = FALSE)
st_write(mg2014_albers, paste0(out_dir, "shapefiles/mg2014_102028.shp"), append = FALSE)

#------------------------------------------

####################
# Aligning rasters #
####################

#------------------------
# Use GDAL/OGR command line tools to process rasters given better memory issues in R
#
# See resample_rasters.sh, which does the following:
#   - 1. Resamples Sanderman SOC data to Simard AGB data
#   - 2. Produces a blank raster via the Simard AGB data to burn DMCR vector files into.
#   - 3. Burn 2000 and 2014 DMCR LULC vector data into the blank rasters.
#
# The outputs of the file are four rasters that exactly align: SOC, AGB, 2000 LULC, & 2014 LULC

source("./scripts/resample_raster.sh")

#---------------------------------

####################################
## Extract districts w/ mangroves ##
####################################


districts <- st_read(paste0(raw_dir, "shapefiles/tha_admbnda_adm2_rtsd_20190221.shp"))
mg2000 <- st_transform(st_read(paste0(in_dir, "shapefiles/MG_TYPE_43.shp")), 4326)

intersections <- st_intersects(districts, mg2000)
districts_mg <- districts[lengths(intersections) > 0, ]

plot(districts_mg["ADM1_EN"])
plot(mg2000["CODE"], add = T)

st_write(districts_mg, dsn = paste0(out_dir, "shapefiles/districts_mg"), layer = "districts_mg", driver = "ESRI Shapefile")


#-----------------------------
# Courtesy clean-up.

rm(list = ls())
gc()

#-----------------------------

print("End of Step 1. Processing of raw data")

