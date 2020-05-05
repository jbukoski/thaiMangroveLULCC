#!/usr/bin/bash

# Bash script to preprocess and rasterize the DMCR land use / land cover files.

ogr2ogr -t_srs EPSG:4326 mg2000_4326.shp MG_TYPE_43.shp
ogr2ogr -t_srs EPSG:4326 mg2014_4326.shp MY_TYPE_57.shp

# Use R to reclass several unknown or uncommon land cover classes.
# Purpose is to facilitate land cover change.
  
R --vanilla --no-readline -q  << 'EOF'

library(sf)
library(tidyverse)

mg2000 <- read_sf("./mg2000_4326.shp") %>%
  mutate(CODE = ifelse(CODE == "Mi", "Unk", CODE)) %>%
  arrange(CODE) %>%
  mutate(CODE_NUM = 1:length(CODE)) %>%
  dplyr::select(CODE, CODE_NUM)
  
mg2014 <- read_sf("./mg2014_4326.shp") %>%
  mutate(CODE = ifelse(CODE %in% c("S", "W", "Unk"), "Unk", CODE)) %>%
  arrange(CODE) %>%
  mutate(CODE_NUM = 1:length(CODE)) %>%
  mutate(CODE_NUM = ifelse(CODE_NUM == 12, 11, ifelse(CODE_NUM == 13, 12, CODE_NUM))) %>%
  dplyr::select(CODE, CODE_NUM)
  
st_write(mg2000, dsn = "./mg2000_edited.shp", layer = "mg2000_edited")
st_write(mg2014, dsn = "./mg2014_edited.shp", layer = "mg2014_edited")

EOF


ogr2ogr -dialect sqlite -sql "SELECT CODE_NUM, ST_BUFFER(geometry, 0.000001) FROM mg2000_edited" mg2000.shp mg2000_edited.shp
ogr2ogr -dialect sqlite -sql "SELECT CODE_NUM, ST_BUFFER(geometry, 0.000001) FROM mg2014_edited" mg2014.shp mg2014_edited.shp

gdal_rasterize -a code_num -l mg2000 ./data/processed/shapefiles/mg2000.shp ./data/scratch/blank.tif ./data/processed/rasters/lulc2000.tif
gdal_rasterize -a CODE_NUM -l mg2014 mg2014.shp lulc2014.tif

rm *_edited.* *_4326.*