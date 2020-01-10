#!/bin/usr/bash

# Preprocessing of mangrove shapefiles
# 1. Clip to Thailand
# 2. Buffer by cms to repair geometries

# Import to R and run the following
# 3. Use raster:intersect() in R to clip mangrove extents by chongwat
# 4. Convert to equal area projection
# 5. Calculate area of each polygon
# 6. Save for visualization and export

# Convert vector files to rasters for analyses and plotting
# 7. Rasterize using GDAL

# For 1996

# Step 1. Clip to Thailand
ogr2ogr -spat 97 5 103 15 -clipsrc ./data/raw/tha_admbnda_adm0_rtsd_20190221.shp ./data/processed/GMW_1996_thai_clipped.shp ./data/raw/GMW_1996_v2.shp 

# Step 2. Buffer to repair geometries
ogr2ogr -dialect sqlite -sql "SELECT ST_BUFFER(geometry, 0.000001) FROM GMW_1996_thai" ./data/scratch/GMW_1996_buffered.shp ./data/processed/GMW_1996_thai_clipped.shp


# For 2016  

# Step 1. Clip to Thailand
ogr2ogr -spat 97 5 103 15 -clipsrc ./data/raw/tha_admbnda_adm0_rtsd_20190221.shp ./data/processed/GMW_2016_thai.shp ./data/raw/GMW_2016_v2.shp 

# Step 2. Buffer to repair geometries
ogr2ogr -dialect sqlite -sql "SELECT ST_BUFFER(geometry, 0.000001) FROM GMW_2016_thai" ./data/scratch/GMW_2016_thai_buffered.shp ./data/processed/GMW_2016_thai.shp


# Steps 3 - 6 in R script (spatial_analysis.R)

# Step 7
# For 1996

cp ./data/scratch/empty.tif ./data/scratch/GMW_1996_thai.tif
cp ./data/scratch/empty.tif ./data/scratch/GMW_2016_thai.tif

gdal_rasterize -burn 1 -l GMW_1996_thai ./data/processed/GMW_1996_thai/GMW_1996_thai.shp ./data/scratch/GMW_1996_thai.tif

gdal_rasterize -burn 2 -l GMW_2016_thai ./data/processed/GMW_2016_thai/GMW_2016_thai.shp ./data/scratch/GMW_2016_thai.tif

gdal_translate -of VRT ./data/processed/GMW_1996_thai.tif ./data/processed/gmw_1996_thai.vrt -a_nodata none
gdal_translate -of VRT ./data/processed/GMW_2016_thai.tif ./data/processed/gmw_2016_thai.vrt -a_nodata none

gdal_calc.py -A ./data/processed/GMW_1996_thai.tif -B ./data/processed/GMW_2016_thai.tif --calc="A+B" --outfile=./data/processed/luc.tif

gdal_translate -srs_nodata 0 -co COMPRESS=DEFLATE ./data/processed/luc.tif ./data/processed/gmw_luc.tif

