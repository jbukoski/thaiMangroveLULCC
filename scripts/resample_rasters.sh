#!/usr/bin/bash

# This script processes raster files to align their projections, extents, and resolution.
# The script inputs raster SOC, raster AGB, and vector LULC for 2000 and 2014.
# The script outputs aligned SOC, AGB, LULC 2000 & LULC 2014 rasters.


# 1. Resample Sanderman SOC raster to same resolution and extent as Simard AGB data

gdalwarp -tr 0.0002777777777779999848 0.0002777777777779999848 -r bilinear -co "COMPRESS=DEFLATE" -te 97.3937716 6.1166292 103.0812716 13.9116292 Mangrove_soc_Thailand.tif soc_resampled.tif

# 2. Create blank raster using Simard AGB data to burn the DMCR vector data into

gdal_calc.py --co="COMPRESS=DEFLATE" -A Mangrove_agb_Thailand.tif --outfile=empty.tif --calc "A*0" --NoDataValue=0

# 3. Burn 2000 and 2014 DMCR LULC vector data into empty.tif

cp empty.tif mg2000.tif
cp empty.tif mg2014.tif

ogr2ogr -t_srs EPSG:4326 ../../processed/shapefiles/mg2000_4326.shp ../../processed/shapefiles/mg2000_102028.shp
ogr2ogr -t_srs EPSG:4326 ../../processed/shapefiles/mg2014_4326.shp ../../processed/shapefiles/mg2014_102028.shp

gdal_rasterize -a code_num -l mg2000_4326 ../../processed/shapefiles/mg2000_4326.shp ../../processed/rasters/mg2000.tif
gdal_rasterize -a code_num -l mg2014_4326 ../../processed/shapefiles/mg2014_4326.shp ../../processed/rasters/mg2014.tif
