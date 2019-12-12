#!/bin/usr/bash

# Preprocessing of mangrove shapefiles
# 1. Clip to Thailand
# 2. Buffer by cms to repair geometries
# 3. Use raster:intersect() in R to clip mangrove extents by chongwat
# 4. Convert to equal area projection
# 5. Calculate area of each polygon
# 6. Save for visualization and export

# For 1996

ogr2ogr -spat 97 5 103 15 -clipsrc ./data/raw/tha_admbnda_adm0_rtsd_20190221.shp ./data/processed/GMW_1996_thai_clipped.shp ./data/raw/GMW_1996_v2.shp 

ogr2ogr -dialect sqlite -sql "SELECT ST_BUFFER(geometry, 0.000001) FROM GMW_1996_thai" ./data/scratch/GMW_1996_buffered.shp ./data/processed/GMW_1996_thai_clipped.shp

#ogr2ogr -t_srs EPSG:102028 ./data/scratch/GMW_1996_thai_rpj.shp ./data/scratch/GMW_1996_buffered.shp
#ogr2ogr -sql "SELECT *, OGR_GEOM_AREA AS AREA FROM GMW_1996_thai_rpj" ./data/scratch/GMW_1996_areas.shp ./data/scratch/GMW_1996_thai_rpj.shp


# For 2016  

ogr2ogr -spat 97 5 103 15 -clipsrc ./data/raw/tha_admbnda_adm0_rtsd_20190221.shp ./data/processed/GMW_2016_thai.shp ./data/raw/GMW_2016_v2.shp 

ogr2ogr -dialect sqlite -sql "SELECT ST_BUFFER(geometry, 0.000001) FROM GMW_2016_thai" ./data/scratch/GMW_2016_thai_buffered.shp ./data/processed/GMW_2016_thai.shp

#ogr2ogr -t_srs EPSG:102028 ./data/processed/GMW_2016_thai_rpj.shp ./data/processed/GMW_2016_thai.shp
#ogr2ogr -sql "SELECT *, OGR_GEOM_AREA AS AREA FROM GMW_2016_thai_rpj" ./data/processed/GMW_2016_thai_areas.shp ./data/processed/GMW_2016_thai_rpj.shp






