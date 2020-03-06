## Land use and land cover change for coastal regions of Thailand

#### Jacob J. Bukoski

The following suite of scripts describes the analysis of land use and land cover change in the coastal regions of Thailand.

---

### Datasets

  - 2000 DMCR (2043) coastal land use and land cover
  - 2014 DMCR (2057) coastal land use and land cover
  - Sanderman soil organic carbon
  - Simard mangrove aboveground biomass 
      
---
      
### Steps of analysis

Rasterization of the DMCR land use and land cover shapefiles.

Datasets:

  - **Script**: rasterize.sh
  - **Input datasets**: MG_TYPE_43.shp, MG_TYPE_57.shp, empty.tif
  - **Output datasets**: lulc2000.tif, lulc2014.tif
  
Analysis steps:

  1. Reproject to EPSG:4326
  2. Harmonize land use and land cover classes
  3. Buffer vectors by cms to repair geometries
  4. Rasterize using GDAL and write out raster files
  
---

#### Land use and land cover change analysis

Datasets: 

  - **Script**: 
  - **Input datasets**: lulc2000.tif, lulc2014.tif
  - **Output datasets**: 

Analysis steps:

  1. Cross-tabulate the LULC rasters
  2. Identify key land use and land cover transiitons
  3. Quantify newly formed coastal land w/ established mangrove

---

#### Carbon quantification

  1. Back-filling of carbon stocks for historical mangroves
  2. Conceptual model of carbon emissions from mangrove loss
      1. Mangrove loss (mangrove -> aquaculture and/or agriculture)
      2. Mangrove gain (aquaculture and/or agriculture -> mangrove)
      3. No change - static? soil C sequestration?

---

#### Prioritization and restoration modeling

To be hashed out...