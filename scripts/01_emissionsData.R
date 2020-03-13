########################
## 01_emissionsData.R ##
########################

# This script estimates the carbon stocks of intact mangroves as well as aquaculture ponds in Thailand.

# Imports plotwise field data for three sites from Thailand:
#
#  - the Krabi River Estuary,   
#  - the Palian River Estuary, and
#  - the Pak Panang Mangrove 
# 
# This script generates site wide estimates of carbon stocks   and
# soil parameters for each of the three sites.  

# Inputs:
#  1. Excel file containing field data

# Outputs 
#  2. CSV files of processed data


print("02_site_analysis.R running...")

#--------------------------------------
# Load libraries and begin script

suppressMessages(library("magrittr"))
suppressMessages(library("tidyverse"))
suppressMessages(library("raster"))
suppressMessages(library("readxl"))
suppressMessages(library("sp"))
suppressMessages(library("plotrix"))

#----------------------
# Load in helper functions & allometry functions

source("./scripts/allometry.R")

#----------------------
# Specify in/out directories

in_dir <- "./data/raw/"
out_dir <- "./data/processed/"

#---------------------------------------
## Load in data

excel_dat <- paste0(in_dir, "full_data.xlsx")

meta <- read_excel(excel_dat, sheet="metadata", col_names = T)

raw_trees <- read_excel(excel_dat, sheet="trees", col_names = T,
                        col_types = c(rep("text", 10), rep("numeric", 4), rep("text", 2))) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("[. ]", "_", colnames(.))) %>%
  dplyr::select(-date, -recorder, -checked_by, -entered_by)

raw_saps <- read_excel(excel_dat, sheet="saplings", col_names = T,
                       col_types = c(rep("text", 10), rep("numeric", 4), rep("text", 1))) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("[. ]", "_", colnames(.))) %>%
  dplyr::select(-date, -recorder, -checked_by, -entered_by)

raw_seedlings <- read_excel(excel_dat, sheet="seedlings", col_names = T) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("[. ]", "_", colnames(.))) %>%
  dplyr::select(-date, -recorder, -checked_by, -entered_by)

raw_cwd <- read_excel(excel_dat, sheet="cwd", col_names = T,
                      na = "NA") %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("[ .]", "_", colnames(.))) %>%
  dplyr::select(-date, -recorder, -checked_by, -entered_by)

raw_soil <- read_excel(excel_dat, sheet="soil", col_names = T) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("[. ]", "_", colnames(.))) %>%
  mutate(int_a = as.numeric(int_a),
         int_b = as.numeric(int_b),
         avg_depth = as.numeric(avg_depth)) %>%
  dplyr::select(-date, -recorder, -checked_by, -entered_by,
                -actual_int_start, -actual_int_stop, -salinity_ppt,
                -soil_color, -texture, -depth_1, -depth_2, -depth_3)

#-------------------------
# Drop incomplete plots from Trang

drop_ids <- raw_trees[is.na(raw_trees$full_species) | is.na(raw_trees$dbh_cm), ] %>%
  dplyr::select(plot) %>%
  distinct %>%
  pull(plot)

#-----------------------------------------
# Specify necessary parameters

plot_size <- (7^2)*pi
subplot_size <- (2^2)*pi
transect_size <- 5 * plot_size

t_val <- qt(0.975, 7-1)

# Specify site areas

site_areas <- tibble(site = c("Krabi", "Nakorn", "Trang", "Krabi", "Nakorn", "Trang"),
                     type = c("forest", "forest", "forest", "aquaculture", "aquaculture", "aquaculture"),
                     area = c(76110000, 110510000, 70650000, 1000000, 1000000, 1000000))

#------------------------------------------------------------------------------
# Processing raw data

# Create species code and calculate basal area

trees <- raw_trees %>%
  separate(full_species, into = c("genus", "species"), sep = " ", remove = FALSE) %>%
  mutate(sps_code = paste0(tolower(substr(genus, 1, 2)), tolower(substr(species, 1, 2))),
         basal_area = pi * (dbh_cm/100/2)^2) %>%
  dplyr::select(-genus, -species)

# Calculate above-ground biomass using species-specific allometric equations. 
# Where species-specific equations are not available, used Komiyama et al 2005 
# general equation with species specific wood densities

trees <- trees[!is.na(trees$full_species),]
trees <- trees[!is.na(trees$dbh_cm),]
trees <- trees %>% filter(!(site == "Trang" & plot %in% drop_ids))
trees <- trees %>% filter(!sps_code %in% c("unun", "aceb", "phpa", "nyfr"))

trees <- trees %>%
  left_join(allom_lookup, by = c("sps_code" = "sps_code")) %>%
  mutate(params = map2(dbh_cm, density, list)) %>%
  mutate(agb = invoke_map_dbl(ag_form, params)) %>%
  mutate(bgb = invoke_map_dbl(bg_form, params))

# Adjust AGB variable based on 'status' variable
# Calculate cone if base_cm measurement exists, otherwise assume a cylinder

trees <- trees %>%
  mutate(top_diam = ifelse(status == 3 & !is.na(base_cm) & height_m < 1.37, 
                           base_cm - (100 * height_m * ((base_cm - dbh_cm) / (100 * height_m))),
                           base_cm - (100 * height_m * ((base_cm - dbh_cm) / 137)))) %>%
  mutate(stump.vol = ifelse(status == 3 & !is.na(base_cm) & height_m < 1.37, 
                            (pi * 100 * height_m) / 12 * (base_cm^2 + top_diam^2 + (base_cm * top_diam)),
                            ifelse(status == 3 & !is.na(base_cm) & height_m >= 1.37, 
                                   (pi * 100 * height_m) / 12 * (base_cm^2 + top_diam^2 + (base_cm * top_diam)), 
                                   NA))) %>%
  mutate(adj_agb = ifelse(is.na(status), agb,
                          ifelse(status == 1, 0.95 * agb, 
                                 ifelse(status == 2, 0.8 * agb, 
                                        ifelse(status == 3 & !is.na(base_cm), 
                                               density * stump.vol / 1000, 
                                               density * pi * dbh_cm * height_m * 100 / 1000)))))

trees <- trees %>%
  mutate(agb = adj_agb,
         biomass = adj_agb + bgb) %>%
  dplyr::select(-ag_form, -bg_form, -ag_ref, -bg_ref, 
                -params, -top_diam, -stump.vol, - adj_agb)

#-------------------------------------------------------------------------------
# Processing for saplings
# Follows same steps as for trees

saps <- raw_saps %>% 
  separate(full_species, into = c("genus", "species"), sep = " ", remove = FALSE) %>%
  mutate(sps_code = paste0(tolower(substr(genus, 1, 2)), tolower(substr(species, 1, 2)))) %>%
  dplyr::select(-genus, -species)

saps <- saps[!is.na(saps$full_species), ]
saps <- saps[!is.na(saps$dbh_cm), ]
saps <- saps %>% filter(!(site == "Trang" & plot %in% drop_ids))
saps <- saps %>% filter(!(sps_code %in% c("mytu", "unun", "phpa", "nyfr")))

saps <- saps %>%
  left_join(allom_lookup, by = c("sps_code" = "sps_code")) %>%
  mutate(params = map2(dbh_cm, density, list)) %>%
  mutate(agb = invoke_map_dbl(ag_form, params)) %>%
  mutate(bgb = invoke_map_dbl(bg_form, params))

saps <- saps %>%
  mutate(adj_agb = ifelse(is.na(status), agb,
                          ifelse(status == 1, 0.95 * agb, 0.8 * agb)))

saps <- saps %>%
  mutate(agb = adj_agb,
         biomass = agb + bgb) %>%
  dplyr::select(-ag_form, -bg_form, -ag_ref, -bg_ref, -params, - adj_agb)

#-------------------------------------------------------------
# Biomass: Join trees and saplings

biomass <- bind_rows(mutate(trees, stage = "tree"), mutate(saps, stage = "sapling"))

#-----------------------------------------------------------------------------
# Processing for coarse woody debris (CWD)

# Convert CWD measurements to mass C in Mg/ha following Kauffman & Donato 2012
# Mean specific gravities (g/cm^3) of CWD classes taken from K&D, 2012

cwd_params <- tibble(size = c("fine", "small", "medium", "large"),
                     density = c(0.48, 0.64, 0.71, 0.69),
                     avg_diam = c(0.43, 1.47, 4.52, NA))

raw_cwd <- raw_cwd %>% 
  filter(!(site == "Trang" & plot %in% drop_ids))

cwd <- raw_cwd %>%
  dplyr::select(-remarks) %>%
  gather(size, n, -site, -plot, -subplot, -transect, -type) %>%
  separate(size, c("size", "status"), fill="right") %>%
  left_join(cwd_params, by = "size") %>%
  mutate(trnsct_lngth = ifelse(size == "fine", 2,
                               ifelse(size == "small", 3,
                                      ifelse(size == "medium", 5, 12)))) %>%
  mutate(volume = ifelse(size != "large", (pi^2) * ((n * (avg_diam^2)) / (8 * trnsct_lngth)),
                         (pi^2) * (n^2) / (8 * trnsct_lngth)),
         mass = volume * density,
         adj_mass = ifelse(is.na(status), mass,
                           ifelse(status == "rotten", mass * 0.5, mass))) %>%
  group_by(site, type, plot, subplot, size, status) %>%
  summarise(total = mean(n), mass = mean(mass)) %>%
  group_by(site, type, plot, subplot) %>%
  mutate(subplot_mass = sum(mass)) %>%
  group_by(site, type, plot) %>%
  mutate(plot_mass = mean(subplot_mass))

cwd_subplot <- cwd %>%
  dplyr::select(site, type, plot, subplot, cwd_biom = subplot_mass)  %>%
  mutate(cwd_carb = cwd_biom *0.5,
         subplot = as.character(subplot)) %>%
  distinct

#------------------------------------------------------
# Processing for soil

soil_drop <- raw_soil %>%
  filter(is.na(avg_depth)) %>%
  dplyr::select(plot) %>%
  unique

filt_soil <- raw_soil[!is.na(raw_soil$avg_depth), ]
filt_soil <- filt_soil %>% 
  filter(!(site == "Trang" & plot %in% drop_ids))

soil <- filt_soil %>%
  mutate(c_dens = bd * (poc/100),
         int_volume = ifelse(interval == 5, 
                             ((avg_depth/100) - (int_a/100)) * 10000, 
                             ((int_b/100) - (int_a/100)) * 10000),
         soc_per_ha = int_volume * c_dens)

write_csv(soil, paste0(out_dir, "th_soil_params.csv"))

#-------------------------------------------------------------------------------
#------------------#
# Forest structure #
#------------------#

# Compute tree and sapling based forest structure variables.

plot_areas <- trees %>% 
  dplyr::select(site, type, plot, subplot) %>%
  distinct() %>%
  group_by(site, type, plot) %>%
  mutate(n_plot = n(),
         area_plot = pi * 7^2 * n_plot) %>%
  dplyr::select(site, type, plot, area_plot) %>%
  distinct

trees_structure <- trees %>%
  filter(!(sps_code %in% c("nyfr", "phpa", "caer"))) %>%
  dplyr::select(site, type, plot, subplot, dbh_cm, status, sps_code, basal_area, biomass) %>%
  left_join(plot_areas, by = c("site", "type", "plot")) %>%
  left_join(site_areas, by = c("site", "type")) %>%
  rename(site_m_sq = area) %>%
  group_by(site, type, plot) %>%
  mutate(dbh = mean(dbh_cm),
         n = 10000 * n() / area_plot,
         ba = 10000 * sum(basal_area) / area_plot,
         qmd = sqrt(sum(dbh_cm^2)/2),
         biomass = 10000 * sum(biomass) / area_plot / 1000) %>%     # divide by extra 1000 to convert to kg
  dplyr::select(site, type, plot, dbh, n, ba, qmd, area_plot, site_m_sq, biomass) %>%
  distinct

saps_structure <- saps %>%
  filter(!(sps_code %in% c("nyfr", "phpa", "caer"))) %>%
  dplyr::select(site, type, plot, subplot, dbh_cm, status, sps_code) %>%
  group_by(site, type, plot, subplot) %>%
  mutate(subplot_dbh = mean(dbh_cm),
         subplot_n = n()) %>%
  dplyr::select(site, type, plot, subplot, subplot_dbh, subplot_n) %>%
  distinct %>%
  group_by(site, type, plot) %>%
  mutate(plot_dbh = mean(subplot_dbh),
         plot_dbh_se = sqrt(var(subplot_dbh) / n()),
         plot_n = 10000 * mean(subplot_n) / plot_size,
         plot_n_se = 10000 * sqrt(var(subplot_n) / n()) / plot_size) %>%
  dplyr::select(site, type, plot, plot_dbh, plot_dbh_se, plot_n, plot_n_se) %>%
  distinct

#write_csv(trees_structure, paste0(out_dir, "structure.csv"))

#-------------------------------------------------------------------------------
#----------------------------#
# Carbon & biomass estimates #
#----------------------------#

# Obtain estimate of biomass per hectare based on all plots
# Outputs estimates of biomass in Mg/ha at the subplot, plot, and site aggregation
# Any of the trees, saplings, or biomass tibbles can be run through
# Calculation of mean and variance follows Gregoire and Valentine 20XX?

# Print summary table of total biomass & aboveground vs belowground carbon
# Note different units (biomass vs C) of reported values.

biomass_subplot <- biomass %>%
  dplyr::select(site, type, plot, subplot, biomass, agb, bgb) %>%
  left_join(site_areas, by = c("site", "type")) %>%
  group_by(site, type, plot, subplot) %>%
  summarise(agb_tau = mean(area, na.rm=T) * ( sum(agb, na.rm=T) / plot_size),
            bgb_tau = mean(area, na.rm=T)* ( sum(bgb, na.rm=T) / plot_size)) %>%
  group_by(site, type, plot, subplot) %>%
  left_join(site_areas, by = c("site", "type")) %>%
  mutate(avg_plot_agb = mean(agb_tau),
         avg_plot_bgb = mean(bgb_tau),
         agb_ha = 10 * avg_plot_agb / area,
         bgb_ha = 10 * avg_plot_bgb / area) %>%
  mutate(agc_ha = agb_ha * 0.47,
         bgc_ha = bgb_ha * 0.39) %>%
  dplyr::select(site, type, plot, subplot, 
                agb_ha, bgb_ha, agc_ha, bgc_ha) %>%
  distinct

#------------------------------------------------------------------------------
#-----------------------------#
# Analysis of soil properties #
#-----------------------------#

soc_subplot <- soil %>%
  group_by(site, type, plot, subplot) %>%
  mutate(subplot_c = sum(soc_per_ha)) %>%
  dplyr::select(site, type, plot, subplot, subplot_c) %>%
  distinct()

#-----------------------------------------------------------------------------

#-----------------------#
# Join all carbon pools #
#-----------------------#

c_summary_subplot <- soc_subplot %>% 
  left_join(biomass_subplot, by = c("site", "type", "plot", "subplot")) %>%
  left_join(cwd_subplot, by = c("site", "type", "plot", "subplot")) %>%
  mutate(agb_ha = ifelse(is.na(agb_ha) && type == "aquaculture", 0, agb_ha),
         bgb_ha = ifelse(is.na(bgb_ha) && type == "aquaculture", 0, bgb_ha),
         agc_ha = ifelse(is.na(agc_ha) && type == "aquaculture", 0, agc_ha),
         bgc_ha = ifelse(is.na(bgc_ha) && type == "aquaculture", 0, bgc_ha),
         cwd_biom = ifelse(is.na(cwd_biom) && type == "aquaculture", 0, cwd_biom),
         cwd_carb = ifelse(is.na(cwd_carb) && type == "aquaculture", 0, cwd_carb)) %>%
  rename(soc = subplot_c) %>%
  arrange(desc(type), site, plot, subplot)

write_csv(c_summary_subplot, paste0(out_dir, "th_carbon.csv"))
write_csv(meta, paste0(out_dir, "th_meta.csv"))

#----------------------------
# Clear environment and collect garbage to clear RAM

rm(list = ls())
gc()

##--------------------------

print("Finished processing Thailand field data. Output files in `./data/processed/` directory.")
