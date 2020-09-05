

library(sf)
library(tidyverse)

#----------------------

raw_dir <- "./data/raw/"
proc_dir <- "./data/processed/"
scratch_dir <- "./data/scratch/"

#---------------------------------------------------
# Load in the models

source("./scripts/models.R")



###################################################################
## Calculate carbon stock losses and gains at the national scale ##
###################################################################

# Load in data

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)


mg2000 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2000")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

mg2014 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

mg2014_ls <- st_read(paste0(proc_dir, "shapefiles/dstrct_losses_2014")) %>%
  st_set_geometry(NULL) %>%
  rowwise() %>%
  mutate(mines = 0,
         ttl_ls = sum(aqucltr, agrcltr, othr_fr, mudflts, abandnd, slt_frm, urban, water, nodata, na.rm = T)) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

mg2014_gn <- st_read(paste0(proc_dir, "shapefiles/dstrct_gains_2014")) %>%
  st_set_geometry(NULL) %>%
  rowwise() %>%
  mutate(ttl_gn = sum(aqucltr, agrcltr, othr_fr, mudflts, abandnd, slt_frm, urban, water, nodata, na.rm = T)) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

# Add the emission factors based on the randomly assigned activity years and models

dstrcts_c_df <- dstrcts_c_df %>%
  rowwise() %>%
  mutate(activity_yr = 2014 - round(runif(1, min = 2000, max = 2013), 0),
         AGB_LS_EF = AGB_AVG * exp(coef(agb_ls_mdl)[1] + coef(agb_ls_mdl)[2] * activity_yr),
         AGB_LS_EF_SE = AGB_SE * exp(coef(agb_ls_mdl)[1] + coef(agb_ls_mdl)[2] * activity_yr),
         SOC_LS_EF = SOC_AVG * exp(coef(soc_ls_mdl)[1]  + coef(soc_ls_mdl)[2] * activity_yr),
         SOC_LS_EF_SE = SOC_SE * exp(coef(soc_ls_mdl)[1]  + coef(soc_ls_mdl)[2] * activity_yr),
         AGB_GN_EF = coef(agb_gn_mdl)[1]  / (1 + coef(agb_gn_mdl)[2] * (exp(1) ^ (-coef(agb_gn_mdl)[3] * activity_yr))),
         AGB_GN_EF_SE = summary(agb_gn_mdl)[["coefficients"]][1, 2]  / (1 + summary(agb_gn_mdl)[["coefficients"]][2, 2] * exp(1) ^ summary(agb_gn_mdl)[["coefficients"]][3, 2] * activity_yr),
         SOC_GN_EF = SOC_AVG * exp(coef(soc_gn_mdl)[1]  + coef(soc_gn_mdl)[2] * log(activity_yr)),
         SOC_GN_EF_SE = SOC_SE * exp(coef(soc_gn_mdl)[1]  + coef(soc_gn_mdl)[2] * log(activity_yr)),
         SEQ_EF = 1.74 * activity_yr,
         SEQ_EF_SE = 0.23 * activity_yr,
         ttl_ls_ef = AGB_LS_EF + SOC_LS_EF,
         ttl_ls_ef_se = AGB_LS_EF_SE + SOC_LS_EF_SE,
         ttl_gn_ef = AGB_GN_EF + SOC_GN_EF,
         ttl_gn_ef_se = AGB_GN_EF_SE + SOC_GN_EF_SE)


mg2014_ls_c <- mg2014_ls %>%
  mutate(aqucltr_c = aqucltr * ttl_ls_ef,
         aqucltr_c_se = aqucltr * ttl_ls_ef_se,
         agrcltr_c = agrcltr * ttl_ls_ef,
         agrcltr_c_se = agrcltr * ttl_ls_ef_se,
         othr_fr_c = othr_fr * ttl_ls_ef,
         othr_fr_c_se = othr_fr * ttl_ls_ef_se,
         mudflts_c = mudflts * ttl_ls_ef,
         mudflts_c_se = mudflts * ttl_ls_ef_se,
         abandnd_c = abandnd * ttl_ls_ef,
         abandnd_c_se = abandnd * ttl_ls_ef_se,
         slt_frm_c = slt_frm * ttl_ls_ef,
         slt_frm_c_se = slt_frm * ttl_ls_ef_se,
         urban_c = urban * ttl_ls_ef,
         urban_c_se = urban * ttl_ls_ef_se,
         water_c = water * ttl_ls_ef,
         water_c_se = water * ttl_ls_ef_se,
         nodata_c = nodata * ttl_ls_ef,
         nodata_c_se = nodata * ttl_ls_ef_se,
         seq_c = ttl_ls * SEQ_EF,
         seq_c_se = ttl_ls * SEQ_EF_SE,
         undist_c = ttl_ls * AGB_AVG + ttl_ls * SOC_AVG + seq_c) %>%
  dplyr::select(ADM1_EN, ADM2_EN, activity_yr, aqucltr_c:undist_c) %>%
  mutate(ttl_ls_c = sum(aqucltr_c, agrcltr_c, othr_fr_c, mudflts_c, abandnd_c, slt_frm_c, urban_c, water_c, nodata_c, na.rm = T),
         ttl_ls_c_se = sum(aqucltr_c_se, agrcltr_c_se, othr_fr_c_se, mudflts_c_se, abandnd_c_se, slt_frm_c_se, urban_c_se, water_c_se, nodata_c_se, na.rm = T),
         net_ls_c = undist_c - ttl_ls_c)

mg2014_ls_c %>%
  dplyr::select(undist_c, ttl_ls_c, net_ls_c) %>%
  ungroup() %>%
  summarize(undist_c = sum(undist_c, na.rm = T),
            ttl_ls_c = sum(ttl_ls_c, na.rm = T),
            net_ls_c = sum(net_ls_c, na.rm = T))


mg2014_gn_c <- mg2014_gn %>%
  mutate(aqucltr_c = aqucltr * ttl_gn_ef,
         aqucltr_c_se = aqucltr * ttl_gn_ef_se,
         agrcltr_c = agrcltr * ttl_gn_ef,
         agrcltr_c_se = agrcltr * ttl_gn_ef_se,
         othr_fr_c = othr_fr * ttl_gn_ef,
         othr_fr_c_se = othr_fr * ttl_gn_ef_se,
         mudflts_c = mudflts * ttl_gn_ef,
         mudflts_c_se = mudflts * ttl_gn_ef_se,
         abandnd_c = abandnd * ttl_gn_ef,
         abandnd_c_se = abandnd * ttl_gn_ef_se,
         slt_frm_c = slt_frm * ttl_gn_ef,
         slt_frm_c_se = slt_frm * ttl_gn_ef_se,
         urban_c = urban * ttl_gn_ef,
         urban_c_se = urban * ttl_gn_ef_se,
         water_c = water * ttl_gn_ef,
         water_c_se = water * ttl_gn_ef_se,
         nodata_c = nodata * ttl_gn_ef,
         nodata_c_se = nodata * ttl_gn_ef_se,
         seq_c = ttl_gn * SEQ_EF,
         seq_c_se = ttl_gn * SEQ_EF_SE) %>%
  dplyr::select(ADM1_EN, ADM2_EN, activity_yr, aqucltr_c:seq_c_se) %>%
  mutate(ttl_gn_c = sum(aqucltr_c, agrcltr_c, othr_fr_c, mudflts_c, abandnd_c, slt_frm_c, urban_c, water_c, nodata_c, na.rm = T) - seq_c,
         ttl_gn_c_se = sum(aqucltr_c_se, agrcltr_c_se, othr_fr_c_se, mudflts_c_se, abandnd_c_se, slt_frm_c_se, urban_c_se, water_c_se, nodata_c_se, na.rm = T) - seq_c_se)

mg2014_ls %>%
  mutate(woulda_been = (ttl_ls * AGB_AVG) + (ttl_ls * SOC_AVG)) %>%
  pull(woulda_been) %>%
  sum(na.rm = T)

mg2014_gn_c %>%
  pull(ttl_gn_c) %>%
  sum(na.rm = T)

mg2014_ls_c %>%
  pull(ttl_ls_c) %>%
  sum(na.rm = T)

mg2014_gn %>%
  mutate(extant_c_2014 = mangrov * AGB_AVG + mangrov * SOC_AVG) %>%
  pull(extant_c_2014) %>%
  sum(na.rm = T)


mg2000 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(ADM1_EN, ADM2_EN, mangrove_2000 = mangrov) %>%
  left_join(dplyr::select(st_set_geometry(mg2014, NULL), ADM1_EN, ADM2_EN, mangrove_2014 = mangrov)) %>%
  left_join(dstrcts_c_df) %>%
  mutate(mg_diff = mangrove_2000 - mangrove_2014,
         agb_ls = mg_diff * AGB_LS_EF,
         agb_ls_se = mg_diff * AGB_LS_EF_SE,
         soc_ls = mg_diff * SOC_LS_EF,
         soc_ls_se = mg_diff * SOC_LS_EF_SE,
         ttl_ls = agb_ls + soc_ls,
         ttl_ls_se = agb_ls_se + soc_ls_se,
         woulda_been_2000 = mg_diff * AGB_AVG + mg_diff * SOC_AVG,
         c_2000 = mangrove_2000 * AGB_AVG + mangrove_2000 * SOC_AVG,
         c_2000_se = mangrove_2000 * AGB_SE + mangrove_2000 * SOC_SE) %>%
  pull(woulda_been_2000) %>%
  sum(na.rm = T)
