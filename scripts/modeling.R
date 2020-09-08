

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
  st_set_geometry(NULL) %>%
  arrange(ADM2_EN)

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


#-----------------------------
# Begin simulating carbon gains and losses


summary <- data.frame()

for(i in 1:nrow(dstrcts_c_df)) {
  
  vals <- c()
  dstrct <- dstrcts_c_df[i,]
  dstrct_ls <- mg2014_ls[i,]
  dstrct_gn <- mg2014_gn[i,]
  
  for(j in 1:3000) {
    
    act_yr <- round(runif(1, min = 1, max = 14), 0)
    agb_avg <- rnorm(1, mean = dstrct$AGB_AVG, sd = dstrct$AGB_SE * sqrt(40))
    agb_avg <- ifelse(agb_avg > 0, agb_avg, 0)
    soc_avg <- rnorm(1, mean = dstrct$SOC_AVG, sd = dstrct$SOC_SE * sqrt(40))
    soc_avg <- ifelse(soc_avg > 0, soc_avg, 0)
    
    agb_ls_coefs <- c(rnorm(1, mean = as.data.frame(summary(agb_ls_mdl)[[4]])[1, 1], sd = as.data.frame(summary(agb_ls_mdl)[[4]])[1, 2] * sqrt(4)),
                      rnorm(1, mean = as.data.frame(summary(agb_ls_mdl)[[4]])[2, 1], sd = as.data.frame(summary(agb_ls_mdl)[[4]])[2, 2] * sqrt(4)))
    
    for(k in 1:length(agb_ls_coefs)) {
      while(agb_ls_coefs[k] > 0) { agb_ls_coefs[k] <- rnorm(1, mean = as.data.frame(summary(agb_ls_mdl)[[4]])[k, 1], sd = as.data.frame(summary(agb_ls_mdl)[[4]])[k, 2] * sqrt(4)) }
    }
    
    soc_ls_coefs <- c(rnorm(1, mean = as.data.frame(summary(soc_ls_mdl)[[4]])[1, 1], sd = as.data.frame(summary(soc_ls_mdl)[[4]])[1, 2] * sqrt(34)),
                      rnorm(1, mean = as.data.frame(summary(soc_ls_mdl)[[4]])[2, 1], sd = as.data.frame(summary(soc_ls_mdl)[[4]])[2, 2] * sqrt(34)))
    
    for(k in 1:length(soc_ls_coefs)) {
      while(soc_ls_coefs[k] > 0) { soc_ls_coefs[k] <- rnorm(1, mean = as.data.frame(summary(soc_ls_mdl)[[4]])[k, 1], sd = as.data.frame(summary(soc_ls_mdl)[[4]])[k, 2] * sqrt(34)) }
    }
    
    agb_gn_coefs <- c(rnorm(1, mean = as.data.frame(summary(agb_gn_mdl)[[10]])[1, 1], sd = as.data.frame(summary(agb_gn_mdl)[[10]])[1, 2] * sqrt(59)),
                      rnorm(1, mean = as.data.frame(summary(agb_gn_mdl)[[10]])[2, 1], sd = as.data.frame(summary(agb_gn_mdl)[[10]])[2, 2] * sqrt(59)),
                      rnorm(1, mean = as.data.frame(summary(agb_gn_mdl)[[10]])[3, 1], sd = as.data.frame(summary(agb_gn_mdl)[[10]])[3, 2] * sqrt(59)))
    
    for(k in 1:length(agb_gn_coefs)) {
      while(agb_gn_coefs[k] < 0) { agb_gn_coefs[k] <- rnorm(1, mean = as.data.frame(summary(agb_gn_mdl)[[10]])[k, 1], sd = as.data.frame(summary(agb_gn_mdl)[[10]])[k, 2] * sqrt(59)) }
    }
    
    soc_gn_coefs <- c(rnorm(1, mean = as.data.frame(summary(soc_gn_mdl)[[12]])[1, 1], sd = as.data.frame(summary(soc_gn_mdl)[[12]])[1, 2] * sqrt(22)),
                      rnorm(1, mean = as.data.frame(summary(soc_gn_mdl)[[12]])[2, 1], sd = as.data.frame(summary(soc_gn_mdl)[[12]])[2, 2] * sqrt(22)))
    
    while(soc_gn_coefs[1] > 0) { soc_gn_coefs[1] <- rnorm(1, mean = as.data.frame(summary(soc_gn_mdl)[[12]])[1, 1], sd = as.data.frame(summary(soc_gn_mdl)[[12]])[1, 2] * sqrt(22)) }
    while(soc_gn_coefs[2] < 0) { soc_gn_coefs[2] <- rnorm(1, mean = as.data.frame(summary(soc_gn_mdl)[[12]])[2, 1], sd = as.data.frame(summary(soc_gn_mdl)[[12]])[2, 2] * sqrt(22)) }
    
    agc_prsrvd <- agb_avg * exp(agb_ls_coefs[1] + agb_ls_coefs[2] * act_yr)
    soc_prsrvd <- soc_avg * exp(soc_ls_coefs[1] + soc_ls_coefs[2] * act_yr)
    agc_gained <- agb_gn_coefs[1]  / (1 + agb_gn_coefs[2] * (exp(1) ^ (-agb_gn_coefs[3] * act_yr)))
    soc_gained <- soc_avg * exp(soc_gn_coefs[1]  + soc_gn_coefs[2] * log(act_yr))
    frgn_sqstr <- 1.5 * act_yr
    
    mgc_loss <- -1 * (agb_avg + soc_avg + frgn_sqstr - agc_prsrvd - soc_prsrvd)
    mgc_rcvr <- agc_gained + soc_gained - agc_prsrvd - soc_prsrvd
    mgc_expn <- agc_gained + soc_gained
    
    vals <- rbind(vals, c(dstrct$ADM2_ID, j, dstrct_ls$ttl_ls, dstrct_gn$ttl_gn - dstrct_gn$nodata, dstrct_gn$nodata, mgc_loss, mgc_rcvr, mgc_expn))
    
  }
  
  vals_df <- as.data.frame(vals) 
  
  colnames(vals_df) <- c("ADM2_ID", "j", "LOSS", "GAIN", "NODATA", "MGC_LOSS", "MGC_RCVR", "MGC_GAIN")
  
  dstrct_df <- vals_df %>%
    left_join(dplyr::select(dstrcts_c_df, ADM1_EN:ADM2_ID)) %>%
    mutate(MGC_LOSS_AVG = mean(MGC_LOSS, na.rm = T),
           MGC_LOSS_SE = plotrix::std.error(MGC_LOSS, na.rm = T),
           MGC_RCVR_AVG = mean(MGC_RCVR, na.rm = T),
           MGC_RCVR_SE = plotrix::std.error(MGC_RCVR, na.rm = T),
           MGC_GAIN_AVG = mean(MGC_GAIN, na.rm = T),
           MGC_GAIN_SE = plotrix::std.error(MGC_GAIN, na.rm = T)) %>%
    dplyr::select(ADM1_EN, ADM2_EN, ADM2_ID, LOSS:NODATA, MGC_LOSS_AVG:MGC_GAIN_SE) %>%
    mutate(GAIN = ifelse(is.na(GAIN), 0, GAIN),
           NODATA = ifelse(is.na(NODATA), 0, NODATA)) %>%
    distinct() %>%
    mutate(NET_MGC = (LOSS * MGC_LOSS_AVG) + (GAIN * MGC_RCVR_AVG) + (NODATA * MGC_GAIN_AVG),
           NET_MGC_SE = (LOSS * MGC_LOSS_SE) + (GAIN * MGC_RCVR_SE) + (NODATA * MGC_GAIN_SE))
  
  summary <- rbind(summary, dstrct_df)

  }
