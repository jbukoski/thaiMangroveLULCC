###############################################
## Modeling of C changes associated with LCC ##
###############################################

library(sf)
library(tidyverse)

#----------------------

raw_dir <- "./data/raw/"
proc_dir <- "./data/processed/"
scratch_dir <- "./data/scratch/"

#---------------------------------------------------
# Load in the models

source("./scripts/04_models.R")

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
  agc_ls_coefs_vctr <- c()
  soc_ls_coefs_vctr <- c()
  agc_gn_coefs_vctr <- c()
  soc_gn_coefs_vctr <- c()
  
  dstrct <- dstrcts_c_df[i,]
  dstrct_ls <- mg2014_ls[i,]
  dstrct_gn <- mg2014_gn[i,]
  
  #for(j in 1:100) {
  for(j in 1:100) {
    
    agb_avg <- -1
    soc_avg <- -1
    
    # Randomly generate activity year
    act_yr <- round(runif(1, min = 1, max = 14), 0)
    
    # Simulated mean district level carbon stocks
    while(agb_avg < 0 & !is.na(agb_avg)) { agb_avg <- rnorm(1, mean = dstrct$AGB_AVG, sd = dstrct$AGB_SE * sqrt(40)) }
    while(soc_avg < 0 & !is.na(soc_avg)) { soc_avg <- rnorm(1, mean = dstrct$SOC_AVG, sd = dstrct$SOC_SE * sqrt(40)) }
    
    # Get 95th CI for coefficients
    agb_ls_coefs <- as.data.frame(summary(agb_ls_mdl)[[4]])[, 1]
    agb_ls_ses <- as.data.frame(summary(agb_ls_mdl)[[4]])[, 2]
    #agb_ls_coefs <- agb_ls_coefs - (agb_ls_ses*1.96) # lower bound
    #agb_ls_coefs <- agb_ls_coefs + (agb_ls_ses*1.96) # upper bound
      
    soc_ls_coefs <- soc_ls_mdl$coefficients$fixed
    soc_ls_ses <- sqrt(diag(vcov(soc_ls_mdl)))
    #soc_ls_coefs <- soc_ls_coefs - (soc_ls_ses*1.96) # lower bound
    #soc_ls_coefs <- soc_ls_coefs + (soc_ls_ses*1.96) # upper bound
    
    agb_gn_coefs <- agb_gn_mdl@fixed.effects
    #agb_gn_coefs[1] <- agb_gn_coefs[1]*1.5   # Can vary the asymptote to test for sensitivity of including Thai data.
    agb_gn_ses <- agb_gn_mdl@se.fixed
    #agb_gn_coefs <- agb_gn_coefs - (agb_gn_ses*1.96) # lower bound
    #agb_gn_coefs <- agb_gn_coefs + (agb_gn_ses*1.96) # upper bound
    
    soc_gn_coefs <- soc_gn_mdl$coefficients$fixed
    soc_gn_ses <- sqrt(diag(vcov(soc_gn_mdl)))
    #soc_gn_coefs <- soc_gn_coefs - (soc_gn_ses*1.96) # lower bound   
    #soc_gn_coefs <- soc_gn_coefs + (soc_gn_ses*1.96) # upper bound
    
    agc_prsrvd <- agb_avg * exp(agb_ls_coefs[1] + agb_ls_coefs[2] * act_yr)
    soc_prsrvd <- soc_avg * exp(soc_ls_coefs[1] + soc_ls_coefs[2] * act_yr)
    agc_gained <- agb_gn_coefs[1]  * (1 - exp(-agb_gn_coefs[2] * act_yr))^2
    soc_gained <- soc_avg * exp(soc_gn_coefs[1]  + soc_gn_coefs[2] * log(act_yr))
    frgn_sqstr <- 1.5 * act_yr
    
    mgc_loss <- -1 * (agb_avg + soc_avg + frgn_sqstr - agc_prsrvd - soc_prsrvd)
    mgc_rcvr <- agc_gained + soc_gained - agc_prsrvd - soc_prsrvd
    mgc_rcvr <- ifelse(mgc_rcvr > 0, mgc_rcvr, 0)
    mgc_gain <- agc_gained + soc_gained - agc_prsrvd - soc_prsrvd
    mgc_gain <- ifelse(mgc_gain > 0, mgc_gain, 0)
    
    vals <- rbind(vals, c(dstrct$ADM2_ID, j, dstrct_ls$ttl_ls, dstrct_gn$ttl_gn - dstrct_gn$nodata, dstrct_gn$nodata, 
                          act_yr, agc_prsrvd, soc_prsrvd, agc_gained, soc_gained, frgn_sqstr, mgc_loss, mgc_rcvr, mgc_gain,
                          agb_avg, soc_avg, agc_gained, soc_gained))
    
    agc_ls_coefs_vctr <- rbind(agc_ls_coefs_vctr, agb_ls_coefs)
    soc_ls_coefs_vctr <- rbind(soc_ls_coefs_vctr, soc_ls_coefs)
    agc_gn_coefs_vctr <- rbind(agc_gn_coefs_vctr, agb_gn_coefs)
    soc_gn_coefs_vctr <- rbind(soc_gn_coefs_vctr, soc_gn_coefs)
    
    
  }
  
  vals_df <- as.data.frame(vals) 
  
  colnames(vals_df) <- c("ADM2_ID", "j", "LOSS", "GAIN", "NODATA", "ACT_YR",
                         "AGC_PRSRVD", "SOC_PRSRVD", "AGC_GAINED", "SOC_GAINED", "FRGN_SQSTR", 
                         "MGC_LOSS", "MGC_RCVR", "MGC_GAIN", "AGC_AVG", "SOC_AVG", "GROSS_AGC_GAIN", "GROSS_SOC_GAIN")
  
  dstrct_df <- vals_df %>%
    left_join(dplyr::select(dstrcts_c_df, ADM1_EN:ADM2_ID)) %>%
    mutate(ACT_YR = mean(ACT_YR, na.rm = T),
           AGC_AVG = mean(AGC_AVG, na.rm = T),
           SOC_AVG = mean(SOC_AVG, na.rm = T),
           MGC_LOSS_AVG = mean(MGC_LOSS, na.rm = T),
           MGC_LOSS_SE = plotrix::std.error(MGC_LOSS, na.rm = T),
           MGC_RCVR_AVG = mean(MGC_RCVR, na.rm = T),
           MGC_RCVR_SE = plotrix::std.error(MGC_RCVR, na.rm = T),
           MGC_GAIN_AVG = mean(MGC_GAIN, na.rm = T),
           MGC_GAIN_SE = plotrix::std.error(MGC_GAIN, na.rm = T),
           GRS_GAIN_AVG = mean(GROSS_AGC_GAIN, na.rm = T) + mean(GROSS_SOC_GAIN, na.rm = T),
           GRS_GAIN_SE = plotrix::std.error(GROSS_AGC_GAIN, na.rm = T) + plotrix::std.error(GROSS_SOC_GAIN, na.rm = T)) %>% 
    dplyr::select(ADM1_EN, ADM2_EN, ADM2_ID, LOSS:ACT_YR, AGC_AVG, SOC_AVG, MGC_LOSS_AVG:GRS_GAIN_SE) %>%
    mutate(GAIN = ifelse(is.na(GAIN), 0, GAIN),
           NODATA = ifelse(is.na(NODATA), 0, NODATA)) %>%
    distinct() %>%
    mutate(NET_MGC = (LOSS * MGC_LOSS_AVG) + (GAIN * MGC_RCVR_AVG) + (NODATA * MGC_GAIN_AVG),
           NET_MGC_SE = (LOSS * MGC_LOSS_SE) + (GAIN * MGC_RCVR_SE) + (NODATA * MGC_GAIN_SE))
  
  summary <- rbind(summary, dstrct_df)

}

soc_ls_coefs_vctr %>%
  as.data.frame() %>%
  summarize(V1_avg = mean(`(Intercept)`),
            V1_se = sqrt(var(`(Intercept)`)),
            V2_avg = mean(age),
            V2_se = sqrt(var(age)))

#----------------------------------
# Examine where the standard error of C losses, recovery, and gains stabilizes

my_seq <- seq(from = 4, to = 500, by = 2)

n_sims <- c()

for(j in my_seq) {
  
  loss_dat <- sample(vals_df$MGC_LOSS, j, replace = F)
  rcvr_dat <- sample(vals_df$MGC_RCVR, j, replace = F)
  gain_dat <- sample(vals_df$MGC_GAIN, j, replace = F)
  loss_se <- plotrix::std.error(loss_dat)
  rcvr_se <- plotrix::std.error(rcvr_dat)
  gain_se <- plotrix::std.error(gain_dat)
  
  n_sims <- rbind(n_sims, c(j, loss_se, rcvr_se, gain_se))
  
}

n_sims_df <- as.data.frame(n_sims) %>%
  rename("Loss" = V2,
         "Recovery" = V3,
         "Gain" = V4) %>%
  pivot_longer(cols = "Loss":"Gain", names_to = "var", values_to = "se")

n_sims_df %>%
  ggplot() +
  geom_point(aes(x = V1, y = se, color = var)) +
  xlab("Number of Simulations") +
  ylab("Standard Error (Mg C/ha)") +
  theme_bw() +
  geom_vline(xintercept = 400, linetype = "dashed") +
  theme(legend.position = c(0.6, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

#-------------------------------------
# Write out table to feed into plots

loss_c <- sum(summary$LOSS * summary$MGC_LOSS_AVG, na.rm = T) / 1000000
loss_c_se <- sum(summary$LOSS * summary$MGC_LOSS_SE, na.rm = T) / 1000000

gain_c <- sum(summary$NODATA * summary$MGC_GAIN_AVG, na.rm = T) / 1000000
gain_c_se <- sum(summary$NODATA * summary$MGC_GAIN_SE, na.rm = T) / 1000000

rcvr_c <- sum(summary$GAIN * summary$MGC_RCVR_AVG, na.rm = T) / 1000000
rcvr_c_se <- sum(summary$GAIN * summary$MGC_RCVR_SE, na.rm = T) / 1000000

net_c <- sum(summary$NET_MGC, na.rm = T)  / 1000000
net_c_se <- sum(summary$NET_MGC_SE, na.rm = T)  / 1000000

# Calculate net loss numbers

net_ls <- sum(mg2000$mangrov, na.rm = T) - sum(mg2014$mangrov, na.rm = T)
net_ls_c <- mean(summary$MGC_LOSS_AVG, na.rm = T)
net_ls_c_se <- sqrt(sum(summary$MGC_LOSS_SE ^2, na.rm = T))

net_ls * net_ls_c / 1000000
net_ls * net_ls_c_se / 1000000

# Calculate historical loss numbers

hist_nums <- mg2000 %>%
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  dplyr::select(ADM2_EN, mangrov, total, othr_fr, mudflts) %>%
  mutate(othr_fr = ifelse(is.na(othr_fr), 0, othr_fr),
         mudflts = ifelse(is.na(mudflts), 0, mudflts)) %>%
  mutate(historic_loss = total - (mangrov + othr_fr + mudflts)) %>%
  left_join(dplyr::select(summary, ADM2_EN, MGC_LOSS_AVG, MGC_LOSS_SE)) %>%
  mutate(hist_ls_c = historic_loss * MGC_LOSS_AVG,
         hist_ls_c_se = historic_loss * MGC_LOSS_SE)

hist_c <- sum(hist_nums$hist_ls_c, na.rm = T) / 1000000
hist_c_se <- sum(hist_nums$hist_ls_c_se, na.rm = T) / 1000000

# Combine all numbers for summary plot

allPrdsSmry <- data.frame(year = c("pre-1960 - 2000", "2000-2014, LULCC", "2000-2014, LULCC", "2000-2014, net"),
                          carbon = c(hist_c, loss_c, gain_c + rcvr_c, net_ls * net_ls_c / 1000000),
                          error = c(hist_c_se, loss_c_se, gain_c_se + rcvr_c_se, net_ls * net_ls_c_se / 1000000),
                          net = c(NA, net_c, net_c, net_ls * net_ls_c / 1000000),
                          style = c("Loss", "Loss", "Gain", "Net"))

allPrdsSmry

write_csv(allPrdsSmry, "./data/processed/allPrdsSmry.csv")

#------------------------------
# Summary

smry4rstrtn <- summary %>%
  dplyr::select(ADM1_EN:ADM2_ID, ACT_YR, AGC_AVG, SOC_AVG, MGC_GAIN_AVG, MGC_GAIN_SE) %>%
  left_join(select(st_set_geometry(mg2014, NULL), ADM2_EN, aqucltr, agrcltr, slt_frm, mangrov, abandnd))

write_csv(smry4rstrtn, "./data/processed/smry4rstrtn.csv")

#----------------------------
# Calculate net change

extant_c_2000 <- mg2000 %>%
  st_set_geometry(NULL) %>%
  mutate(mgc2000 = mangrov * (AGB_AVG + SOC_AVG),
         mgc2000_se = mangrov * (AGB_SE + SOC_SE)) %>%
  dplyr::select(mgc2000, mgc2000_se) %>%
  summarize(mgc2000_ttl = sum(mgc2000, na.rm = T) / 1000000,
            mgc2000_ttl_se = sum(mgc2000_se, na.rm = T) / 1000000)

extant_c_2014 <- mg2014_gn %>%
  left_join(select(summary, ADM2_EN, GAIN, NODATA, GRS_GAIN_AVG, GRS_GAIN_SE)) %>%
  mutate(mgc2014 = mangrov * (AGB_AVG + SOC_AVG) + ((GAIN + NODATA) * GRS_GAIN_AVG),
         mgc2014_se = mangrov * (AGB_SE + SOC_SE) + ((GAIN + NODATA) * GRS_GAIN_SE)) %>%
  dplyr::select(mgc2014, mgc2014_se) %>%
  ungroup() %>%
  summarize(mgc2014_ttl = sum(mgc2014, na.rm = T) / 1000000,
            mgc2014_ttl_se = sum(mgc2014_se, na.rm = T) / 1000000)

(extant_c_2000 - extant_c_2014) / extant_c_2000   # Does not include preserved carbon in deforested areas.
