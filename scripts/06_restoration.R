##########################
## Restoration Analysis ##
##########################

library(tidyverse)

#---------------------------------------------
#############################################
## Potential carbon gains from restoration ##
#############################################

rstrn_dat <- read_csv("./data/processed/smry4rstrtn.csv")

vals <- c()

for(i in c(0.001, 0.01, 0.02, 0.05, 0.1)) {

  a <- rstrn_dat %>%
    rowwise() %>%
    mutate(aqucltr = aqucltr * i,
           agrcltr = agrcltr * i,
           slt_frm = slt_frm * i,
           rstrn_area = sum(aqucltr, agrcltr, slt_frm, abandnd, na.rm = T),
           rstrn_c = (10 * MGC_GAIN_AVG / ACT_YR) * rstrn_area,
           rstrn_c_se = (10 * MGC_GAIN_SE / ACT_YR) * rstrn_area) %>%
    ungroup() %>%
    summarize(c = sum(rstrn_c, na.rm = T) / 1000000,
              se = sum(rstrn_c_se, na.rm = T) / 1000000)
  
  vals <- bind_rows(vals, a)

}

vals

# Percent of INDC

indc_commitment <- 30.3   # Reduction commitment by 2030

1.77 / 30.3   # % of INDC from best case mangrove restoration scenario

# Halting continued loss of mangroves

6.15 / 14 * 10




