##########################
## Restoration Analysis ##
##########################

library(tidyverse)

#---------------------------------------------

#############################################
## Potential carbon gains from restoration ##
#############################################

rstrn_dat <- read_csv("./data/processed/smry4rstrtn.csv")

rate <- 0.1

rstrn_dat %>%
  rowwise() %>%
  mutate(aqucltr = aqucltr * rate,
         agrcltr = agrcltr * rate,
         rstrn_area = sum(aqucltr, agrcltr, abandnd, na.rm = T),
         rstrn_c = (10 * MGC_GAIN_AVG / ACT_YR) * rstrn_area,
         rstrn_c_se = (10 * MGC_GAIN_SE / ACT_YR) * rstrn_area) %>%
  ungroup() %>%
  summarize(c = sum(rstrn_c, na.rm = T) / 1000000,
            se = sum(rstrn_c_se, na.rm = T) / 1000000)


# Percent of INDC

indc_commitment <- 30.3   # Reduction commitment by 2030

1.56 / 30.3   # % of INDC from best case mangrove restoration scenario

# Halting continued loss of mangroves

6.15 / 14 * 10




