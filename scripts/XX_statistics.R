################
## Statistics ##
################

# Calculation of statistics for the land cover change study

library(tidyverse)
library(plotrix)

#-----------------------------------
# Statistics for soil parameters

soil_dat <- read_csv("./data/processed/th_soil_params.csv")

soil_dat %>%
  group_by(site, type) %>%
  summarize(bd_avg = mean(bd),
            bd_se = std.error(bd),
            poc_avg = mean(poc),
            poc_se = std.error(poc))


soil_dat %>%
  mutate(vol = ifelse(int_volume > 10000, 10000, int_volume),
         mass = bd * vol * 100 / 1000,
         depth = ifelse(avg_depth > 200, 200, avg_depth)) %>%
  group_by(site, type, plot, subplot, interval) %>%
  summarize(total_mass = sum(mass),
            depth = mean(depth),
            avg_bd = mean(bd),
            avg_poc = mean(poc),
            vol = mean(vol)) %>%
  group_by(site, type, interval) %>%
  summarize(avg_mass = mean(total_mass),
            se_mass = std.error(total_mass),
            avg_bd = mean(avg_bd),
            se_bd = std.error(avg_bd),
            depth = mean(depth),
            avg_poc = mean(avg_poc),
            vol = mean(vol)) %>% 
  group_by(site, type) %>%
  summarize(mass = sum(avg_mass),
            se_mass = std.error(avg_mass),
            wgt_poc = weighted.mean(avg_poc, vol),
            poc = mean(avg_poc),
            poc_se = std.error(avg_poc),
            depth = mean(depth))

soil_dat %>%
  group_by(site, type, interval) %>%
  summarize(bd_avg = mean(bd),
            bd_se = std.error(bd),
            poc_avg = mean(poc),
            poc_se = std.error(poc))

# For bulk density 

(kt_kre_bd <- kruskal.test(bd ~ type, data = filter(soil_dat, site == "Krabi")))
(kt_ppm_bd <- kruskal.test(bd ~ type, data = filter(soil_dat, site == "Nakorn")))
(kt_pre_bd <- kruskal.test(bd ~ type, data = filter(soil_dat, site == "Trang")))

# For percent organic carbon

(kt_kre_poc <- kruskal.test(poc ~ type, data = filter(soil_dat, site == "Krabi")))
(kt_ppm_poc <- kruskal.test(poc ~ type, data = filter(soil_dat, site == "Nakorn")))
(kt_pre_poc <- kruskal.test(poc ~ type, data = filter(soil_dat, site == "Trang")))
