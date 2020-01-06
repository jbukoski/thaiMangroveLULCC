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
  mutate(mass = bd * int_volume * 100 / 1000) %>%
  group_by(site, type, plot, subplot, interval) %>%
  summarize(total_mass = sum(mass),
            depth = mean(avg_depth),
            avg_bd = mean(bd)) %>%
  group_by(site, type, interval) %>%
  summarize(avg_mass = mean(total_mass),
            se_mass = std.error(total_mass),
            avg_bd = mean(avg_bd),
            se_bd = std.error(avg_bd),
            depth = mean(depth)) %>% View

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
