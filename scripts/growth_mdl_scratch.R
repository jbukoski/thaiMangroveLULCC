############################
## Growth model for Bodie ##
############################

library(ggthemes)
library(janitor)
library(nlme)
library(tidyverse)
library(readxl)

#---------------------------------------- 
# Load in data and produce clean modeling dataset 

dat <- read_csv("./data/processed/cleaned_data_w_GUO.csv")
traits <- read_csv("./data/processed/trait_data.csv")

mdl_dat <- dat %>%
  filter(!is.na(site_id)) %>%
  rename(binomial = species,
         biome = biome_name,
         age = stand_age) %>%
  mutate(age = round(age)) %>%
  separate(binomial, c("genus", "species"), sep = " ", remove = F) %>%
  left_join(traits) %>%
  separate(biome, sep = " ", into = "rgn", remove = FALSE) %>%
  mutate(type = paste(rgn, leaf_type)) %>%
  dplyr::select(agc, age, binomial, genus, study_id, site_id, plot_id, country, biome, type, prior:thinned, leaf_type:wood_dens)

#------------------------
# Bole adjustment

cmps <- read_excel("~/Desktop/components.xlsx", sheet = 2) %>%
  clean_names()

tempConf <- cmps %>%
  #filter(unit != "tree_biomass_kg") %>%
  left_join(unique(select(mdl_dat, binomial, type)), by = c("species" = "binomial")) %>%
  filter(type == "Temperate needleleaf") %>%
  mutate(perc_bole_ln = log(perc_bole),
         inv_ttl_agb = 1/total_agb,
         inv_bole_agb = 1/bole_agb,
         inv_age = 1/stand_age)

plot(tempConf$inv_age, tempConf$perc_bole_ln)

boleAdj_mdl <- lme(perc_bole_ln ~ inv_age, random = ~1|site_id, data = tempConf)

summary(boleAdj_mdl)

boleAdj_coefs <- boleAdj_mdl$coefficients$fixed

x <- 1:100
y <- exp(boleAdj_coefs[1] + boleAdj_coefs[2]/x)

plot(x, y, type = "l")

#------------------------
# For Pines

# Prep dataframe
cula <- mdl_dat %>%
  filter(type == "Temperate needleleaf") %>%
  mutate(bole_fctr = exp(boleAdj_coefs[1] + boleAdj_coefs[2]/agc),
         bole_agc = agc * bole_fctr,
         ttl_agc_ln = log(agc),
         bole_agc_ln = log(bole_agc),
         age_inv = 1/age) %>%
  group_by(site_id) %>%
  mutate(age_avg = mean(age),
         agc_avg = mean(agc)) %>%
  ungroup() %>%
  select(agc, bole_agc, bole_fctr, age, ttl_agc_ln, bole_agc_ln, age_inv, site_id, agc_avg, age_avg, type, genus, binomial) %>%
  unique()


cula %>%
  ggplot() +
  geom_point(aes(x = age, y = agc))

ttl_fe_mdl <- lm(ttl_agc_ln ~ age_inv, data = cula)
log_re_mdl <- lme(agc ~ log(age), random = ~1|site_id, data = cula)
ttl_re_mdl <- lme(ttl_agc_ln ~ age_inv, random = ~1|site_id, data = cula)

fe_coefs <- ttl_fe_mdl$coefficients
re_coefs <- ttl_re_mdl$coefficients$fixed
log_re_coefs <- log_re_mdl$coefficients$fixed
  
x <- 1:100
y_fe <- exp(fe_coefs[1] + fe_coefs[2] * 1/x)
y_re <- exp(re_coefs[1] + re_coefs[2] * 1/x)
y_log <- log_re_coefs[1] + log_re_coefs[2] * log(x)
  
xy <- data.frame(cbind(x, y_re))

cula %>%
  ggplot() +
  geom_point(aes(x = age, y = agc)) +
  geom_line(data = xy, aes(x = x, y = y_re), col = "red") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot(cula$age, cula$agc, xlab = "Stand age", ylab = "AGC (Mg/ha)")
lines(x, y_fe, col = "red", type = "l")
lines(x, y_re, type = "l", col = "blue")
lines(x, y_log, type = "l", col = "orange")


