#############################################
## Modeling of AGB & SOC loss and recovery ##
#############################################

library(grid)
library(gridExtra)
library(readxl)
library(sf)
library(tidyverse)

#------------------------
####################
## AGB Loss Model ##
####################

agb_ls_dat <- read_csv("./data/raw/agb_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

agb_ls_mdl <- lm(LNRR ~ Age, data = agb_ls_dat)

#--------------------
####################
## SOC Loss Model ##
####################

soc_ls_dat <- read_csv("./data/raw/soc_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

soc_ls_mdl <- lm(LNRR ~ Age, data = soc_ls_dat)

#---------------------------------------------------
########################
## AGB recovery model ##
########################

library(nls2)
library(readxl)

# Data, provided by Sigit (what a guy!)

agb_gn_dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

# Model form is: y = a / (1 + b * e^-kx )

agb_gn_mdl <- nls(agb ~ a / (1 + b * exp(1) ^ (-k * yr)), start=list(a = 140, b = 13, k = 0.1), data = agb_gn_dat)

#---------------
########################
## SOC Recovery Model ##
########################

soc_gn_dat <- read_csv("./data/raw/soc_recovery.csv") %>%
  dplyr::select(Study, Age, LNRR)

soc_gn_mdl <- glm(LNRR ~ log(Age), data = soc_gn_dat)

rm(agb_gn_dat, agb_ls_dat, soc_gn_dat, soc_ls_dat)
