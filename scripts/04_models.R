#############################################
## Modeling of AGB & SOC loss and recovery ##
#############################################

library(ggthemes)
library(grid)
library(gridExtra)
library(nlme)
library(nls2)
library(readxl)
library(saemix)
library(sf)
library(tidyverse)

#------------------------
####################
## AGB Loss Model ##
####################

# Drop this as it looks weird and doesn't really make modeling sense.

agb_ls_dat <- read_csv("./data/raw/agb_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

agb_ls_mdl <- lm(LNRR ~ Age, data = agb_ls_dat)

#--------------------
####################
## SOC Loss Model ##
####################

# Old SOC data
#
# soc_ls_dat <- read_csv("./data/raw/soc_loss.csv", col_names = c("age", "LNRR")) %>%
#   mutate(age = round(Age, 0))

soc_ls_dat <- read_csv("./data/raw/Dataset 1_Updated_16012020.csv", skip = 1) %>%
  select(dataset_variable, short_reference, site_name, age = "time_since_landuse_1", LNRR = ln_res_ratio) %>%
  filter(dataset_variable == "Soil carbon stock") %>%
  drop_na()

soc_ls_mdl <- lme(LNRR ~ age, random = ~1|site_name, data = soc_ls_dat)

#---------------------------------------------------
########################
## AGB recovery model ##
########################

# Data, provided by Sigit but amended with own data

agb_gn_dat <- read_csv("./data/raw/agb_data.csv") %>%
  dplyr::select(short_reference, type = site_name, age = "regeneration age", 
                agc = mean, country, lat = Latitude, lon = Longitude) %>%
  filter(!(short_reference %in% c("Lunstrum_and_Chen_2014"))) %>%
  mutate(inv_age = 1/age) %>%
  filter(lat >= -15 & lat <= 15, age < 70)

saemix.data <- saemixData(name.data = agb_gn_dat,
                          name.group = "type",
                          name.predictors = "age",
                          name.response = "agc")

growth_lmemdl <- function(psi, id, x){
    
  t <- x[, 1]
  A <- psi[id, 1]
  k <- psi[id, 2]
  fpred <- A * (1 - exp(-k*t))^2
  return(fpred)
  
}

saemix.model <- saemixModel(model = growth_lmemdl, psi0  = c(A = 100, k = 0.05))
saemix.options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, displayProgress=FALSE, seed=632545)
saemix.fit1 <- saemix(saemix.model, saemix.data, saemix.options)

agb_gn_mdl <- saemix.fit1@results

# coefs <- saemix.fit1@results@fixed.effects
# st_errs <- saemix.fit1@results@se.fixed
# 
# x <- 1:70
# y <- coefs[1] * (1 - exp(-coefs[2]*x))^2
# y1 <- (coefs[1] + 1.65 * st_errs[1]) * (1 - exp(-(coefs[2] + 1.65 * st_errs[2]) * x)) ^ 2
# y2 <- (coefs[1] - 1.65 * st_errs[1]) * (1 - exp(-(coefs[2] - 1.65 * st_errs[2]) * x)) ^ 2

# line_dat <- data.frame(cbind(x, y))
# line_dat1 <- data.frame(cbind(x, y1))
# line_dat2 <- data.frame(cbind(x, y2))

# agb_gn_dat %>%
#   ggplot() +
#   geom_point(aes(x = age, y = agc, col = (type == "Anchor"))) +
#   geom_line(aes(x = x, y = y), data = line_dat) +
#   theme_bw() 



#---------------
########################
## SOC Recovery Model ##
########################

soc_gn_dat <- read_csv("./data/raw/soc_recovery.csv") %>%
  dplyr::select(Study, Age, LNRR) %>%
  mutate(age_tr = 1 - 1/(Age))

soc_gn_dat %>%
  ggplot() +
  geom_point(aes(x = Age, y = LNRR)) +
  geom_smooth(aes(x = Age, y = LNRR),
              data = soc_gn_dat,
              formula = y ~ log(x),
              method = lm) +
  theme_tufte()

soc_gn_mdl <- lme(LNRR ~ log(Age), random = ~1|Study, data = soc_gn_dat)

summary(soc_gn_mdl)

#-----------------
###########################
## Validate growth curve ##
###########################

rmse_df <- c()
sites <- unique(agb_gn_dat$type)
seeds <- round(runif(300)*10000, 0)

for(i in 1:300) {
  
  set.seed(seeds[i])
  idx <- sample(1:15, 4)
  out_sites <- sites[idx]

  trn_dat <- filter(agb_gn_dat, !(type %in% out_sites))
  test_dat <- filter(agb_gn_dat, type %in% out_sites)
  
  saemix.data <- saemixData(name.data = trn_dat,
                            name.group = "type",
                            name.predictors = "age",
                            name.response = "agc")
  
  saemix.fit1 <- saemix(saemix.model, saemix.data, saemix.options)
  
  A <- saemix.fit1@results@fixed.effects[1]
  k <- saemix.fit1@results@fixed.effects[2]
  
  pred <- A * (1 - exp(-k*test_dat$age))^2
  
  rmse_df <- c(rmse_df, rmse(pred, test_dat$agc))
  
}

rmse_df

mean(rmse_df)
sd(rmse_df)


df <- data.frame(run = numeric(), rmse_avg = numeric(), rmse_sd = numeric())

for(i in 2:300) {
  
  dat <- rmse_df[1:i]
  
  rmse_avg <- mean(dat)
  rmse_sd <- sd(dat)
  
  df <- bind_rows(df, c(run = i, rmse_avg = rmse_avg, rmse_sd = rmse_sd))
  
  
}


figS3 <- ggplot(df) +
  geom_point(aes(x = run, y = rmse_avg)) +
  ylab("Mean RMSE (Mg C/ha)") +
  xlab("Number of runs") +
  geom_hline(yintercept = 48.89, col = "red", lty = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./figs/fig_s3.jpg", figS3, device = "jpeg", width = 6, height = 4, units = "in")



#-------------------------
# Clean up work space

rm(agb_gn_dat, agb_ls_dat, soc_gn_dat, soc_ls_dat,
   saemix.data, saemix.fit1, saemix.model, saemix.options,
   growth_lmemdl)
