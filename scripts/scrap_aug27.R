#############################################
## Modeling of AGB & SOC loss and recovery ##
#############################################

library(sf)
library(tidyverse)
library(propagate)

#------------------------
####################
## AGB Loss Model ##
####################

agb_gn <- read_csv("~/Desktop/")


#---------------------

dat <- read_csv("~/Desktop/restoration.csv", col_names = c("year", "lnrr"))

head(dat)

mdl1 <- glm(lnrr ~ log(year), data = filter(dat, year <= 50))

vals <- predict(year = 14, mdl1)

dat %>%
  filter(year <= 50) %>%
  ggplot() +
  theme_bw() + 
  geom_point(aes(x = year, y = lnrr)) +
  geom_smooth(aes(x = year, y = lnrr), method = "glm", formula = y ~ log(x))

exp(-0.077)


c_dat <- read_sf("./data/processed/shapefiles/dstrcts_c/") %>%
  st_set_geometry(NULL)

agb <- mean(c_dat$AGB_AVG, na.rm = T)
soc <- mean(c_dat$SOC_AVG, na.rm = T)

agb + soc * 0.92

(agb * 0.18 + soc * 0.46) + (agb * 0.65 + soc * 0.43)

322.5667 / (agb + soc)

d2 <- data.frame(x = seq(0, 2, by = 0.05), y = log(seq(0, 2, by = 0.05), base = exp(1)))

plot(d2$x, d2$y)


#--------------------
####################
## SOC Loss Model ##
####################

soc_loss <- read_csv("~/Desktop/soc_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

soc_ls_mdl <- lm(LNRR ~ Age, data = soc_loss)

soc_loss %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#---------------
########################
## SOC Recovery Model ##
########################

library(tidyverse)

dat <- read_csv("~/Desktop/soc_recovery.csv") %>%
  select(Study, Age, LNRR)

mdl <- glm(LNRR ~ log(Age), data = dat)

dat %>%
  ggplot() +
  geom_point(aes(x = Age, y = LNRR)) + 
  geom_smooth(aes(x = Age, y = LNRR), formula = y ~ log(x), method = "glm") +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


exp((coef(mdl)[1] - 1.96 * 0.12235) + ((coef(mdl)[2] - 1.96 * 0.04912) * log(14)))
exp(coef(mdl)[1] + (coef(mdl)[2] * log(14)))
exp((coef(mdl)[1] + 1.96 * 0.12235) + ((coef(mdl)[2] + 1.96 * 0.04912) * log(14)))


coef(mdl[1], complete = T)
