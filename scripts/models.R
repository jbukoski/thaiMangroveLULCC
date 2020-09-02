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

agb_gn <- read_csv("~/Desktop/agb_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

agb_gn_mdl <- lm(LNRR ~ Age, data = agb_gn)

plt_agb_gn <- agb_gn %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plt_agb_gn

#--------------------
####################
## SOC Loss Model ##
####################

soc_loss <- read_csv("~/Desktop/soc_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

soc_ls_mdl <- lm(LNRR ~ Age, data = soc_loss)

plt_soc_loss <- soc_loss %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plt_soc_loss

#---------------
########################
## SOC Recovery Model ##
########################

library(tidyverse)

dat <- read_csv("~/Desktop/soc_recovery.csv") %>%
  dplyr::select(Study, Age, LNRR)

mdl <- glm(LNRR ~ log(Age), data = dat)

plt_soc_gn <- dat %>%
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

plt_soc_gn


exp((coef(mdl)[1] - 1.96 * 0.12235) + ((coef(mdl)[2] - 1.96 * 0.04912) * log(14)))
exp(coef(mdl)[1] + (coef(mdl)[2] * log(14)))
exp((coef(mdl)[1] + 1.96 * 0.12235) + ((coef(mdl)[2] + 1.96 * 0.04912) * log(14)))


#---------------------------
# Combine plots



