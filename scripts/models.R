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

plt_agb_ls <- agb_gn %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  ggtitle("AGC Loss") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plt_agb_ls

#--------------------
####################
## SOC Loss Model ##
####################

soc_loss <- read_csv("~/Desktop/soc_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

soc_ls_mdl <- lm(LNRR ~ Age, data = soc_loss)

plt_soc_ls <- soc_loss %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since disturbance (Years)") +
  ylab("ln(Response Ratio)") +
  ggtitle("SOC Loss") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plt_soc_loss

#---------------------------------------------------
########################
## AGB recovery model ##
########################

library(nls2)
library(propagate)
library(readxl)

# Data, provided by Sigit (what a guy!)

dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

# Model form is: y = a / (1 + b * e^-kx )

model <- nls(agb ~ a / (1 + b * exp(1) ^ (-k * yr)), start=list(a = 140, b = 13, k = 0.1), data = dat)

predictDat <- seq(0, max(dat$yr), by = 2)


plt_agb_gn <- dat %>%
  ggplot(aes(x = yr, y = agb)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a / (1 + b * exp(1) ^ (-k * x)),
              method.args = list(start = c(a = 138.794, b = 25.162, k = 0.197)),
              se = FALSE) +
  xlab("Time since disturbance (Years)") +
  ylab("Aboveground Biomass (Mg/ha)") +
  ggtitle("AGC Recovery") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

prop1 <- predictNLS(model, newdata = data.frame(yr = predictDat))

prop_df <- data.frame(yr = predictDat,
                      prdct = prop1$summary$Prop.Mean.1,
                      lwr = prop1$summary$`Prop.2.5%`,
                      upr = prop1$summary$`Prop.97.5%`)

eq1_mdlRuns <- write_csv(prop_df, "./data/processed/eq1_mdlRuns.csv")

# Estimate biomass recovered at 15 years

yr14val <- (138.7840 / (1 + 17.8151 * exp(1) ^ (-0.1765 * 14) ))

# Estimate biomass recovered at 40 years

yr50val <- (138.7840 / (1 + 17.8151 * exp(1) ^ (-0.1765 * 50) ))

mean_AGB_rate <- 100 * yr14val / yr50val
low_AGB_rate <- 100 * prop_df[prop_df$yr == 14, 3] / yr50val
high_AGB_rate <- 100 * prop_df[prop_df$yr == 14, 4] / yr50val


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
  ggtitle("SOC Recovery") +
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

library(grid)

grid.arrange(plt_agb_ls, plt_soc_ls, plt_agb_gn, plt_soc_gn)
