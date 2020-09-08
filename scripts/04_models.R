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

agb_ls_dat <- read_csv("~/Desktop/agb_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

agb_ls_mdl <- lm(LNRR ~ Age, data = agb_ls_dat)

agb_ls_plt <- agb_ls_dat %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since LULCC (Years)") +
  ylab("ln(Response Ratio)") +
  ggtitle("a. AGC Loss") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

agb_ls_plt

#--------------------
####################
## SOC Loss Model ##
####################

soc_ls_dat <- read_csv("~/Desktop/soc_loss.csv", col_names = c("Age", "LNRR")) %>%
  mutate(Age = round(Age, 0))

soc_ls_mdl <- lm(LNRR ~ Age, data = soc_ls_dat)

soc_ls_plt <- soc_ls_dat %>%
  ggplot(aes(x = Age, y = LNRR)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  xlab("Time since LULCC (Years)") +
  ylab("ln(Response Ratio)") +
  ggtitle("b. SOC Loss") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

soc_ls_plt

#---------------------------------------------------
########################
## AGB recovery model ##
########################

library(nls2)
library(propagate)
library(readxl)

# Data, provided by Sigit (what a guy!)

agb_gn_dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

# Model form is: y = a / (1 + b * e^-kx )

agb_gn_mdl <- nls(agb ~ a / (1 + b * exp(1) ^ (-k * yr)), start=list(a = 140, b = 13, k = 0.1), data = agb_gn_dat)

#predictDat <- seq(0, max(agb_gn_dat$yr), by = 2)

se_ribbon <- data.frame(yr = 1:70,
                        ymax = (138.79387 + 18.7) / (1 + (25.16169 - 18.1) * exp(1) ^ ((-0.19670 - 0.05) * 1:70)),
                        ymin = (138.79387 - 18.7) / (1 + (25.16169 + 18.1) * exp(1) ^ ((-0.19670 + 0.05) * 1:70)))

agb_gn_plt <- ggplot() +
  geom_ribbon(data = se_ribbon, aes(x = yr, ymin = ymin, ymax = ymax), fill = "grey85") +
  geom_smooth(data = agb_gn_dat,
              method = "nls",
              mapping = aes(x = yr, y = agb),
              formula = y ~ a / (1 + b * exp(1) ^ (-k * x)),
              method.args = list(start = c(a = 138.794, b = 25.162, k = 0.197)),
              se = FALSE) +
  geom_point(data = agb_gn_dat, aes(x = yr, y = agb)) +
  xlab("Time since LULCC (Years)") +
  ylab("Aboveground Biomass (Mg/ha)") +
  ggtitle("c. AGC Recovery") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

agb_gn_plt

#prop1 <- predictNLS(agb_gn_mdl, newdata = data.frame(yr = predictDat))

# prop_df <- data.frame(yr = predictDat,
#                       prdct = prop1$summary$Prop.Mean.1,
#                       lwr = prop1$summary$`Prop.2.5%`,
#                       upr = prop1$summary$`Prop.97.5%`)

eq1_mdlRuns <- write_csv(prop_df, "./data/processed/eq1_mdlRuns.csv")

#---------------
########################
## SOC Recovery Model ##
########################

soc_gn_dat <- read_csv("~/Desktop/soc_recovery.csv") %>%
  dplyr::select(Study, Age, LNRR)

soc_gn_mdl <- glm(LNRR ~ log(Age), data = soc_gn_dat)

soc_gn_plt <- soc_gn_dat %>%
  ggplot() +
  geom_point(aes(x = Age, y = LNRR)) + 
  geom_smooth(aes(x = Age, y = LNRR), formula = y ~ log(x), method = "glm") +
  xlab("Time since LULCC (Years)") +
  ylab("ln(Response Ratio)") +
  ggtitle("d. SOC Recovery") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

soc_gn_plt


#---------------------------
# Combine plots

all_plts <- grid.arrange(agb_ls_plt, soc_ls_plt, agb_gn_plt, soc_gn_plt)

ggsave("./figs/fig4_models.jpg", all_plts, height = 6, width = 8, device = "jpeg")
  
