library(tidyverse)

in_dir <- "./data/"

#------------------
# Figure of mangrove extent estimates

dat <- read_csv(paste0(in_dir, "scratch/extent_estimates.csv"))

dat


ggplot(dat, aes(x = Year, y = Extent_ha)) +
  geom_bar(aes(fill = as.factor(Reference)), stat = "identity", position = "dodge") +
  stat_smooth() +
  theme_bw() 

lm1 <- lm(dat$Extent_ha ~ dat$Year)
  