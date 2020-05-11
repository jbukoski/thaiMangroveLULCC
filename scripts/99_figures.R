# 06_Figures.R
# Building of figures for manuscript

# Lis of figures will go here.

# Figure 1. Mangrove loss by district
# Figure 2. Mangrove stock density by district
# Figure 3. Estimated total emissions by district

#----------------------
# Load in necessary libaries

library(ggmap)
library(grid)
library(gridExtra)
library(ggthemes)
library(tidyverse)

#--------------------

in_dir <- "./data/processed/"
scratch_dir <- "./data/scratch/"
raw_dir <- "./data/raw/"

#---------------------------------------
# Figure of mangrove extent estimates

dat <- read_csv(paste0(scratch_dir, "extent_estimates.csv"))

dat


ggplot(dat, aes(x = Year, y = Extent_ha)) +
  geom_bar(aes(fill = as.factor(Reference)), stat = "identity", 
           position = position_dodge2(width = 10000, preserve = "single")) +
  stat_smooth() +
  theme_bw() +
  ylab("Extent of Mangroves (ha)") +
  labs(fill = "Source") +
  theme(legend.position = "bottom")

  lm1 <- lm(dat$Extent_ha ~ dat$Year)
  
  
#---------------------------------------
# Figure 1. Mangrove loss by 1960 - 2014
  
gadm_sea <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp"))
  
tha <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  filter(NAME_0 == "Thailand")
  
mg2014 <- st_read(paste0(in_dir, "shapefiles/dstrct_ttls_2014")) %>%
    mutate(aqucltr = ifelse(is.na(aqucltr), 0, aqucltr),
           agrcltr = ifelse(is.na(agrcltr), 0, agrcltr),
           mangrov = ifelse(is.na(mangrov), 0, mangrov),
           mg_loss = 100 * (total - mangrov) / total,
           loss2aqua = 100 * (aqucltr / (total - mangrov)),
           loss2agri = 100 * (agrcltr / (total - mangrov)))

base <- ggplot(data = gadm_sea) +
  geom_sf(fill = "#F5F5F5", size  = 0.1) +
  geom_sf(data = tha, fill = "#E8E8E8", size = 0.2) +
  xlim(c(97.5, 103.5)) +
  ylim(c(5.75, 13.75)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "light blue"))

p1 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = total/ 1000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#228B22") +
  guides(fill = guide_legend(title = "Mangrove Area (10e3 ha)")) +
  annotate(geom = "text", label = "A",  x = 103.25, y = 13.5, size = 8, color = "black")
  
p2 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = mg_loss), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#FF0000") +
  guides(fill = guide_legend(title = "Mangrove Loss (%)")) +
  annotate(geom = "text", label = "B",  x = 103.25, y = 13.5, size = 8, color = "black")

p3 <- base + 
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2aqua), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#0000FF") +
  guides(fill = guide_legend(title = "Loss to Aquaculture (%)")) +
  annotate(geom = "text", label = "C",  x = 103.25, y = 13.5, size = 8, color = "black")

p4 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2agri), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#FFAA1D") +
  guides(fill = guide_legend(title = "Loss to Agriculture (%)")) +
  annotate(geom = "text", label = "D",  x = 103.25, y = 13.5, size = 8, color = "black")

f1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave("./figs/f1_loss_map.jpg", f1, width = 10.2, height = 13.2, units = c("in"), device = "jpeg")
