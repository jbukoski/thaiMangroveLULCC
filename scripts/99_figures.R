# 06_Figures.R
# Building of figures for manuscript

# Lis of figures will go here.

# Figure 1. Mangrove loss by district
# Figure 2. Mangrove stock density by district
# Figure 3. Estimated total emissions by district

#----------------------
# Load in necessary libaries

library(ggmap)
library(ggpubr)
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
        panel.background = element_rect(fill = "light blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

p1 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = (total - mangrove) / 1000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#228B22") +
  guides(fill = guide_legend(title = "Mangrove Area (10e3 ha)")) +
  annotate(geom = "text", label = "a",  x = 103.25, y = 13.5, size = 8, color = "black")
  
p2 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = mg_loss), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#FF0000") +
  guides(fill = guide_legend(title = "Mangrove Loss (%)")) +
  annotate(geom = "text", label = "b",  x = 103.25, y = 13.5, size = 8, color = "black")

p3 <- base + 
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2aqua), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#0000FF", breaks=c(0,20,40,60,80,100)) +
  guides(fill = guide_legend(title = "Loss to Aquaculture (%)")) +
  annotate(geom = "text", label = "c",  x = 103.25, y = 13.5, size = 8, color = "black")

p4 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2agri), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#FFAA1D") +
  guides(fill = guide_legend(title = "Loss to Agriculture (%)")) +
  annotate(geom = "text", label = "d",  x = 103.25, y = 13.5, size = 8, color = "black")

f1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave("./figs/f1_loss_map.jpg", f1, width = 10.2, height = 13.2, units = c("in"), device = "jpeg")

#--------------------------------
# Figure 2. District level C Stock estimates.


gadm_sea <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp"))

tha <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  filter(NAME_0 == "Thailand")

mg2000_df <- st_read(paste0(in_dir, "shapefiles/dstrct_ttls_2000"))
st_geometry(mg2000_df) <- NULL

mg2014_df <- st_read(paste0(in_dir, "shapefiles/dstrct_ttls_2000"))
st_geometry(mg2014_df) <- NULL

dstrct_c <- st_read(paste0(in_dir, "shapefiles/dstrcts_c/")) %>%
  mutate(ECO_AVG = AGB_AVG + SOC_AVG,
         ECO_SD = sqrt(AGB_SD^2 + SOC_SD^2)) %>%
  left_join(select(mg2000_df, ADM2_EN, mangrov_00 = mangrov, total_00 = total), by = c("ADM2_EN")) %>%
  left_join(select(mg2014_df, ADM2_EN, mangrov_14 = mangrov, total_14 = total), by = c("ADM2_EN"))


base <- ggplot(data = gadm_sea) +
  geom_sf(fill = "#F5F5F5", size  = 0.1) +
  geom_sf(data = tha, fill = "#E8E8E8", size = 0.2) +
  xlim(c(97.5, 103.5)) +
  ylim(c(5.75, 13.75)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "light blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

stocks1960 <- base + 
  geom_sf(data = dstrct_c, aes(fill = ECO_AVG * total_00 / 1000000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#228B22", breaks = c(0, 2, 4, 6, 8, 10)) +
  #annotate(geom = "text", label = "a",  x = 103.25, y = 13.5, size = 8, color = "black") +
  guides(fill = guide_legend(title = "Megatonnes C (1e6 Mg C)")) +
  ggtitle("Pre-1960")

leg <- get_legend(stocks1960)
stocks1960 <- stocks1960 +
  theme(legend.position = "NONE")

stocks2000 <- base +
  geom_sf(data = dstrct_c, aes(fill = ECO_AVG * mangrov_00 / 1000000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#228B22", breaks = c(0, 2, 4, 6, 8, 10)) +
  #annotate(geom = "text", label = "a",  x = 103.25, y = 13.5, size = 8, color = "black") +
  ggtitle("2000") +
  theme(legend.position = "NONE")

stocks2014 <- base +
  geom_sf(data = dstrct_c, aes(fill = ECO_AVG * mangrov_14 / 1000000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#228B22", breaks = c(0, 2, 4, 6, 8, 10)) +
  #annotate(geom = "text", label = "a",  x = 103.25, y = 13.5, size = 8, color = "black") +
  ggtitle("2014") +
  theme(legend.position = "NONE")

f2 <- grid.arrange(stocks1960, stocks2000, stocks2014, leg, ncol = 3, nrow = 2,
                   layout_matrix = rbind(c(1, 2, 3), c(4, 4, 4)), heights = c(1, 0.1))

ggsave("./figs/f2_cStocks.jpg", f2, width = 12, height = 5.75, units = c("in"), device = "jpeg")
