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
library(raster)
library(rasterVis)
library(readxl)
library(sf)
library(tidyverse)

#--------------------

proc_dir <- "./data/processed/"
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


#------------------------------
# Raster plot with levelplot

gadm_sea <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  as("Spatial")
    
tha <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  filter(NAME_0 == "Thailand") %>%
  as("Spatial")

r <- raster(paste0(proc_dir, "rasters/mg2000.tif"))

filter_r <- r == 3
filter_r2 <- mask(r, filter_r, maskvalue = 1)

plot(gadm_sea$geometry)

levelplot(r, margin = list(axis = T, FUN = "sum"), colorkey = F,
          xlab = NULL, ylab = NULL) +
  layer(sp.polygons(gadm_sea, fill = "light grey", alpha = 0.2)) +
  layer(sp.polygons(tha, fill = "light grey", alpha = 0.2))
  

#---------------------------------------
# Build a plot to summarize the data:

tab <- data.frame(year = factor(c("1960-2000", "2000-2014, LULCC", "2000-2014, LULCC", "2000-2014, LULCC", "2000-2014, net")),
                  carbon = c(-41.3, -9.7, 3.4, -3.4, -2.3),
                  style = factor(c("Net", "Net", "Gain", "Loss", "Net"), levels = c("Net", "Gain", "Loss")),
                  alph = c(1, 1, 0.5, 0.5, 1),
                  fct = rep(" ", 5))

tab2 <- data.frame(loss = factor(c(rep("High Loss", 9), rep("Medium Loss", 9), rep("Low Loss", 9)), levels = c("High Loss", "Medium Loss", "Low Loss")),
                   scenario = factor(rep( c(rep("Low Gain", 3), rep("Medium Gain", 3), rep("High Gain", 3)), 3), 
                                     levels = c("Low Gain", "Medium Gain", "High Gain")),
                   value = c(-2.0, 2.0, -14.1, -3.4, 3.4, -12.7, -6.1, 6.1, -10.0, -2.0, 2.0, -11.1, -3.4, 3.4, -9.7, -6.1, 6.1, -7, -2.0, 2.0, -7.0, -3.4, 3.4, -5.6, -6.1, 6.1, -2.9),
                   type = rep(c("Loss", "Gain", "Net"), 9),
                   type2 = rep(c(rep("dotted", 2), "solid"), 9),
                   alph = rep(c(rep(0.5, 2), 1), 9))

p1 <- ggplot(tab) +
  facet_wrap(~fct) +
  geom_bar(aes(x = year, y = carbon, fill = factor(style, levels = c("Loss", "Gain", "Net")), 
               linetype = style), col = "black", stat = "identity", width = 0.6) +
  scale_fill_manual("Carbon Flux", values = c("Net" = "#44aa99", "Gain" = "#117733", "Loss" = "#88ccee")) +
  ylim(c(-42, 10)) +
  ggtitle("a) Carbon Stock Losses by Time Period") +
  labs(y = "Million Mg C") +
  guides(linetype = "none",
         alpha = "none") +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.5, 0.97),
        legend.direction = "horizontal")

p2 <- ggplot(tab2) +
  facet_wrap(~loss) +
  geom_bar(aes(x = scenario, y = value, fill = type, 
               linetype = factor(type2, levels = c("solid", "dotted"))), 
           col = "black", stat = "identity") +
  scale_fill_manual("Carbon Flux", values = c("Net" = "#44aa99", "Gain" = "#117733", "Loss" = "#88ccee")) +
  ylim(c(-42, 10)) +
  ggtitle("b) Variation in Loss and Gain Rate Assumptions") +
  guides(linetype = "none",
         alpha = "none") +
  labs(y = "Million Mg C") +
  theme_tufte() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


fig2 <- grid.arrange(p1, p2, nrow = 1, widths = c(0.33, 0.66))

ggsave("./figs/f2_stocks.jpg", fig2, width = 12, height = 5, units = c("in"), device = "jpeg")

#-----------------------------------------------
# Biomass growth plot

dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

prop_df <- read_csv("./data/processed/eq1_mdlRuns.csv")

growthMdl <- ggplot(dat, aes(yr, agb)) +
  geom_point(color = "dark grey") +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a / (1 + b * exp(1) ^ (-k * x)),
                                 start = list(a = 140, b = 13, k = 0.1)),
              data = dat,
              se = FALSE,
              color = "black") +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a / (1 + b * exp(1) ^ (-k * x)),
                                 start = list(a = 140, b = 13, k = 0.1)),
              data = prop_df,
              se = FALSE,
              color = "black",
              linetype = "dashed",
              aes(x = yr, y = lwr)) +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a / (1 + b * exp(1) ^ (-k * x)),
                                 start = list(a = 140, b = 13, k = 0.1)),
              data = prop_df,
              se = FALSE,
              color = "black",
              linetype = "dashed",
              aes(x = yr, y = upr)) +
  ylab("Aboveground Biomass Carbon (Mg / ha)") +
  xlab("Time Since Regeneration (Years)") +
  geom_vline(xintercept = 14,linetype = "dashed", color = "red") +
  theme_tufte()

ggsave("./figs/fig4_growth.jpg", growthMdl, width = 8, height = 5, units = c("in"), device = "jpeg")

