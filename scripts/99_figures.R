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
library(ggsn)
library(ggspatial)
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
# Figure 1. Mangrove loss, 1960 - 2014
  
gadm_sea <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp"))
  
tha <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  filter(NAME_0 == "Thailand")
  
mg2014 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014")) %>%
    mutate(aqucltr = ifelse(is.na(aqucltr), 0, aqucltr),
           agrcltr = ifelse(is.na(agrcltr), 0, agrcltr),
           mangrov = ifelse(is.na(mangrov), 0, mangrov),
           mg_loss = 100 * (total - mangrov) / total,
           loss2aqua = 100 * (aqucltr / (total - mangrov)),
           loss2agri = 100 * (agrcltr / (total - mangrov)))

base <- ggplot(data = gadm_sea) +
  geom_sf(fill = "#F5F5F5", size  = 0.1) +
  geom_sf(data = tha, fill = "#E8E8E8", size = 0.2) +
  ylim(c(6, 14)) +
  scale_x_continuous(breaks = c(97, 99, 101, 103), limits = c(97, 103)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#C0E3F7"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)) +
  annotation_north_arrow(style = north_arrow_orienteering(
    text_col = "#C0E3F7", fill = c("#838383", "#838383"),
    line_col = "#838383", text_size = -10),
    height = unit(0.7, "cm"), width = unit(0.5, "cm"),
    pad_x = unit(0.8, "cm"), pad_y = unit(1.5, "cm")) +
  ggsn::scalebar(x.min = 97, x.max = 103,
                 y.min = 6, y.max = 14,
                 location = "bottomleft",
                 box.color = "#838383",
                 box.fill = c("#838383", "white"),
                 dist = 75, dist_unit = "km", height = 0.015,
                 st.bottom = FALSE, st.color = "#838383",
                 st.size = 3, border.size = 0.2,
                 transform = TRUE, model = "WGS84")

p1 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = (total) / 1000), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#009E73") +
  guides(fill = guide_legend(title = "Mangrove Area (kha)")) +
  annotate(geom = "text", label = "a",  x = 97.1, y = 14, size = 8, color = "black")
  
p2 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = mg_loss), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#CC79A7") +
  guides(fill = guide_legend(title = "Mangrove Loss (%)")) +
  annotate(geom = "text", label = "b",  x = 97.1, y = 14, size = 8, color = "black")

p3 <- base + 
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2aqua), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#0072B2", breaks=c(0,20,40,60,80,100)) +
  guides(fill = guide_legend(title = "Loss to Aquaculture (%)")) +
  annotate(geom = "text", label = "c",  x = 97.1, y = 14, size = 8, color = "black")

p4 <- base +
  geom_sf(data = mg2014, aes(geometry = geometry, fill = loss2agri), size = 0.2) +
  scale_fill_gradient(low = "#ffffff", high = "#D55E00") +
  guides(fill = guide_legend(title = "Loss to Agriculture (%)")) +
  annotate(geom = "text", label = "d",  x = 97.1, y = 14, size = 8, color = "black")

f1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)

plot(f1)

ggsave("./figs/f1_loss_map.jpg", f1, width = 10.2, height = 13.2, units = c("in"), device = "jpeg")

#-----------------------------------------------

###################################
## Fig. 2 -  Biomass growth plot ##
###################################

dat <- read_xlsx("./data/raw/sigit_data.xlsx") %>%
  dplyr::select(dataset_variable, yr = "regeneration age", agb = mean) %>%
  filter(dataset_variable == "Aboveground biomass carbon stock") %>%
  as.data.frame()

prop_df <- read_csv("./data/processed/eq1_mdlRuns.csv")

fig3_growthMdl <- ggplot(dat, aes(yr, agb)) +
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
  ylab("AGC (Mg / ha)") +
  xlab("Time Since Regeneration (Years)") +
  annotate("text", x = 18, y = 225, label = "t = 14", col = "red") +
  geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
  theme_tufte() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(family = "sans"),
        axis.title.y = element_text(family = "sans"))

fig3_growthMdl

ggsave("./figs/fig2_growth.jpg", fig3_growthMdl, width = 6, height = 4, units = c("in"), device = "jpeg")


#----------------------------------------

###############################################################
## Fig. 3 - Bar charts of total C under different approaches ##
###############################################################

dat2000 <- read_csv("./data/processed/allPrdsSmry.csv") %>%
  mutate(error = error * 1.96)

dat2000$year[1] <- "1960 - 2000"

fig3 <- dat2000 %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed") +
  geom_errorbar(aes(x = year, ymin = carbon + error, ymax = carbon - 1 * (error / abs(error)),
                    col = factor(style, levels = c("Loss", "Gain" , "Net"))),
                width = .1, alpha = 0.8) +
  geom_bar(aes(x = year, y = carbon, fill = factor(style, levels = c("Loss", "Gain", "Net"))),
           stat = "identity", width = 0.6, alpha = 1) +
  scale_fill_manual(values = c("Net" = "#A44294", "Gain" = "#44AA99", "Loss" = "#117733")) +
  scale_color_manual(values = c("Net" = "#A44294", "Gain" = "#44AA99", "Loss" = "#117733")) +
  labs(y = "Total Emissions (Million Mg C)") +
  #ggtitle("A. Total & Net Carbon Emissions") +
  xlab("Time Period & Estimation Approach") +
  #guides(linetype = "none", alpha = "none") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(color = NA))) +
  theme(axis.title.x = element_text(margin=margin(15, 0, 0, 0)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        title = element_text(size = 10),
        strip.text = element_text(" "),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.direction = "vertical")

fig3

ggsave("./figs/fig3_stocks.jpg", fig3, width = 4, height = 6, units = c("in"), device = "jpeg")

#---------------------------------

#################################################################
## Fig. 4 - Uncertainty based on rate of gain/loss assumptions ##
#################################################################

dat2014 <- read_csv("./data/processed/carbonSummary2014.csv")

dat2plt <- dat2014 %>%
  mutate(loss = factor(loss, levels = c("low", "med", "hgh")),
         gain = factor(gain, levels = c("hgh", "med", "low"))) %>%
  group_by(loss) %>%
  mutate(avg_ttl = mean(ttl)) %>%
  ungroup() %>%
  mutate(loss_clr = ifelse(loss == "low", "#44aa99", ifelse(loss == "med", "#117733", "#88ccee"))) %>%
  arrange(loss, ttl)

levels(dat2plt$loss) = list(`Low Rate of C Stock Loss`="low", `Medium Rate of C Stock Loss`="med", `High Rate of C Stock Loss`="hgh")
levels(dat2plt$gain) = list(High="hgh", Medium="med", Low="low")
    
fig4 <- dat2plt %>%
  ggplot() +
  facet_wrap(. ~ loss, ncol = 3) + 
  geom_bar(aes(x = gain, y = ttl, col = loss, fill = loss), alpha = 0.2, width = 0.7, stat = "identity") +
  #geom_point(aes(x = gain, y = ttl, col = loss), fill = NA, size = 1.5, shape = 19, stat = "identity") +
  geom_errorbar(aes(x = gain, ymin = ttl - ttl_se * 1.96, ymax = ttl + ttl_se * 1.96, col = loss), alpha = 0.8, width = .1, position = position_dodge(.9)) +
  #ylim(c(-2.5, 8.5)) +
  ylab("Total Emissions, 2000-2014 (Million Mg C)") +
  xlab("Rate of C Stock Gain") +
  scale_fill_manual(values = c("Low Rate of C Stock Loss" = "#56B4E9", "Medium Rate of C Stock Loss" = "#009E73", "High Rate of C Stock Loss" = "#0072B2")) +
  scale_color_manual(values = c("Low Rate of C Stock Loss" = "#56B4E9", "Medium Rate of C Stock Loss" = "#009E73", "High Rate of C Stock Loss" = "#0072B2")) +
  geom_hline(aes(yintercept = 0), col = "grey", linetype = "solid") +
  geom_hline(aes(yintercept = net_ttl), col = "#999999", linetype = "dashed") +
  #geom_hline(aes(yintercept = net_ttl + net_ttl_se * 1.96), col = "#999999", linetype = "dotted") +
  #geom_hline(aes(yintercept = net_ttl - net_ttl_se * 1.96), col = "#999999", linetype = "dotted") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        title = element_text(size = 10),
        axis.title.x = element_text(margin=margin(15, 0, 0, 0)),
        axis.title.y = element_text(margin=margin(0, 0, 0, 5)))

fig4

ggsave("./figs/fig4_assumptions.jpg", fig4, width = 6, height = 5, units = c("in"), device = "jpeg")

#-------------------------------
# Correlation between NA gains and mangrove extents

dstrcts_c_df <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
  st_set_geometry(NULL)

mg2014_gn <- st_read(paste0(proc_dir, "shapefiles/dstrct_gains_2014")) %>%
  left_join(dstrcts_c_df, by = c("ADM2_EN", "ADM1_EN", "ADM2_ID"))

sum(mg2014_gn$nodata, na.rm = T)

na_cor <- cor(na.omit(mg2014_gn$mangrov), na.omit(mg2014_gn$nodata), method = "pearson")

lm(nodata ~ mangrov, data = mg2014_gn)

fig5 <- mg2014_gn %>%
  st_set_geometry(NULL) %>%
  ggplot(aes(x = mangrov / 1000, y = nodata / 1000)) +
  geom_point(color = "dark grey") +
  theme_tufte() +
  ylab("Extent of new mangrove colonization (kha)") +
  xlab("Extent of non-converted mangrove (kha)") +
  stat_smooth(method = "lm", color = "red", size = 0.5, fill = "grey") +
  annotate("text", x = 12.5, y = 0.1, label = "Pearson Corr. = 0.87", col = "red") +
  theme(axis.title.x = element_text(family = "sans"),
        axis.title.y = element_text(family = "sans"))

fig5

