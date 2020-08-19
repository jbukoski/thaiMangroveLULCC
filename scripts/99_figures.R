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

ggsave("./figs/f1_loss_map.jpg", f1, width = 10.2, height = 13.2, units = c("in"), device = "jpeg")

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

ggsave("./figs/fig2_growth.jpg", fig3_growthMdl, width = 6, height = 4, units = c("in"), device = "jpeg")


#----------------------------------------
# Build bar charts of total carbon emissions under the different assumptions

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
  ylim(c(-2.5, 7.5)) +
  ylab("Total Emissions, 2000-2014 (Million Mg C)") +
  xlab("Rate of C Stock Gain") +
  scale_fill_manual(values = c("Low Rate of C Stock Loss" = "#56B4E9", "Medium Rate of C Stock Loss" = "#009E73", "High Rate of C Stock Loss" = "#0072B2")) +
  scale_color_manual(values = c("Low Rate of C Stock Loss" = "#56B4E9", "Medium Rate of C Stock Loss" = "#009E73", "High Rate of C Stock Loss" = "#0072B2")) +
  #geom_hline(aes(yintercept = 0), col = "grey", linetype = "longdash") +
  geom_hline(aes(yintercept = net_ttl), col = "#999999", linetype = "dashed") +
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
