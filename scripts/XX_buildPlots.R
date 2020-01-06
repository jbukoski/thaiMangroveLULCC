## Build plots
#
# 
#

library(tidyverse)
library(plotrix)
library(ggthemes)

fig_dir <- "./figs/"

#-----------------------------------------------------------------
# Soil parameters by depth

soil_params <- read_csv("./data/processed/th_soil_params.csv")

depth_ints <- data.frame(interval = c(1, 2, 3, 4, 5), 
                         depth = c(-7.5, -22.5, -40, -75, -150))

soil2plot <- soil_params %>%
  dplyr::select(site, type, plot, subplot, interval, int_a, int_b, avg_depth, bd, poc, c_dens, int_volume, soc_per_ha) %>%
  gather(-site, -type, -plot, -subplot, -interval, -int_a, -int_b, -avg_depth, -int_volume, key = var, value = value) %>%
  group_by(site, type, interval, var) %>%
  summarize(avg_val = mean(value),
            se_val = std.error(value)) %>%
  arrange(site, type, var, interval) %>%
  left_join(depth_ints, by = "interval")# multiply interval by -1 to create y-axis variable


soil_labs = c("Bulk Density (g cm-3)", "Carbon Density (g cm-3)", "Organic Carbon (%)")
names(soil_labs) = c("bd", "c_dens", "poc")

fig2 <- soil2plot %>%
  filter(!(var %in% c("soc_per_ha"))) %>%
  group_by(site, type, var) %>%
  do(augment(
    loess(avg_val ~ depth, data = .),
    newdata = data.frame(depth = seq(min(.$depth), max(.$depth), l = 286))
  )) %>%
  left_join(select(soil2plot, site, type, var, depth, se_val), 
            by = c("site", "type", "var", "depth")) %>%
  ggplot(aes(y = depth, x = .fitted)) +
  facet_grid(~ var, scales = "free_x", 
             labeller = labeller(var = soil_labs)) + 
  geom_path(aes(x = .fitted, col = site, lty = type)) +
  geom_errorbarh(aes(xmin = .fitted - se_val, xmax = .fitted + se_val, y = depth,
                       col = site, lty = type, height = 2)) +
  scale_linetype_discrete(name = "Type", labels = c("Aquaculture", "Mangrove")) +
  scale_color_discrete(name = "Site", labels = c("Krabi River Estuary", "Pak Panang Mangrove", "Palian River Estuary")) +
  theme_bw() +
  ylab("Depth (cm)") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0.1, "cm"))

ggplot2::ggsave(fig2, device = "jpeg", 
                filename = paste0(fig_dir, "draft_fig2_depth_plots.jpg"), 
                width = 8.5, height = 5.5, units = "in")

  #-------------------------------------------------------
# Carbon data

c_dat <- read_csv("./data/processed/th_carbon.csv")

cDat2plot <- c_dat %>%
  dplyr::select(site, type, plot, subplot, agc_ha, bgc_ha, cwd_carb, soc) %>%
  gather(-site, -type, -plot, -subplot, key = var, value = value) %>%
  group_by(site, type, var) %>%
  summarize(avg_val  = mean(value),
            se_val = std.error(value)) %>%
  mutate(avg_val = ifelse(var %in% c("soc", "bgc_ha"), avg_val * -1, avg_val))

ggplot(cDat2plot) +
  geom_bar(aes(x = type, y = avg_val, col = var, fill = var), stat = "identity") +
  facet_grid(. ~ site) +
  theme_bw()

head(c_dat)



