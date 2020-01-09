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

#-----------------------------------

soil_params %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_boxplot(aes(x = type, y = poc))

classes <- c_dat %>% 
  group_by(site, type, plot) %>% 
  summarize(agb = mean(agb_ha)) %>% 
  mutate(type_2 = ifelse(type == "aquaculture" & agb > 0, "aqua_regen", type)) %>% 
  ungroup %>% 
  dplyr::select(site, type, type_2, plot) %>%
  arrange(site, type_2, plot)

poc_plot <- soil_params %>%
  left_join(classes, by = c("site", "type", "plot")) %>%
  ggplot() +
  facet_wrap(. ~ site) +
  geom_boxplot(aes(x = as.factor(interval), y = poc, fill = type)) +
  theme_bw() +
  ylab("Organic Carbon (%)") +
  xlab("Depth interval") +
  theme(legend.position = "none")

bd_plot <- soil_params %>%
  left_join(classes, by = c("site", "type", "plot")) %>%
  ggplot() +
  facet_wrap(. ~ site) +
  geom_boxplot(aes(x = as.factor(interval), y = bd, fill = type)) +
  theme_bw() +
  ylab("Bulk Density (g cm-3)") +
  xlab("Depth interval") +
  theme(legend.position = "bottom")

fig3 <- ggarrange(poc_plot, bd_plot, nrow = 2)

ggplot2::ggsave(fig3, device = "jpeg", 
                filename = paste0(fig_dir, "draft_fig3_soil_luc.jpg"), 
                width = 8.5, height = 8, units = "in")

#-------------------------
# Scrap

soil_params %>%
  #filter(site == "Trang") %>%
  #mutate(poc = ifelse(site == "Trang" & type == "aquaculture", poc / 2.06, poc)) %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_point(aes(x = bd, y = poc, col = type)) +
  ylab("percent organic carbon") +
  xlab("bulk density") +
  theme_bw() +
  theme(legend.position = "bottom")


soil_params %>%
  filter(site == "Trang") %>%
  #mutate(poc = ifelse(site == "Trang" & type == "aquaculture", poc / 2.06, poc)) %>%
  ggplot() +
  #facet_grid(. ~ site) +
  geom_point(aes(x = as.factor(plot), y = c_dens, col = type)) +
  theme_bw() +
  theme(legend.position = "bottom")

site_level_dat <- soil_params %>%
  #filter(site == "Trang") %>%
  filter(!plot %in% c("T6", "12s", "8")) %>%
  group_by(site, type) %>%
  summarize(poc_avg = mean(poc),
            poc_avg_se = std.error(poc),
            c_dens_avg = mean(c_dens),
            c_dens_avg_se = std.error(c_dens),
            max_poc = max(poc),
            min_poc = min(poc),
            med_poc = median(poc),
            bd_avg = mean(bd),
            bd_avg_se = std.error(bd), 
            poc_lwr = poc_avg - poc_avg_se,
            poc_upr = poc_avg + poc_avg_se,
            c_dens_lwr = c_dens_avg - c_dens_avg_se,
            c_dens_upr = c_dens_avg + c_dens_avg_se,
            bd_lwr = bd_avg + bd_avg_se,
            bd_upr = bd_avg - bd_avg_se)

bd <- site_level_dat %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_bar(aes(x = as.factor(type), y = bd_avg), stat = "identity") +
  geom_errorbar(aes(x = as.factor(type), ymax = bd_upr, ymin = bd_lwr), width = 0.2) +
  theme_bw()

poc <- site_level_dat %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_bar(aes(x = as.factor(type), y = poc_avg), stat = "identity") +
  geom_errorbar(aes(x = as.factor(type), ymax = poc_upr, ymin = poc_lwr), width = 0.2) +
  theme_bw()

c_dens <- site_level_dat %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_bar(aes(x = as.factor(type), y = c_dens_avg), stat = "identity") +
  geom_errorbar(aes(x = as.factor(type), ymax = c_dens_upr, ymin = c_dens_lwr), width = 0.2) +
  theme_bw()

ggarrange(bd, poc, c_dens, nrow = 3)

set.seed(1)

soil_params %>%
  filter(!(site == "Trang" & plot %in% c("12s", "8", "T6"))) %>%
  ggplot() +
  facet_grid(. ~ site) +
  geom_jitter(aes(x = as.factor(type), y = poc, col = as.factor(interval)), 
              width = 0.25, alpha = 0.5) +
  #geom_boxplot(aes(x = as.factor(type), y = poc), width = 0.25) +
  theme_tufte()

  #-----------------------------------
# Build dataset with poc & bd quantiles for visualizing

meta <- read_csv("./data/processed/th_meta.csv")

dat <- meta %>%
  left_join(soil_params, by = c("site", "type", "plot", "subplot")) %>%
  filter(site == "Trang", interval == 1) %>%
  mutate(poc_qs = quantcut(poc, q = 5),
         bd_qs = quantcut(bd, q = 5)) %>%
  arrange(poc_qs)

write_csv(dat, "~/Desktop/test.csv")
