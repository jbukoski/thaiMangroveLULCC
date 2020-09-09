#--------------------------------
# Figure 2. District level C Stock estimates.

gadm_sea <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp"))

tha <- st_read(paste0(raw_dir, "shapefiles/gadm_se_asia.shp")) %>%
  filter(NAME_0 == "Thailand")

mg2000_df <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2000"))
st_geometry(mg2000_df) <- NULL

mg2014_df <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2000"))
st_geometry(mg2014_df) <- NULL

dstrct_c <- st_read(paste0(proc_dir, "shapefiles/dstrcts_c/")) %>%
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



#-----------------------------
# Old for loop for adding uncertainty to raster data

for(i in 1:nrow(dstrcts_sp)) {
  
  shp <- dstrcts_sp[i, ]
  agb_crop <- crop(agb, shp)
  agb_dat <- raster::extract(agb_crop, shp, df = T)
  
  means <- c()
  
  for(j in 1:100) {
    
    agb_mean <- mean(agb_dat$Mangrove_agb_Thailand, na.rm = T)
    agb_sd <- sqrt(sd(agb_dat$Mangrove_agb_Thailand, na.rm = T)^2 + agb_rmse^2)
    
    if(!is.na(agb_mean)) {
      
      sim_agb <- rnorm(100, mean = mean(agb_dat$Mangrove_agb_Thailand, na.rm = T), sd = agb_sd)
      sim_agb <- sim_agb[sim_agb > 0]
      gammaParams <- egamma(sim_agb)
      means[j] <- mean(stats::rgamma(100, shape = gammaParams$parameters[1], scale = gammaParams$parameters[2]))  
      
    } else {
      
      means <- NA
      
    }
  
  }
  
  dstrct_avgs$AGB_AVG[i] <- mean(means) * 0.47
  dstrct_avgs$AGB_SE[i] <- plotrix::std.error(means)
  
  rm(agb_crop, agb_dat)
  gc()
  
}

# Old biomass growth plot

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