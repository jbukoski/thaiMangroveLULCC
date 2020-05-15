# 04_statistics.R
# A R script to recreate statistics and tables in the manuscript.
# Needs work! 

#--------------

proc_dir <- "./data/processed/"

#--------------------

library(tidyverse)

#--------------------
# Examine table of top losses by district

dat2014 <- st_read(paste0(proc_dir, "shapefiles/dstrct_ttls_2014/"))

dat2014_df <- dat2014
st_geometry(dat2014_df) <- NULL

dat <- dat2014_df %>%
  mutate(mangrov = ifelse(is.na(mangrov), 0, mangrov),
         ttl_loss = total - mangrov,
         perc_loss = 100 * (total - mangrov) / total) %>%
  arrange(-ttl_loss) %>%
  mutate(ttl_sum = cumsum(ttl_loss),
         percent = ttl_sum / 194223.6)

sum(dat[1:20, ]$ttl_loss) / 194223.6

sum(dat[0:49, ]$ttl_loss) / 194223.6

plot(dat$ttl_sum)

