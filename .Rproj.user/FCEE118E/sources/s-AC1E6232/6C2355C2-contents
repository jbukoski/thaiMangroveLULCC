library(tidyverse)
library(spsurvey)


dat1996 <- read.dbf("~/Documents/GMW_001_GlobalMangroveWatch/processed/GMW_1996_thai.shp")
dat2016 <- read.dbf("~/Documents/GMW_001_GlobalMangroveWatch/processed/GMW_2016_thai.shp")

head(dat1996)
head(dat2016)

dat1996 %>% 
  summarize(Area = sum(Area) / 10000)

dat2016 %>% 
  summarize(Area = sum(Area) / 10000)
