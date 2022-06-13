library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(plotly);theme_set(theme_classic(12))

# read in data from noah
dat <-read_xlsx("/Users/jacktarricone/ch1_jemez_data/climate_station_data/VC_MC_2020_depth.xlsx")

# change to numeric
dat[, 2:6] <- sapply(dat[, 2:6], as.numeric)

# format date_time
dat$TIMESTAMP <-ymd_hms(dat$TIMESTAMP) #format data

# quick test plot
ggplot(dat)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_8))+
  lims(y = c(0, 200))

