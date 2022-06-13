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

# filter and plot for 2/12-2/26
filt <-filter(dat, TIMESTAMP >= "2020-02-10 18:50:00" & TIMESTAMP <= "2020-02-26 18:50:00")

# read in HQ met data
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# filter down to same date range
hq_filt <-filter(vg_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

# bind two data sets
insar <-cbind(filt,hq_filt)
insar$snow_depth_cm <-insar$snow_depth_mm * (1/10)

# test_plot
ggplot(insar) +
  geom_point(aes(x = date_time, y = snow_depth_cm))

# plot
ggplot(insar)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_1, col = "1"), size = .3)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_2, col = "2"), size = .3)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_3, col = "3"), size = .3)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_6, col = "4"), size = .3)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_8, col = "5"), size = .3)+
  geom_point(aes(x = TIMESTAMP, y = snow_depth_cm, col = "6"), size = .3)+
  lims(y = c(50, 110))+
  ylab("Depth (cm)") + xlab("Date") +
  scale_color_manual(values = c('1' = 'darkgreen', '2' = 'red', '3' = 'plum', '4' = 'goldenrod', '5'='blue', '6'='firebrick'),
                     labels = c('1' = 'DSDepth_1', '2' = 'DSDepth_2', '3' = 'DSDepth_1', 
                                '4' = 'DSDepth_6', '5' = 'DSDepth_8', '6' ='HQ Met'),
                     name = "Sensor")+
  scale_x_datetime(breaks = "2 day", 
                   date_labels="%b %d", 
                   limits = ymd_hm(c("2020-02-10 18:50:000", "2020-02-26 18:50:00")))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
# ggsave(file = "noah_jemez_depth_v1.png",
#        width = 7, 
#        height = 4,
#        dpi = 400)

# read back in the proper data!!!
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# filter down to time range between insar pair one
filt_v1 <-filter(vg_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

# test plot pair 1
ggplot(filt_v1) +
  geom_line(aes(x = date_time, y = snow_depth_mm))

ggplot(pair_1) +
  geom_hline(yintercept = 0, linetype=3, col = "red", alpha = .5) +
  geom_vline(xintercept = pair_1$date_time[7], linetype=1, col = "blue", alpha = .5) +
  geom_vline(xintercept = pair_1$date_time[177], linetype=1, col = "blue", alpha = .5) +
  geom_vline(xintercept = pair_1$date_time[345], linetype=1, col = "blue", alpha = .5) +
  geom_line(aes(x = date_time, y = mean_air_temp_c), col = "black") + 
  scale_x_datetime(breaks = "2 day", date_labels="%b %d", limits = ymd_hm(c("2020-02-12 01:00", "2020-03-04 23:00")))+
  scale_y_continuous(breaks = seq(-20,10,5))+
  xlab("Date") + ylab("Air Temperature (C)")

