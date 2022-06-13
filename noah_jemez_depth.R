library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

# read in data from noah
dat <-read_xlsx("/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/VC_MC_2020_depth.xlsx")

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
vg_met_data$vg_snow_depth_cm <-as.numeric(vg_met_data$vg_snow_depth_cm)
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# filter down to same date range
vg_filt <-filter(vg_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

# read in redondo met data
redondo_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/redondo_met_data_v1.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time)
redondo_met_data$date <-ymd(redondo_met_data$date)

# filter down to same date range
redondo_filt <-filter(redondo_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

# bind snow depth and date_time cols to df
insar <-cbind(filt, vg_filt$date_time, vg_filt$vg_snow_depth_cm, redondo_filt$redondo_snow_depth_cm)

# rename new cols
names(insar)[7] <- "date_time"
names(insar)[8] <- "vg_snow_depth_cm"
names(insar)[9] <- "redondo_snow_depth_cm"

# test_plot
ggplot(insar) +
  geom_point(aes(x = date_time, y = redondo_snow_depth_cm))

# plot
ggplot(insar)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_1, col = "1"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_2, col = "2"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_3, col = "3"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_6, col = "4"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = DSDepth_8, col = "5"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = vg_snow_depth_cm, col = "6"), size = .1)+
  geom_point(aes(x = TIMESTAMP, y = redondo_snow_depth_cm, col = "7"), size = .1)+
  scale_y_continuous(limits = c(50,110),breaks = c(seq(50,110,10)))+
  ylab("Depth (cm)") + xlab("Date") +
  scale_color_manual(values = c('1' = 'darkgreen', '2' = 'red', '3' = 'plum', '4' = 'goldenrod', 
                                '5'='blue', '6'='firebrick', '7'='aquamarine'),
                     labels = c('1' = 'DSDepth_1', '2' = 'DSDepth_2', '3' = 'DSDepth_1', 
                                '4' = 'DSDepth_6', '5' = 'DSDepth_8', '6' ='VG', '7'='Redondo'),
                     name = "Sensor")+
  scale_x_datetime(breaks = "2 day", 
                   date_labels="%b %d", 
                   limits = ymd_hm(c("2020-02-10 18:50:000", "2020-02-26 18:50:00")))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave(file = "noah_jemez_depth_v2.png",
       width = 7,
       height = 4,
       dpi = 400)



