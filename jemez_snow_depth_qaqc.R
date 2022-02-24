# jack Tarricone
# feb 24 2022
# qc jemez snowdepth data
# with influnece from Dr. Keith's script

library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)
theme_set(theme_classic(12))

# read back in the proper data!!!
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$date_time <-ymd_hms(vg_met_data$date_time)
vg_met_data$date <-as.Date(vg_met_data$date)

# filter down to time range between insar pair one
pair_1 <-filter(vg_met_data, date >= "2020-02-01" & date <="2020-03-17")

# test plot pair 1
ggplot(pair_1) +
  geom_line(aes(x = date_time, y = snow_depth_mm))

###############################################################################################
#4: Run the second QC 
#4a: Remove values outside ±2 standard deviations (± 5 cm per 15 min)
#4b: Think about using more complex routine that identifies spikes
#based on rate of change and length (as in Lehning et al., 2002)
#this might be more accurate
#5 cm seems to be about the max of real snowfall events

del_thresh = 15 # 5mm
pair_1$delta_depth <- c(0, diff(pair_1$snow_depth_mm))

pair_1$depth_lvl_2 <- ifelse(pair_1$delta_depth > del_thresh |
                               pair_1$delta_depth < (-1 * del_thresh) |
                               is.na(pair_1$delta_depth),
                             NA,
                             pair_1$snow_depth_mm)

# test plot of first filter of depth data
ggplot(pair_1) +
  geom_point(aes(x = date_time, y = snow_depth_mm), size = .2)

ggplot(pair_1) +
  geom_point(aes(x = date_time, y = depth_lvl_2), size = .1)

# filter off the bottom NaNs for interpolation funciton
# doesn't work with NaNs at ennd
pair_1 <-filter(pair_1, X <= 1099)

# linearly interpolate between points to fill in NaNs
pair_1 <-data.frame(X = seq(pair_1$X[1], pair_1$X[nrow(pair_1)], by = 1)) %>%
  full_join(pair_1, by = "X") %>%
  mutate(sd_interp = na.approx(pair_1$depth_lvl_2))

# test plot
ggplot(pair_1) +
  # geom_line(aes(x = date_time, y = depth_lvl_2, col = "blue"))+
  geom_line(aes(x = date_time, y = sd_interp), size = .3) 


### filter again on the new data
del_thresh = 10 # 5mm
pair_1$delta_sd_interp <- c(0, diff(pair_1$sd_interp))

pair_1$depth_lvl_3 <- ifelse(pair_1$delta_sd_interp > del_thresh |
                               pair_1$delta_sd_interp < (-1 * del_thresh) |
                               is.na(pair_1$delta_sd_interp),
                               NA,
                               pair_1$sd_interp)

# linearly interpolate between interpolated data, lol
pair_1 <-data.frame(X = seq(pair_1$X[1], pair_1$X[nrow(pair_1)], by = 1)) %>%
  full_join(pair_1, by = "X") %>%
  mutate(sd_interp_v2 = na.approx(depth_lvl_3))

# test plot with new data
ggplot(pair_1) +
  # geom_line(aes(x = date_time, y = snow_depth_mm))+
  # geom_point(aes(x = date_time, y = sd_interp, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v2, col = "blue"), size = .3)

write.csv(pair_1, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/snow_depth_qaqcv1.csv" )

