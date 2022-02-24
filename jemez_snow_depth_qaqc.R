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

# write.csv(pair_1, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/snow_depth_qaqcv1.csv" )


#### third filter!

### filter again on the new data
del_thresh = 5 # 5mm
pair_1$delta_sd_interp_v2 <- c(0, diff(pair_1$sd_interp_v2))

pair_1$depth_lvl_4 <- ifelse(pair_1$delta_sd_interp_v2 > del_thresh |
                               pair_1$delta_sd_interp_v2 < (-1 * del_thresh) |
                               is.na(pair_1$delta_sd_interp_v2),
                               NA,
                               pair_1$sd_interp_v2)

# linearly interpolate between interpolated data, lol
pair_1 <-filter(pair_1, X <= 1066)
pair_1 <-data.frame(X = seq(pair_1$X[1], pair_1$X[nrow(pair_1)], by = 1)) %>%
  full_join(pair_1, by = "X") %>%
  mutate(sd_interp_v3 = na.approx(depth_lvl_4))

# test plot with new data
ggplot(pair_1) +
  # geom_line(aes(x = date_time, y = snow_depth_mm))+
  geom_point(aes(x = date_time, y = sd_interp_v2, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3, col = "blue"), size = .3)


######################
# compute rolling mean
######################

pair_1$sd_interp_v3_smooth3 <- rollmeanr(pair_1$sd_interp_v3, 3, fill = NA)
pair_1$sd_interp_v3_smooth10 <- rollmeanr(pair_1$sd_interp_v3, 10, fill = NA)
pair_1$sd_interp_v3_smooth20 <- rollmeanr(pair_1$sd_interp_v3, 20, fill = NA)

# test plot look good
ggplot(pair_1) +
  geom_point(aes(x = date_time, y = sd_interp_v2, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3_smooth20 ))

# write.csv(pair_1, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/snow_depth_qaqc_v2.csv" )


#### plot for ryan
insar <-filter(pair_1, date >= "2020-02-10" & date <="2020-02-26")
flight1 <-as.numeric(insar$date_time[58]) # row for correct date time
flight2 <-as.numeric(insar$date_time[226])
flight3 <-as.numeric(insar$date_time[394])

lims <- as.POSIXct(strptime(c("2020-02-10 00:00:00", "2020-02-27 00:00:00"), 
                            format = "%Y-%m-%d %H:%M:%S"))

ggplot(insar) +
  # geom_point(aes(x = date_time, y = depth_lvl_3, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3_smooth20))+
  geom_vline(xintercept = flight1, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=3, col = "red", alpha = .7) +
  annotate("text", label = "Flight 1 (2/12)", x = insar$date_time[55], y = 700, col = "red") +
  annotate("text", label = "Flight 2 (2/19)", x = insar$date_time[222], y = 700, col = "red") +
  annotate("text", label = "Flight 3 (2/19)", x = insar$date_time[390], y = 700, col = "red") +
  scale_x_datetime(limits = lims, 
                   breaks = "3 day") +
  labs(title = "HQ Met Snow Depth 2/10/20 - 2/27/20",
       y = "Snow Depth [mm]",
       x = "Date")


              