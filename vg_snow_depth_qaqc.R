# jack Tarricone
# feb 24 2022
# qc jemez snowdepth data
# with influence from Dr. Keith's script

library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

# read back in the proper data!!!
depth_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/wrcc_depth_formatted.csv")
depth_data$date_time <-mdy_hm(depth_data$date_time)
depth_data$date <-as.Date(depth_data$date)


###############################################################################################
#4: Run the second QC 
#4a: Remove values outside ±2 standard deviations (± 5 cm per 15 min)
#4b: Think about using more complex routine that identifies spikes
#based on rate of change and length (as in Lehning et al., 2002)
#this might be more accurate
#5 cm seems to be about the max of real snowfall events

  
del_thresh = 15 # 5mm
depth_data$delta_depth <- c(0, diff(depth_data$vg_snow_depth_cm))

depth_data$depth_lvl_2 <- ifelse(depth_data$delta_depth > del_thresh |
                               depth_data$delta_depth < (-1 * del_thresh) |
                               is.na(depth_data$delta_depth),
                             NA,
                             depth_data$vg_snow_depth_cm)

# test plot of first filter of depth data
ggplot(depth_data) +
  geom_point(aes(x = date_time, y = vg_snow_depth_cm), size = .2)

ggplot(depth_data) +
  geom_point(aes(x = date_time, y = depth_lvl_2), size = .1)

# filter off the bottom NaNs for interpolation funciton
# doesn't work with NaNs at ennd
depth_data <-filter(depth_data, X <= 1099)

# linearly interpolate between points to fill in NaNs
depth_data <-data.frame(X = seq(depth_data$X[1], depth_data$X[nrow(depth_data)], by = 1)) %>%
  full_join(depth_data, by = "X") %>%
  mutate(sd_interp = na.approx(depth_data$depth_lvl_2))

# test plot
ggplot(depth_data) +
  # geom_line(aes(x = date_time, y = depth_lvl_2, col = "blue"))+
  geom_line(aes(x = date_time, y = sd_interp), size = .3) 


### filter again on the new data
del_thresh = 10 # 5mm
depth_data$delta_sd_interp <- c(0, diff(depth_data$sd_interp))

depth_data$depth_lvl_3 <- ifelse(depth_data$delta_sd_interp > del_thresh |
                               depth_data$delta_sd_interp < (-1 * del_thresh) |
                               is.na(depth_data$delta_sd_interp),
                               NA,
                               depth_data$sd_interp)

# linearly interpolate between interpolated data, lol
depth_data <-data.frame(X = seq(depth_data$X[1], depth_data$X[nrow(depth_data)], by = 1)) %>%
  full_join(depth_data, by = "X") %>%
  mutate(sd_interp_v2 = na.approx(depth_lvl_3))

# test plot with new data
ggplot(depth_data) +
  # geom_line(aes(x = date_time, y = vg_snow_depth_cm))+
  # geom_point(aes(x = date_time, y = sd_interp, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v2, col = "blue"), size = .3)

# write.csv(depth_data, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/snow_depth_qaqcv1.csv" )


#### third filter!

### filter again on the new data
del_thresh = 5 # 5mm
depth_data$delta_sd_interp_v2 <- c(0, diff(depth_data$sd_interp_v2))

depth_data$depth_lvl_4 <- ifelse(depth_data$delta_sd_interp_v2 > del_thresh |
                               depth_data$delta_sd_interp_v2 < (-1 * del_thresh) |
                               is.na(depth_data$delta_sd_interp_v2),
                               NA,
                               depth_data$sd_interp_v2)

# linearly interpolate between interpolated data, lol
depth_data <-filter(depth_data, X <= 1066)
depth_data <-data.frame(X = seq(depth_data$X[1], depth_data$X[nrow(depth_data)], by = 1)) %>%
  full_join(depth_data, by = "X") %>%
  mutate(sd_interp_v3 = na.approx(depth_lvl_4))

# test plot with new data
ggplot(depth_data) +
  # geom_line(aes(x = date_time, y = vg_snow_depth_cm))+
  geom_point(aes(x = date_time, y = sd_interp_v2, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3, col = "blue"), size = .3)


######################
# compute rolling mean
######################

depth_data$sd_interp_v3_smooth3 <- rollmeanr(depth_data$sd_interp_v3, 3, fill = NA)
depth_data$sd_interp_v3_smooth10 <- rollmeanr(depth_data$sd_interp_v3, 10, fill = NA)
depth_data$sd_interp_v3_smooth20 <- rollmeanr(depth_data$sd_interp_v3, 30, fill = NA)



# test plot look good
ggplot(depth_data) +
  geom_point(aes(x = date_time, y = sd_interp_v2, col = "red"), size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3_smooth20 ))

write.csv(depth_data, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_snow_depth_qaqc_v2.csv" )


#### plot for ryan
insar <-filter(depth_data, date >= "2020-02-01" & date <="2020-02-26")
flight1 <-as.numeric(insar$date_time[58]) # row for correct date time
flight2 <-as.numeric(insar$date_time[226])
flight3 <-as.numeric(insar$date_time[394])

lims <- as.POSIXct(strptime(c("2020-02-01 00:00:00", "2020-02-27 00:00:00"), 
                            format = "%Y-%m-%d %H:%M:%S"))

ggplot(insar) +
  geom_point(aes(x = date_time, y = depth_lvl_3), col = "green", size = .3) +
  geom_line(aes(x = date_time, y = sd_interp_v3_smooth10))+
  geom_vline(xintercept = flight1, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=3, col = "red", alpha = .7) +
  annotate("text", label = "Flight 1 (2/12)", x = insar$date_time[270], y = 700, col = "red") +
  annotate("text", label = "Flight 2 (2/19)", x = insar$date_time[437], y = 700, col = "red") +
  annotate("text", label = "Flight 3 (2/19)", x = insar$date_time[605], y = 700, col = "red") +
  scale_x_datetime(limits = lims, 
                   breaks = "1 week") +
  labs(title = "HQ Met Snow Depth 2/1/20 - 2/27/20",
       y = "Snow Depth [mm]",
       x = "Date")

setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave("hq_met_sd_v2.png",
       width = 8, 
       height = 5,
       units = "in",
       dpi = 300)

              