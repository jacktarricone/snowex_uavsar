# jack tarricone
# feb 23, 2022

# formatting Vhourly Valle Grande met station data from the WRCC website
# comes in a dumb hmtl table with no download option so have to copy each date by hand
# function for converting into proper df for plotting

library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_classic(12))

# read in
list <-list.files("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/raw_data/",full.names = TRUE)
# file_path <-list[20]

# function for formatting csvs
format_csv <-function(file_path){

    # read csv
    csv <-read.csv(file_path)

    # pull out date from basename
    date_raw <-basename(file_path)
    date_str <-gsub(".csv","",date_raw)

    # delete blank cols
    csv <- csv[ -c(3,7,9:13,15,17,20,22) ]

    # delete first three rows
    csv <-csv[-c(1:3),]

    # define col names
    names <-c("hour","solar_rad","avg_wind_ms","wind_dir_deg","max_wind_ms","mean_air_temp_c","mean_soil_temp_c", 
              "rh_percent","dew_point_c","wet_bulb_c","pressure_mb","snow_depth_mm","precip_mm")

    # assign
    colnames(csv) <-names

    # add date col
    date <-as.Date(rep(date_str,nrow(csv)))
    csv <-cbind(date,csv)
    
    # add date_time col
    date_time <- ymd_hm(paste(csv$date, csv$hour))
    csv <-cbind(date_time,csv)
    
    setwd("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/formatted/")
    write.csv(csv, paste0(date_raw), row.names = FALSE)
}

# applot to list of csvs
lapply(list, format_csv)


# bind rows for one big df
vg_met_data <-list.files(path="/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/formatted/", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows

# clear unwatned cols
vg_met_data <-vg_met_data[ -c(16:20) ]

# convert date_time and date
vg_met_data$date_time <-ymd_hms(vg_met_data$date_time)
vg_met_data$date <-as.Date(vg_met_data$date)

# save rough csv
# write.csv(vg_met_data, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v1.csv")

# read back in
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v1.csv")
vg_met_data$date_time <-ymd_hms(vg_met_data$date_time)
vg_met_data$date <-as.Date(vg_met_data$date)

# filter down to time range between insar pair one
pair_1 <-filter(vg_met_data, date >= "2020-02-12" & date <="2020-02-19")
pair_1$snow_depth_mm[pair_1$snow_depth_mm < 550] <- NA 

# test plot
ggplot(vg_met_data) +
  geom_line(aes(x = date_time, y = precip_mm))

# test plot pair 1
ggplot(pair_1) +
  geom_line(aes(x = date_time, y = snow_depth_mm))



