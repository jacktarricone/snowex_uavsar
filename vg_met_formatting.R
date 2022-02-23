# jack tarricone
# feb 23, 2022

# formatting Vhourly Valle Grande met station data from the WRCC website
# comes in a dumb hmtl table with no download option so have to copy each date by hand
# function for converting into proper df for plotting


library(dplyr)

setwd("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/raw_data/")

# read in
list <-list.files(full.names = TRUE)
file_path <-"/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/raw_data/2020-02-20.csv"
csv <-read.csv(file_path)

# pull out date from basename
date_raw <-basename(file_path)
date_str <-gsub(".csv","",date_raw)

# delete blank cols
csv_v1 <- csv[ -c(3,7,9:13,15,17,20,22) ]

# delete first four ros
csv_v2 <-csv_v1[-c(1:3),]

# define col names
names <-c("hour","solar_rad","avg_wind_ms","wind_dir_deg","max_wind_ms","mean_air_temp_c","mean_soil_temp_c", 
          "rh_percent","dew_point_c","wet_bulb_c","pressure_mb","snow_depth_mm","precip_mm")

# assign
colnames(csv_v2) <-names
head(csv_v2) #check

# add date col
date <-as.Date(rep(date_str,nrow(csv_v2)))
csv_v3 <-cbind(date,csv_v2)
tail(csv_v3)

