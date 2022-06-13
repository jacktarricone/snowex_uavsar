# jack tarricone
# june 23, 2022

# formatting hourly redonde met station data from the WRCC website
# comes in a dumb hmtl table with no download option so have to copy each date by hand
# function for converting into proper df for plotting

library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_classic(12))

# read in
list <-list.files("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/raw_data/",full.names = TRUE)

# function for formatting csvs
format_csv <-function(file_path){

    # read csv
    csv <-read.csv(file_path)

    # pull out date from basename
    name_raw <-basename(file_path)
    file_name <-gsub(".csv","",name_raw)
    date_str <-gsub("redondo_","",file_name)

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
    single_date <-as.Date(date_str)
    date <-rep(single_date,nrow(csv))
    csv <-cbind(date,csv)
    
    #add depth cm
    csv$snow_depth_mm <-as.numeric(csv$snow_depth_mm)
    csv$snow_depth_cm <-csv$snow_depth_mm*(1/10)

    # format hours col
    hours <-as.character(seq(1,24,1))
    hours <-paste0(hours,":00:00")
    
    # add date_time col
    date_time <- ymd_hms(paste(csv$date, hours))
    csv <-cbind(date_time,csv)
    
    # save
    setwd("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/formatted/")
    write.csv(csv, paste0(name_raw), row.names = FALSE)
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
write.csv(vg_met_data, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")


