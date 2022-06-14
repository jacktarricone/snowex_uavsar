# jack tarricone
# june 14, 2022

# formatting hourly hidden valley met station data from the WRCC website
# comes in a dumb hmtl table with no download option so have to copy each date by hand
# function for converting into proper df for plotting

# slightly different than last two bc has soil moisture data

library(dplyr)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

# read in
list <-list.files("/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/raw_data",full.names = TRUE)
head(list)

#file_path <-list[2]

# function for formatting csvs
format_csv <-function(file_path){

    # read csv
    csv <-read.csv(file_path)

    # pull out date from basename
    name_raw <-basename(file_path)
    file_name <-gsub(".csv","",name_raw)
    date_str <-gsub("hv_","",file_name)

    # delete blank cols,
    head(csv)
    csv <-csv[, c("Hour","Total","X.1","X.2","Air",
                    "Soil","X.4","Relative","X.6","X.7",
                    "X.8", "X.10","Snow","X.11","X.13",
                    "X.14","X.16","X.18","X.19")]

    # delete first three rows
    csv <-csv[-c(1:3),]

    # define col names
    names <-c("hour","solar_rad", # time and rad
              "avg_wind_ms","wind_dir_deg","max_wind_ms", # wind
              "mean_air_temp_c","max_air_temp_c","min_air_temp_c", # air temp
              "mean_soil_temp_c","max_soil_temp_c","min_soil_temp_c", # soil temp     
              "mean_rh_percent","max_rh_percent","min_rh_percent", # rh
              "dew_point_c","wet_bulb_c","pressure_mb","snow_depth_mm","precip_mm") # other

    # assign
    colnames(csv) <-names

    # add date col
    single_date <-as.Date(date_str)
    date <-rep(single_date,nrow(csv))
    csv <-cbind(date,csv)
    
    #add depth cm
    csv$snow_depth_mm <-as.numeric(csv$snow_depth_mm)
    csv$hv_snow_depth_cm <-csv$snow_depth_mm*(1/10)# need to check this

    # format hours col
    hours <-as.character(seq(1,24,1))
    hours <-paste0(hours,":00:00")
    
    # add date_time col
    date_time <- ymd_hms(paste(csv$date, hours))
    csv <-cbind(date_time,csv)
    
    # save
    setwd("/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/formatted/")
    write.csv(csv, paste0(file_name,".csv"), row.names = FALSE)
}

# applot to list of csvs
lapply(list, format_csv)


# bind rows for one big df
hv_met_data <-list.files(path="/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/formatted/", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows

# convert date_time and date
hv_met_data$date_time <-ymd_hms(hv_met_data$date_time)
hv_met_data$date <-as.Date(hv_met_data$date)

head(hv_met_data)

# save rough csv
write.csv(hv_met_data, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/hv_met_data_v1.csv", row.names =  FALSE)


