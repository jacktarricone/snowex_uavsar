# figures for snowex poster

library(data.table)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
library(plyr)
library(tidyverse)

# read in data
lwc_den <-read.csv("/Volumes/jt/UNR_fall_20/snowex/data_analysis/pits/lwc/lwc_density_r.csv")
swe <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/snowex_swe_r.csv")
depth <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/snowex_depth_r.csv")
pit_avgs <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/pits/pit_avgs_old.csv") 
all_pits_good <-read.csv("/Volumes/JT/projects/uavsar/jemez/pit_data/jemez_pits_density_r.csv") 

# convert to date data type
all_pits_good$date <- as.Date(all_pits_good$date)

#chop off 2020-
display_date <- gsub("2020-", "", all_pits_good$date)

#add new column
all_pits_good<-cbind(all_pits_good,display_date)

# take off zeros for graph
all_pits_good$display_date <- gsub("01-", "1/", all_pits_good$display_date)
all_pits_good$display_date <- gsub("02-", "2/", all_pits_good$display_date)
all_pits_good$display_date <- gsub("03-", "3/", all_pits_good$display_date)

###############################
#### set theme
###############################

theme_set(theme_light(12))


# save new data set
#write.csv(all_pits_good, "/Volumes/JTARRICONE/UNR_fall_20/snowex/data/pits/all_pits_good.csv") 

## 9/01/2020

# snow pit plots

p1 <-ggplot(all_pits_good, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = density, group = location), stat = "identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"}))) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(#title="Jemez River SnowEx 2020 Snow Pit Time Series", 
       y = "Depth (cm)", x = "Date") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

print(p1)

setwd("/Volumes/JT/projects/uavsar/jemez/scatter_plots/")
ggsave(p1,
       file = "jemez_pit_density2.png",
       width = 7, 
       height = 4,
       dpi = 400)

################################################

# met data
# april 5th 2020

jemez_met <-fread("/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_daily_3site.csv")
jemez_met$date <-as.Date(jemez_met$date)
jemez_met2 <-filter(jemez_met, date <= "2020-02-27")

plt <-ggplot(jemez_met2) +
  geom_vline(xintercept = as.Date("2020-02-12"), 
             linetype="solid", color = "grey", size=1) +
  geom_vline(xintercept = as.Date("2020-02-19"), 
             linetype="solid", color = "grey", size=1) +
  geom_vline(xintercept = as.Date("2020-02-26"), 
             linetype="solid", color = "grey", size=1) +
  scale_x_date(date_breaks = "2 day", 
               limits = as.Date(c('2/10/2020', '2/27/2020'), format="%m/%d/%Y"),
               labels = function(z) gsub("^0", "", strftime(z, "%m/%d"))) + #takes out leading 0
  scale_y_continuous(breaks = seq(-20,60,10))+
  geom_line(aes(x = date, y = t_mean_f, linetype = site, color = "Mean Temp")) +
  geom_line(aes(x = date, y = t_min_f, linetype = site, color = "Min Temp")) +
  geom_line(aes(x = date, y = t_max_f, linetype = site, color = "Max Temp")) +
  scale_colour_manual(values=c('Mean Temp' = "black", 'Min Temp' = "blue", 'Max Temp' = "red")) +
  scale_linetype_manual("Location",values=c("Hidden Valley" = 1, "Redondo" = 3, "Valle Grande" = 2)) +
  labs(#title="Jemez River Daily Temperature (F) 2/10/2020 - 3/5/2020", 
       y = "Temperature (F)", x = "Date", color = "Variable") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .1))
 
print(plt)       

ggsave(plt,
       file = "jemez_daily_temp3.png",
       width = 7, 
       height = 3,
       dpi = 400)
        #legend.direction = "horizontal", 
        #legend.position = c(.50,.93),
        #legend.spacing.y = unit(.00001, 'mm'),
        #legend.margin = margin(t=0, unit='cm'),
        #legend.key = element_rect(size = .1))
        #legend.key.size = unit(1, 'lines'),
        #legend.title = element_blank())

#setwd("/Volumes/JT/projects/uavsar/jemez/scatter_plots/")

ggsave(plt,
       file = "jemez_daily_temp3.png",
       width = 7, 
       height = 3,
       dpi = 400)

####################################
##### hourly solar radiation plot

jemez_hourly <-fread("/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly.csv")
jemez_hourly$date <-as.Date(jemez_hourly$date) 
jemez_hourly$date_time <-as.POSIXct(paste(jemez_hourly$date, jemez_hourly$time), format="%Y-%m-%d %H")
#fwrite(jemez_hourly, "/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly.csv")
#jemez_hourly <-fread("/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly.csv")

sw<-ggplot(jemez_hourly) +
  geom_line(aes(x = date_time, y = watts_meter_sq, group = site, color = site, linetype = site))+
  scale_color_manual(values=c("grey50", "firebrick", "goldenrod"))+
  scale_linetype_manual("Station Name",values=c("Hidden Valley" = 4, "Redondo" = 3, "Valle Grande" = 2))+
  scale_x_datetime(date_breaks = "2 day",
                   labels = function(z) gsub("^0", "", strftime(z, "%m/%d")))+
  labs(#title = "Jemez Incoming Solar Radiation 2/10/2020 - 2/27/2020", 
       y =(expression(SW["in"]~(W/m^{"2"}))), x="Date",
       color  = "Station Name", linetype = "Station Name") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.direction = "horizontal", 
        legend.position = c(.60,.97),
        legend.key = element_rect(size = 40),
        legend.key.size = unit(1, 'lines'),
        legend.title = element_blank())
print(sw)

setwd("/Volumes/JT/projects/uavsar/jemez/scatter_plots/")
ggsave(sw,
       file = "jemez_solar_rad2.png",
       width = 7, 
       height = 3,
       dpi = 400)

  
####################################
##### hourly snow depth plot

#jemez_hourly <-fread("/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly.csv")
#jemez_hourly$date <-as.Date(jemez_hourly$date)
#jemez_hourly$date <- fifelse(jemez_hourly$time == "0:00:00", (jemez_hourly$date)+1 ,jemez_hourly$date)
#jemez_hourly$date_time <-as.POSIXct(paste(jemez_hourly$date, jemez_hourly$time), format="%Y-%m-%d %H")
#jemez_hourly$snow_depth_cm <-(jemez_hourly$snow_depth_mm*.1)
#head(jemez_hourly)
#fwrite(jemez_hourly, "/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly_date_corrected.csv")
jemez_hourly <-fread("/Volumes/JT/projects/uavsar/jemez/met_data/jemez_met_hourly_date_corrected.csv")


feb22<-dplyr::filter(jemez_hourly, date > "2020-02-20", date < "2020-02-23")%>%
  filter(site == "Redondo")
plot(feb22$date_time, feb22$snow_depth_cm)



depth<-ggplot(jemez_hourly, aes(x = date_time, y = snow_depth_cm, group = site, color = site, linetype = site)) +
  geom_line(size = .8)+
  scale_color_manual(values=c("grey50", "firebrick", "goldenrod"))+
  scale_linetype_manual("Station Name",values=c("Hidden Valley" = 4, "Redondo" = 3, "Valle Grande" = 2))+
  scale_x_datetime(date_breaks = "2 day",
                   labels = function(z) gsub("^0", "", strftime(z, "%m/%d")))+
  scale_y_continuous(breaks = seq(30,100,10))+
  labs(y = "Snow Depth (cm)", x= "Date",
       color  = "Station Name", linetype = "Station Name")+
  theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "top",
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 13, face = "bold"),
                  legend.margin = margin(t=0, unit='cm'))
print(depth)

setwd("/Volumes/JT/projects/uavsar/jemez/scatter_plots/")
ggsave(depth,
       file = "jemez_station_depth2_no_title2.png",
       width = 6, 
       height = 3,
       dpi = 400)
