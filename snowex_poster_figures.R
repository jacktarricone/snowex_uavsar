# figures for snowex poster

library(data.table)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
library(forcats)
library(plyr) 

# read in data
lwc_den <-read.csv("/Volumes/jt/UNR_fall_20/snowex/data_analysis/pits/lwc/lwc_density_r.csv")
swe <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/snowex_swe_r.csv")
depth <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/snowex_depth_r.csv")
pit_avgs <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/pits/pit_avgs_old.csv") 
all_pits_good <-read.csv("/Volumes/JTARRICONE/UNR_fall_20/snowex/data_analysis/pits/all_pits_good.csv") 

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

# save new data set
#write.csv(all_pits_good, "/Volumes/JTARRICONE/UNR_fall_20/snowex/data/pits/all_pits_good.csv") 

## 9/01/2020

# snow pit plots

plot <-ggplot(all_pits_good, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = density, group = location), stat = "identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"}))) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit Time Series", y = "Depth (cm)", x = "Date") 

print(plot)

# assign levels to location data so you can put in proper order for poster

neworder <- c("Tower 4","Forest","Open")
library(plyr)  ## or dplyr (transform -> mutate)
all_pits_good <- arrange(transform(all_pits_good,
                           location=factor(location,levels=neworder)),location)

# filter for single sites
tower4 <-filter(all_pits_good, location == "Tower 4")
open <-filter(all_pits_good, location == "Open")
forest <-filter(all_pits_good, location == "Forest")

# tower 4
ggplot(tower4, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"})),
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  labs(title="Tower 4 Snow Pit Timeseries", y = "Depth (cm)", x = "Date")

# open
ggplot(open, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6",name = expression('Density'~(kg/m^{"3"})), 
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous("Depth (cm)", limits = c(0, 115), breaks = seq(0, 120, by = 10)) +
  labs(title="Open Snow Pit Timeseries", x = "Date")

# forest
ggplot(forest, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"})), 
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous("Depth (cm)", limits = c(0, 115), breaks = seq(0, 120, by = 10)) +
  labs(title="Forest Snow Pit Timeseries", x = "Date")




##### pit avgs plots

# convert to date data type
pit_avgs$date <- as.Date(pit_avgs$date)

# chop off 2020-
display_date <- gsub("2020-", "", pit_avgs$date)

# add new column
pit_avgs <-cbind(pit_avgs, display_date)

# take off zeros for graph
pit_avgs$display_date <- gsub("01-", "1/", pit_avgs$display_date)
pit_avgs$display_date <- gsub("02-", "2/", pit_avgs$display_date)
pit_avgs$display_date <- gsub("03-", "3/", pit_avgs$display_date)


pit_avgs$depth_mm_graph <- pit_avgs$depth_mm - pit_avgs$swe_mm

#write.csv(pit_avgs, "/Volumes/JTARRICONE/UNR_fall_20/snowex/data/pits/pit_avgs.csv") 

neworder <- c("Tower 4","Forest","Open")
library(plyr)  ## or dplyr (transform -> mutate)
pit_avgs <- arrange(transform(pit_avgs,
                                   location=factor(location,levels=neworder)),location)

theme_set(theme_light(base_size =11))
ggplot(pit_avgs, aes(display_date, swe_mm)) +
  geom_bar(stat="identity", fill = "firebrick" ) +
  scale_y_continuous(breaks = seq(0,1200,100)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit SWE Time Series", y = "SWE (mm)", x = "Date") 

ggplot(data = df, aes(x = dateValues, y = value, fill = variable)) + 
  geom_bar(stat = "identity")



ggplot(NULL, aes(lab, perc)) + 
  geom_bar(aes(fill = "dEQ"), data = dEQ, alpha = 0.5) +
  geom_bar(aes(fill = "LMD"), data = LMD, alpha = 0.5)

my_data_long <- melt(my_data, id.vars = c("Block"))

ggplot(data=my_data_long,aes(x=Block, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))

#####

##### swe vs cc plot

theme_set(theme_light(base_size =11))
ggplot(swe)+ 
  geom_point(aes(cc_percent, swe_20200129, color="01/29/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200205, color="02/05/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200212, color="02/12/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200219, color="02/19/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200226, color="02/26/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200304, color="03/04/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200311, color="03/11/2020"),size = 2) +
  geom_point(aes(cc_percent, swe_20200322, color="03/22/2020"),size = 2) +
  scale_x_continuous("LiDAR Canopy Cover %", breaks = c(0,20,40,60,80,100)) +
  scale_y_continuous("SWE (mm)", breaks = c(0,50,100,150,200,250,300,350)) +
  scale_colour_manual(values=c('01/29/2020'="#ffffcc",
                               '02/05/2020' = "#edf8b1", '02/12/2020' = "#c7e9b4", 
                               '02/19/2020' = "#7fcdbb", '02/26/2020' = "#41b6c4",
                               '03/04/2020' = "#1d91c0", '03/11/2020' = "#225ea8",'03/22/2020' = "#0c2c84"))+
  labs(title="Sagehen Creek SWE vs. Canopy Cover %", 
       y="SWE (mm)", x="Distance along Transect (m)", color = "Date")


# 3/22 swe vs cc test plot

ggplot(swe)+ 
  geom_point(aes(cc_percent, swe_20200322, color="03/22/2020"),size = 2) +
  scale_x_continuous("LiDAR Canopy Cover %", breaks = c(0,20,40,60,80,100)) +
  scale_y_continuous("SWE (mm)", breaks = c(0,50,100,150,200,250,300,350)) +
  labs(title="Sagehen Creek SWE vs. Canopy Cover %", 
       y="SWE (mm)", x="Distance along Transect (m)", color = "Date")



theme_set(theme_light(base_size =11))
ggplot(depth)+ 
  geom_line(aes(distance_m, depth_20201220, color="12/20/2019"),size = .7) +
  geom_line(aes(distance_m, depth_20200129, color="01/29/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200205, color="02/05/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200212, color="02/12/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200219, color="02/19/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200226, color="02/26/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200304, color="03/04/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200311, color="03/11/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200319, color="03/19/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200322, color="03/22/2020"),size = .7) +
  scale_x_continuous(breaks = seq(0,320,20)) +
  scale_y_continuous(breaks = seq(0,350,20)) +
  scale_colour_manual(values=c('12/20/2019'="red",'01/29/2020'="darkblue",
                               '02/05/2020' = "turquoise3", '02/12/2020' = "darkseagreen1", 
                               '02/19/2020' = "grey45", '02/26/2020' = "deeppink3",
                               '03/04/2020' = "red", '03/11/2020' = "green",
                               '03/19/2020' = "black", '03/22/2020' = "blue"))+
  labs(title="Sagehen Creek WY2020 Depth Transect", 
       y="Depth (cm)", x="Distance along Transect (m)", color = "Date")



### test with new data 9/15

# read in data
lwc_den <-read.csv("/Volumes/jt/UNR_fall_20/snowex/data_analysis/pits/lwc/lwc_density_r_new.csv")

# convert to date data type
lwc_den$date <- as.Date(lwc_den$date)

#chop off 2020-
display_date <- gsub("2020-", "", lwc_den$date)

#add new column
lwc_den<-cbind(lwc_den, display_date)

# take off zeros for graph
lwc_den$display_date <- gsub("01-", "1/", lwc_den$display_date)
lwc_den$display_date <- gsub("02-", "2/", lwc_den$display_date)
lwc_den$display_date <- gsub("03-", "3/", lwc_den$display_date)

#set chart order
neworder <- c("Tower 4","Forest","Open")
lwc_den <- arrange(transform(lwc_den,
                              location=factor(location,levels=neworder)),location)

# Webb LWC
# .037Es+((((Es-.051rho))-(3.17e-5)rhoe2)/158.8)-.08

lwc <- .037*(lwc_den$avg_dielec)+(((lwc_den$avg_dielec +.051*lwc_den$avg_density)-((3.17e-5)*(lwc_den$avg_density)^2))/158.8)-.08
lwc_den <-cbind(lwc_den, lwc)

# pit density time series

theme_set(theme_light(base_size =11))
density <-ggplot(lwc_den, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = avg_density, group = location), stat = "identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"}))) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit Time Series", y = "Depth (cm)", x = "Date") 

print(density)

# dielectric vs depth

dielec <-ggplot(lwc_den, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = avg_dielec, group = location), stat = "identity") +
  scale_fill_continuous(high = "#67000d", low = "#fff5f0", name = "Dielectric Constant") +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit Dielectric Constant", y = "Depth (cm)", x = "Date") 

print(dielec)

# lwc vs depth

lwc <-ggplot(lwc_den, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = lwc, group = location), stat = "identity") +
  scale_fill_continuous(high = "#014636", low = "#f7fcfd", name = "LWC") +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Volumetic LWC", y = "Depth (cm)", x = "Date") 

print(lwc)



# read in data for the two days we did two readings

twoaday <-read.csv("/Volumes/jt/UNR_fall_20/snowex/data_analysis/pits/open_2aday_lwc.csv")

neworder <- c("2/26 10:15 am","2/26 3:00 pm","3/11 9:50 am", "3/11 3:30 pm")
twoaday <- arrange(transform(twoaday,
                             date=factor(date,levels=neworder)),date)

compare <-ggplot(twoaday, aes(date, seg_cm)) +
  geom_bar(aes(fill = avg_dielec, group = location), stat = "identity") +
  scale_fill_continuous(high = "#67000d", low = "#fff5f0", name = "Dielectric Constant") +
  scale_y_continuous(breaks = seq(0,40,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Open WISe Morning vs. Afternoon", y = "Depth (cm)", x = "Date") 

print(compare)

# permitivty vs density

ggplot(lwc_den, aes(avg_dielec, avg_density)) +
  geom_point()
