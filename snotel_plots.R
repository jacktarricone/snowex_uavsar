library(snotelr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# download data for two sites within InSAR swath
quemazon <-as.data.frame(snotel_download(708, path = tempdir(), internal = TRUE))
garita <-as.data.frame(snotel_download(1173, path = tempdir(), internal = TRUE))

# format dates
quemazon$date <-ymd(quemazon$date)
garita$date <-ymd(garita$date)

# filter for wy2020
quemazon_wy2020 <-filter(quemazon, date >= "2019-10-01" & date < "2020-10-01")
garita_wy2020 <-filter(garita, date >= "2019-10-01" & date < "2020-10-01")

# full year
ggplot()+
  geom_line(quemazon_wy2020, mapping = aes(x = date, y = snow_water_equivalent))+
  geom_line(garita_wy2020, mapping = aes(x = date, y = snow_water_equivalent), color = "red")

# study period
que_insar <-filter(quemazon, date >= "2020-02-10" & date < "2020-04-01")
gar_insar <-filter(garita, date >= "2020-02-10" & date < "2020-04-01")
  
# plot
ggplot()+
  geom_line(gar_insar, mapping = aes(x = date, y = snow_water_equivalent))+
  geom_line(que_insar, mapping = aes(x = date, y = snow_water_equivalent), color = "red")
  