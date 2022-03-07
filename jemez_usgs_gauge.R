# jack tarricone
# march 7th, 2022

### downloading jemez stream gauge data

library(dataRetrieval)
library(dplyr)
library(lubridate)

# meta data for download
siteNumber <- "08324000" # gauge number
site_info <- readNWISsite(siteNumber)
site_info
parameterCd <- "00060" # discharge

# download fully daily discharge time series
discharge <- readNWISdv(siteNumber,parameterCd)

# convert date
discharge$Date <-as.Date(raw_discharge$Date)

# change colnames
colnames(discharge)[3:5] <-c("date","discharge_cfs","quality_flag")
head(discharge)

# full ts plot
ggplot(discharge, aes(x = date, y = discharge_cfs))+
  geom_line()

# for wy2020
wy2020 <-filter(discharge, date >= "2019-10-01" & date <= "2020-09-30")
melt_szn <-filter(discharge, date >= "2020-02-10" & date <= "2020-05-31")

# 2020 water year
ggplot(wy2020, aes(x = date, y = discharge_cfs))+
  geom_line()

# feb - may
ggplot(melt_szn, aes(x = date, y = discharge_cfs))+
  geom_line()

