library(terra)
library(XML)
library(xml2)
library(dplyr)

data <- xmlParse("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/jemez_march_14_2020_psscene_analytic_sr_udm2/files/20200314_172903_1014_3B_AnalyticMS_metadata_clip.xml")
data
print(data)
RizivNumber <- data %>% 
  xml_find_all("reflectanceCoefficient") %>% 
  xml_text()

xml_data <- xmlToList(data)
xml_data[["resultOf"]][["EarthObservationResult"]]

# https://developers.planet.com/tutorials/index2.html

# load in raw harmonized data from one aquisistion
raw_data <-rast("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/jemez_march_14_2020_psscene_analytic_sr_udm2/files/20200314_172905_1014_3B_AnalyticMS_SR_harmonized_clip.tif")
plot(raw_data)

# load in aux data
clip <-rast("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/jemez_march_14_2020_psscene_analytic_sr_udm2/files/20200314_172905_1014_3B_udm2_clip.tif")
snow_mask <-clip[[2]]
values(snow_mask)[values(snow_mask) == 0] <-NA

plot(raw_data[[3]])
plot(snow_mask, add = T)


r_max <-global(test[[3]], max, na.rm = TRUE)
red <-test[[3]]*.01
red

g_max <-global(test[[2]], max, na.rm = TRUE)
green <-test[[2]]*.01

b_max <-global(test[[1]], max, na.rm = TRUE)
blue <-test[[1]]*.01
blue


hmmt<-c(red,green,blue)

plotRGB(hmmt, r=1, g=2, b=3, stretch = "hist")

setwd("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/")
png("planetlabs_jemez_mar14.png", height=nrow(hmmt), width=ncol(hmmt)) # same dim as raster
plotRGB(hmmt, r=1, g=2, b=3, stretch = "hist", maxcell=ncell(hmmt)) # maxcell key
dev.off()

max(test[[1]])
