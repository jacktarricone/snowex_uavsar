# june 10th 2021
# testing working with RTC corrected sentinal data in R

library(gridExtra)
library(data.table)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)
library(terra)
library(zoo)


##### sentinal data
# bring in rasters
#vv <-raster("/Users/jacktarricone/Downloads/S1B_IW_20191231T010049_DVP_RTC30_G_saunem_3303/S1B_IW_20191231T010049_DVP_RTC30_G_saunem_3303_VV.tif")
#vv2 <-raster("/Users/jacktarricone/Downloads/S1B_IW_20191008T010051_DVP_RTC30_G_saunem_AC9F/S1B_IW_20191008T010051_DVP_RTC30_G_saunem_AC9F_VV.tif")

files_list <-list.files("/Users/jacktarricone/Desktop/sen1_data", pattern = ".tif", full.names = TRUE)
rast_list <-lapply(files_list, function(x) rast(x))
rast_list # list of rasters

# set crop extent
crop <-ext(357000, 382000, 3950000, 4000000)

# function to crop
crop_list <-function(x){
  crop(x, ext(crop))
}

# lapp
cropped <-lapply(rast_list, crop_list)
vv_stack <-rast(cropped)
vg <-crop(vv_db_stack, ext(vg_crop))
vv_db_stack <-10*log10(vv_stack)
feb29_mar05_change <-vv_db_stack[[21]] - vv_db_stack[[20]]

# plot
plot(feb29_mar05_change,
     range = c(-10,0),
     col = hcl.colors(255, palette = "grays"),
     main = "Feb 22 - Mar 4 S1 Amplitude Change Jemez")

#save
setwd("/Volumes/JT/projects/uavsar/jemez/sen1/")
png("mar5_VV.png", height = 924 * 0.707, width = 924 * 0.707, units = "mm" , res = 500, pointsize = 60)
plot(vv_db_stack[[21]],
     range = c(-12,0),
     col = hcl.colors(255, palette = "grays"),
     main = "March 5 S1 Amplitude Jemez")
dev.off()

vv_db_stack[[10]]
writeRaster(vv_db_stack[[21]], "/Volumes/JT/projects/uavsar/jemez/sen1/sen1_2020_03_05_VV.tif", overwrite = TRUE)

jemez_stack <-raster("/Volumes/JT/projects/uavsar/jemez/sen1/jemez_sen1_VV_stack.tif")

avg <-(vv_stack[[1]] + vv_stack[[10]] + vv_stack[[11]] + vv_stack[[12]] + vv_stack[[13]])/5
plot(avg)
dB_avg <-10*log10(avg)
plot(dB_avg)
writeRaster(dB_avg, "/Volumes/JT/projects/uavsar/jemez/sen1/dB_avg.tif")

# oct 8, 2020
oct08 <-vv_db_stack[[1]]
plot(oct08, main = names(oct08) , col = hcl.colors(255, palette = "grays"))
hist(oct08, breaks = 100)
hist(mar05, breaks = 100)

par(mfrow=c(1,2))
plot(vg[[4]], main = names(vg[[4]]), col = hcl.colors(255, palette = "grays"))
hist(vg[[4]], breaks = 100, xlim=c(-10,0))


# jan 5th plot and hist
jan05_vg <-vg[[13]]
setwd("/Volumes/JT/projects/uavsar/jemez/sen1")
png("jan05_vg.png", height = 500, width = 1200, units = "mm" , res = 150, pointsize = 50)
par(mfrow=c(1,2))
plot(jan05_vg, main = names(jan05_vg), col = hcl.colors(255, palette = "grays"))
hist(jan05_vg, breaks = 100, xlim=c(-10,0))
dev.off()

# feb 10
feb10_vg <-vg[[18]]

# feb 22
feb22_vg <-vg[[19]]
png("feb22_vg.png", height = 500, width = 1200, units = "mm" , res = 150, pointsize = 50)
par(mfrow=c(1,2))
plot(feb22_vg, main = names(feb22_vg), col = hcl.colors(255, palette = "grays"))
hist(feb22_vg, breaks = 100, xlim=c(-10,0))
dev.off()

# feb 29
feb29_vg <-vg[[20]]
png("feb29_vg.png", height = 500, width = 1200, units = "mm" , res = 150, pointsize = 50)
par(mfrow=c(1,2))
plot(feb29_vg, main = names(feb29_vg), col = hcl.colors(255, palette = "grays"))
hist(feb29_vg, breaks = 100, xlim=c(-10,0))
dev.off()

# mar 5
mar05_vg <-vg[[21]]
png("mar05_vg.png", height = 500, width = 1200, units = "mm" , res = 150, pointsize = 50)
par(mfrow=c(1,2))
par(mfrow=c(1,2))
plot(mar05_vg, main = names(mar05_vg), col = hcl.colors(255, palette = "grays"))
hist(mar05_vg, breaks = 100, xlim=c(-10,0))
dev.off()

# mar 12
mar12_vg  <-vg[[22]]

# mar 21
mar12_vg <-vg[[22]]
png("mar12_vg.png", height = 500, width = 1200, units = "mm" , res = 150, pointsize = 50)
par(mfrow=c(1,2))
par(mfrow=c(1,2))
plot(mar12_vg, main = names(mar12_vg), col = hcl.colors(255, palette = "grays"))
hist(mar12_vg, breaks = 100, xlim=c(-10,0))
dev.off()











# dec 12, 2019
dec12 <-vv_db_stack[[10]]
plot(dec12, main = names(dec12) , col = hcl.colors(255, palette = "grays"))

# dec 24, 2019
dec24 <-vv_db_stack[[11]]
plot(dec24, main = names(dec24) , col = hcl.colors(255, palette = "grays"))

# dec 31, 2019
dec31 <-vv_db_stack[[12]]
plot(dec31, main = names(dec31) , col = hcl.colors(255, palette = "grays"))

# jan 5, 2020
jan05 <-vv_db_stack[[13]]
plot(jan05, main = names(jan05) , col = hcl.colors(255, palette = "grays"))

# jan 12, 2020
jan12 <-vv_db_stack[[14]]
plot(jan12, main = names(jan12) , col = hcl.colors(255, palette = "grays"))

# jan 17, 2020
jan17 <-vv_db_stack[[15]]
plot(jan17, main = names(jan17) , col = hcl.colors(255, palette = "grays"))

# jan 24, 2020
jan24 <-vv_db_stack[[16]]
plot(jan24, main = names(jan24) , col = hcl.colors(255, palette = "grays"))

# jan 29, 2020
jan29 <-vv_db_stack[[17]]
plot(jan29, main = names(jan29) , col = hcl.colors(255, palette = "grays"))

# feb 10, 2020
feb10 <-vv_db_stack[[18]]
plot(feb10, main = names(feb10) , col = hcl.colors(255, palette = "grays"))

# feb 22, 2020
feb22 <-vv_db_stack[[19]]
plot(feb22, main = names(feb22) , col = hcl.colors(255, palette = "grays"))

# feb 29, 2020
feb29 <-vv_db_stack[[20]]
plot(feb29, main = names(feb29) , col = hcl.colors(255, palette = "grays"))

# mar 5, 2020
mar05 <-vv_db_stack[[21]]
plot(mar05, main = names(mar05) , col = hcl.colors(255, palette = "grays"))

# mar 12, 2020
mar12 <-vv_db_stack[[22]]
plot(mar12, main = names(mar12) , col = hcl.colors(255, palette = "grays"))

# mar 17, 2020
mar17 <-vv_db_stack[[23]]
plot(mar17, main = names(mar17) , col = hcl.colors(255, palette = "grays"))

# mar 17, 2020
mar17 <-vv_db_stack[[24]]
plot(mar17, main = names(mar17) , col = hcl.colors(255, palette = "grays"))


# full swath melt onset 5panel
png("jemez_snowmelt_onset_2020.png", height = 350, width = 1000, units = "mm" , res = 150, pointsize = 33)
par(mfrow=c(1,5))
plot(feb10, main = names(feb10), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(feb22, main = names(feb22), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(feb29, main = names(feb29), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(mar05, main = names(mar05), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(mar12, main = names(mar12), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
dev.off()

# VG melt onset 5panel
png("vg_snowmelt_onset_2020.png", height = 300, width = 1400, units = "mm" , res = 150, pointsize = 40)
par(mfrow=c(1,5))
plot(feb10_vg, main = names(feb10), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(feb22_vg, main = names(feb22), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(feb29_vg, main = names(feb29), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(mar05_vg, main = names(mar05), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(mar12_vg, main = names(mar12), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
dev.off()


