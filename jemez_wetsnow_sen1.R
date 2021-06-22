# start of my wet snow analysis in the jemez
# june 15th
# using terra!

library(terra)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(sp)
library(data.table)
library(rgeos)
library(ggplot2)
library(gridExtra)
library(rasterVis)

# bring in jemez stack i made in orginal script
# one that converted to dB
jemez_dB <-rast("/Volumes/JT/projects/uavsar/jemez/sen1/jemez_sen1_VV_stack.tif")
jemez_dB

# one thats in linear amplitude, use this for creating reference raster
jemez_raw <-rast("/Volumes/JT/projects/uavsar/jemez/sen1/jemez_sen1_VV_stack_nodB.tif")
jemez_raw

# get dates to rename rasters
list.files("/Users/jacktarricone/desktop/sen1_data/")
dates <-read.table("/Users/jacktarricone/desktop/sen1_data/sen1_dates.csv")
dates$V1[1]

# loop to rename stack layers from csv file
for (i in 1:length(dates$V1)) {
  names(jemez_raw)[[i]] <-dates$V1[i]
  names(jemez_dB)[[i]] <-dates$V1[i]
}

jemez_raw # check, yup
jemez_dB

# test plot
plot(jemez_dB[[21]], main = names(jemez_dB[[21]]), col = hcl.colors(255, palette = "grays"), range = c(-12,0))

# set crop extent
crop <-ext(357000, 382000, 3950000, 4000000) # full data set
vg_crop <-ext(360000, 373000, 3965000, 3974000) # valle grande

# crop down to vg extent
vg_raw <-crop(jemez_raw, ext(vg_crop))
plot(vg_raw[[6]])
vg_dB <-10*log10(vg_raw)

# test plot
#plot(vg[[5]], main = names(vg[[5]]), col = hcl.colors(255, palette = "grays"), range = c(-12,0))


# testing creating refence image for for whole jemez and vg
# avg first 13 images in the stack
# this is just a first pass, need to see if adding more from the summer changes anything
jemez_ref_raw <-(jemez_raw[[1]]+jemez_raw[[2]]+jemez_raw[[3]]+jemez_raw[[4]]+jemez_raw[[5]]+
             jemez_raw[[6]]+jemez_raw[[7]]+jemez_raw[[8]]+jemez_raw[[9]]+
             jemez_raw[[10]]+jemez_raw[[11]]+jemez_raw[[12]]+jemez_raw[[13]])/13
plot(jemez_ref_raw) # test plot
jemez_dB_ref <-10*log10(jemez_ref_raw) # convert to DB
jemez_dB_ref
plot(jemez_dB_ref)
#writeRaster(jemez_dB_ref, "/Volumes/JT/projects/uavsar/jemez/sen1/jemez_dB_ref.tif")

# crop to vg extent and save
vg_ref_raw <-crop(jemez_ref_raw, ext(vg_crop))
plot(vg_ref_raw)
vg_dB_ref <-10*log10(vg_ref_raw)
writeRaster(vg_dB_ref, "/Volumes/JT/projects/uavsar/jemez/sen1/vg_dB_ref.tif") 

#####
jemez_dB_ref <-rast("/Volumes/JT/projects/uavsar/jemez/sen1/jemez_dB_ref.tif")

# testing we snow algo

jemez_wet_snow<-function(raw_img){
  
wet <-10*log10(raw_img/jemez_ref_raw) #divide (or log substract) raw from ref
values(wet)[values(wet) > -2] = NA # set na if value is greater than -2 dB
#values(wet)[values(wet) <= -2 ] = 1 # change all other values 
plot(wet, main = names(wet), col = hcl.colors(255, palette = "viridis"))
return(wet)

}
vg_wet_snow<-function(raw_img){
  
  wet <-10*log10(raw_img/vg_ref_raw) #divide (or log substract) raw from ref
  values(wet)[values(wet) > -2] = NA # set na if value is greater than -2 dB
  #values(wet)[values(wet) <= -2 ] = 1 # change all other values 
  plot(wet, main = names(wet), 
      xlab = "Easting",
      ylab = "Northing",
      col = hcl.colors(255, palette = "viridis"))
  return(wet)
}

# just vg function
test <-vg_wet_snow(vg_raw[[21]])

plot(test)
 
# full scene function
test <-jemez_wet_snow(jemez_raw[[24]])

# just vg function
vg_wet_snow(vg_raw[[21]])
writeRaster(feb29_wet, "/Volumes/JT/projects/uavsar/jemez/sen1/feb29_wet.tif")

########################################################
##### bring in fsca images for basemap of wetsnow plots
########################################################

# bring in feb 18 and mar 5th fsca
fsca_0218_raw <-rast("/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/LC08_CU_010012_20200218_20200227_C01_V01_SNOW/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif")
fsca_0305_raw <-rast("/Volumes/JT/projects/uavsar/jemez/fsca/03_05_2020/LC08_CU_010012_20200305_20200316_C01_V01_SNOW/LC08_CU_010012_20200305_20200316_C01_V01_SNOW.tif")
bare_earth_raw <-rast("/Volumes/JT/projects/uavsar/jemez/fsca/LC08_L1TP_033035_20200305_20200314_01_T1/LC08_L1TP_033035_20200305_20200314_01_T1.tif")

# plot and check
plot(fsca_0218_raw)
plot(fsca_0305_raw)
plot(bare_earth_raw)
fsca_0218_raw
fsca_0305_raw
bare_earth_raw

#reproject to sen1 proj and extent
fsca_0218_reproj <-terra::project(fsca_0218_raw, jemez_dB_ref)
fsca_0305_reproj <-terra::project(fsca_0305_raw, jemez_dB_ref)
bare_earth_reproj <-terra::project(bare_earth_raw, jemez_dB_ref)
values(fsca_0218_reproj)[values(fsca_0218_reproj) == 0] <-NA #0 to NA
values(fsca_0305_reproj)[values(fsca_0305_reproj) == 0] <-NA #0 to NA

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(bare_earth_reproj,
        r = 2, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")
bare_earth_vg <-crop(bare_earth_reproj, vg_crop)
plotRGB(bare_earth_vg)
#test plot
plot(bare_earth_reproj, main = names(jemez_dB[[21]]), col = hcl.colors(255, palette = "grays"), range = c(-12,0))
plot(fsca_0218_reproj, add = TRUE)

## crop to valle grande extent
# 2/18
fsca_0218_vg <-crop(fsca_0218_reproj, vg_crop)
plot(fsca_0218_vg, main = "Valle Grande 2/18 fSCA", col = hcl.colors(255, palette = "grays"), range = c(0,1000))

# 3/05
fsca_0305_vg <-crop(fsca_0305_reproj, vg_crop)
plot(fsca_0305_vg,  main = "Valle Grande 3/5 fSCA", col = hcl.colors(255, palette = "grays"), range = c(0,1000))
plot(test, add =TRUE, legend = FALSE,
      col = hcl.colors(255, palette = "viridis"))

# just vg function
mar5 <-vg_wet_snow(vg_raw[[21]])

# march 5 wet snow
setwd("/Volumes/JT/projects/uavsar/jemez/sen1")
png("fsc_wetsnow_0305.png", height = 924 * 0.707, width = 924, units = "mm" , res = 150, pointsize = 60)
plot(fsca_0305_vg,  main = "Valle Grande 3/5 fSCA and Sen-1 Wet Snow", col = hcl.colors(255, palette = "grays"), range = c(0,1000))
plot(mar5, add =TRUE, legend = FALSE,
     col = hcl.colors(255, palette = "viridis"))
dev.off()

feb22 <-vg_wet_snow(vg_raw[[19]])

png("fsc_wetsnow_0218.png", height = 924 * 0.707, width = 924, units = "mm" , res = 150, pointsize = 60)
plotRGB(bare_earth_vg)
plot(fsca_0218_vg, add = TRUE,  
     main = "Valle Grande 2/22 fSCA and Sen-1 Wet Snow", 
     col = hcl.colors(255, palette = "grays"), range = c(0,1000))
plot(feb22, add =TRUE, legend = FALSE,
     col = hcl.colors(255, palette = "viridis"))
dev.off()



