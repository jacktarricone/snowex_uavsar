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
}
jemez_raw # check, yup


# test plot
#plot(jemez[[21]], main = names(jemez[[21]]), col = hcl.colors(255, palette = "grays"), range = c(-12,0))

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
writeRaster(jemez_dB_ref, "/Volumes/JT/projects/uavsar/jemez/sen1/jemez_dB_ref.tif")

# crop to vg extent and save
vg_ref_raw <-crop(jemez_ref_raw, ext(vg_crop))
plot(vg_ref_raw)
vg_dB_ref <-10*log10(vg_ref_raw)
writeRaster(vg_dB_ref, "/Volumes/JT/projects/uavsar/jemez/sen1/vg_dB_ref.tif") 


# testing we snow algo

wet_snow<-function(raw_img){

wet <-10*log10(raw_img/vg_ref_raw)
plot(wet)
values(wet)[values(wet) > -2] = NA
plot(wet)
}

wet_snow(vg_raw[[22]])
