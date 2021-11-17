# gpr vs insar comparison start
# jack tarricone
# november 17th, 2021

library(terra)
library(sf)
library(sp)

### read in the ROUGH (is not an absolute change yet) SWE data
d_swe <-rast("/Volumes/JT/projects/uavsar/jemez/swe_inversion_lidar/delta_swe_lidar_test.tif")
plot(d_swe, col = brewer.pal(n = 1000, name = "RdBu")) # test plot

##### read in the three days of gpr data

# feb 12
feb12_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb12_GPR_ver2.2.csv")
feb12 <-vect(feb12_csv, geom = c("Long","Lat"), crs = crs(swe_crop))

# feb 20
feb20_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb20_GPR_ver2.csv")
feb20 <-vect(feb20_csv, geom = c("Long","Lat"), crs = crs(swe_crop))

# feb 26
feb26_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb26_GPR_ver2.csv")
feb26 <-vect(feb26_csv, geom = c("Long","Lat"), crs = crs(swe_crop))


# crop down to area around the GPR
vg <-ext(-106.5255, -106.521, 35.856, 35.8594)
swe_crop <-crop(d_swe, vg)
# writeRaster(swe_crop, "/Volumes/JT/projects/uavsar/jemez/swe_inversion_lidar/swe_vg_crop.tif")

# plot
plot(swe_crop)
points(feb12, cex = .1)
points(feb20, col = "red", cex = .1)
points(feb26, col = "green", cex = .1)

# mask SWE values for feb12 
gpr_only_pixels <-mask(swe_crop, feb12)
plot(gpr_only_pixels)

?extract
test <-extract(gpr_only_pixels, 
               feb12)

test <-as.data.frame(cellFromXY(gpr_only_pixels, cbind(feb12_csv$Long,feb12_csv$Lat)))


