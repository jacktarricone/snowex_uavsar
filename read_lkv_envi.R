# attempt envi reader in r

library(terra)
library(rgdal)
library(data.table)
library(caTools)

##### look vector

lkv <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.lkv"
lvk_hdr <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.lkv.hdr"

lkv_envi <-read.ENVI(lkv, headerfile=paste(lkv, ".hdr", sep="")) 
readLines(lkv_hdr)
dim(lkv_envi)

# create mat
east <-matrix(lkv_envi[,,1], nrow = 8636)
east<-east[nrow(east):1,ncol(east):1] #flip

# create mat
north <-matrix(lkv_envi[,,2], nrow = 8636)
north<-north[nrow(north):1,ncol(north):1] #flip

# create mat
up <-matrix(lkv_envi[,,3], nrow = 8636)
up<-up[nrow(up):1,ncol(up):1] #flp

write.ENVI(east, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/east.lkv", interleave = c("bsq"))
write.ENVI(up, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/up.lkv", interleave = c("bsq"))
write.ENVI(north, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/north.lkv", interleave = c("bsq"))

##### lat lon dem

llh <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.llh"

llh_envi <-read.ENVI(llh, headerfile=paste(llh, ".hdr", sep="")) 
readLines(llh_hdr)
dim(llh_envi)


lat <-matrix(llh_envi[,,1], nrow = 8636)
lat<-lat[nrow(lat):1,ncol(lat):1]

lon <-matrix(llh_envi[,,2], nrow = 8636)
lon<-lon[nrow(lon):1,ncol(lon):1]

ele <-matrix(llh_envi[,,3], nrow = 8636)
ele<-ele[nrow(ele):1,ncol(ele):1]

write.ENVI(lat, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/lat.llh", interleave = c("bsq"))
write.ENVI(lon, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/lon.llh", interleave = c("bsq"))
write.ENVI(ele, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/ele.llh", interleave = c("bsq"))

up_tiff <- rast("/Volumes/JT/projects/uavsar/jemez/look_vector/BU/envi_files/up.lkv.tiff")
up_tiff
insar_dem <-rast("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")
insar_dem
ele_resamp <-resample(up_tiff, insar_dem, method = "bilinear")
plot(ele_resamp)
ele_resamp
plot(insar_dem)
