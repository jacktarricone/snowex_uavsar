# attempt envi reader in r
# updated sept 15th

# trying to read in binary llh file, split it into lat, lon, elevation
# and then resave single layers back into binary format so can be read into python properly

library(rgdal)
library(data.table)
library(caTools)
library(sf)
library(ggmap)
library(maps)
library(tmap)
library(terra)

# import the llh file using read.ENVI, no geo data attached just envi formatting
# the data is oriented from south to north becasue of the radar flight direction

##############################################################
#################### lat lon height (llh) ####################
##############################################################

llh <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.llh"
llh_hdr <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.llh.hdr"
llh_envi <-read.ENVI(llh, llh_hdr) 
readLines(llh_hdr)
dim(llh_envi)

# pull out first layer from data cube, latitude
lat <-matrix(llh_envi[,,1], nrow = 8636)
dim(lat)
lat_r <-terra::rast(lat)
plot(lat_r)

# pull out second layer, longtiude
lon <-matrix(llh_envi[,,2], nrow = 8636)
dim(lon)
lon_r <-terra::rast(lon)
plot(lon_r)

#third layer, height or elevation
ele <-matrix(llh_envi[,,3], nrow = 8636)
ele_r <-terra::rast(ele)
plot(ele_r)

########## saving single layers, or trying to

# print llh header
readLines(llh_hdr)

# test this build vrt function
setwd("/Users/jacktarricone/warp_test/good_llh_vrt/")
ele_raster <-raster::raster(ele)
raster::hdr(ele_raster, format="VRT", filename = "ele")
plot(ele_raster)

lat_raster <-raster::raster(lat)
raster::hdr(lat_raster, format="VRT", filename = "lat")

read_back_in <-"/Users/jacktarricone/warp_test/good_llh_vrt/ele.vrt"
hmmt <-raster::raster(read_back_in)
plot(hmmt)


# redefine data type to default on 4
write.ENVI2 <- function(X, filename, interleave=c("bsq", "bil", "bip") ) 
{ # write matrix or data cube to binary ENVI file
  if (is.vector(X)) {
    nCol = length(X)
    nRow <- nBand <- 1
  } else {
    d = dim(X)
    nRow  = d[1]
    nCol  = d[2]
    nBand = prod(d)/(nRow*nCol)
  }
  dim(X) = c(nRow, nCol, nBand)    # make it into 3D array in case it was not
  
  # check data type
  data.type = 0
  if (is.double (X)) data.type = 4 # 64-bit double
  if (is.integer(X)) data.type = 3 # 32-bit int
  if (is.complex(X)) data.type = 9 # 2x64-bit complex<double>
  if (data.type == 0) {            # do not know what is it -> make it a double
    X = as.double(X) 
    data.type = 5 
  } 
  
  # change interleave and store tha data
  interleave = match.arg(interleave)
  if      (interleave=="bil") X=aperm(X, c(2,3,1))  # R's [row,col,band] -> bil [col,band,row] 
  else if (interleave=="bip") X=aperm(X, c(3,2,1))  # R's [row,col,band] -> bip [band,col,row] 
  else if (interleave=="bsq") X=aperm(X, c(2,1,3))  # R's [row,col,band] -> bsq [col,row,band] 
  writeBin(as.vector(X), filename)                  # write Envi file
  
  # write header file
  out  = "ENVI\ndescription = { R-language data }\n"
  out  = paste(out, "samples = ", nCol, "\n", sep="")
  out  = paste(out, "lines = ", nRow, "\n", sep="")
  out  = paste(out, "bands = ", nBand, "\n", sep="")
  out  = paste(out, "data type = ",data.type,"\n", sep="")
  out  = paste(out, "header offset = 0\n", sep="")
  out  = paste(out, "interleave = ",interleave,"\n", sep="")   # interleave is assumed to be bsq - in case of 1 band images all 3 formats are the same 
  ieee = if(.Platform$endian=="big") 1 else 0       # does this machine uses ieee (UNIX) format? or is it intel format?
  out  = paste(out, "byte order = ", ieee, "\n", sep="")
  cat(out, file=paste(filename, ".hdr", sep=""))
  invisible(NULL)
}

write.ENVI2(lat, "/Users/jacktarricone/warp_test/good_llh_vrt/lat.llh", interleave = c("bsq"))
write.ENVI2(lon, "/Users/jacktarricone/warp_test/good_llh_vrt/lon.llh", interleave = c("bsq"))
write.ENVI2(ele, "/Users/jacktarricone/warp_test/good_llh_vrt/ele.llh", interleave = c("bsq"))


# test loading data back in
ele_path <-"/Users/jacktarricone/warp_test/good_llh_vrt/ele.llh"
ele_hdr <-"/Users/jacktarricone/warp_test/good_llh_vrt/ele.llh.hdr"
readLines(ele_hdr)

ele_envi <-read.ENVI(ele, headerfile=paste(ele, ".hdr", sep="")) 
readLines(ele_hdr)
dim(ele_envi)
q <-rast(ele_envi)
plot(q)
q




###########################################################
#################### look vector (lkv) ####################
###########################################################

lkv <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.lkv"
lkv_hdr <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.lkv.hdr"

lkv_envi <-read.ENVI(lkv, headerfile=paste(lkv, ".hdr", sep="")) 
readLines(lkv_hdr)
dim(lkv_envi)

# create east mat
east <-matrix(lkv_envi[,,1], nrow = 8636)
dim(east)
east_r <-rast(east)
plot(east_r)

# create north mat
north <-matrix(lkv_envi[,,2], nrow = 8636)
dim(north)
north_r <-rast(north)
plot(north_r)

# create up mat
up <-matrix(lkv_envi[,,3], nrow = 8636)

write.ENVI2(east, "/Users/jacktarricone/warp_test/good_llh_vrt/east", interleave = c("bsq"))
write.ENVI2(north, "/Users/jacktarricone/warp_test/good_llh_vrt/north", interleave = c("bsq"))
write.ENVI2(up, "/Users/jacktarricone/warp_test/good_llh_vrt/up", interleave = c("bsq"))


###########################################################
##### read back in new geotiffs from python code ##########
###########################################################



up_gc <-rast("/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_up_lkv_raw.tif")
values(up_gc)[values(up_gc) == 0] = NA
up_gc
plot(up_gc)
writeRaster(up_gc, "/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_up_lkv.tif")

north_gc <-rast("/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_north_lkv_raw.tif")
values(north_gc)[values(north_gc) == 0] = NA
plot(north_gc)
writeRaster(north_gc, "/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_north_lkv.tif")

east_gc <-rast("/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_east_lkv_raw.tif")
values(east_gc)[values(east_gc) == 0] = NA
plot(east_gc)
writeRaster(east_gc, "/Users/jacktarricone/warp_test/good_llh_vrt/geocoded_east_lkv.tif")

lvk_list <-list.files(patter ="/*_lkv.tif", full.names = T)
print(lvk_list)
lvk_stack <-rast(lvk_list)
writeRaster(lvk_stack, "/Users/jacktarricone/warp_test/good_llh_vrt/lvk_stack.tif")

plot(lvk_stack)
hist(north_gc)
