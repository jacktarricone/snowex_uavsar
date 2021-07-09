# july 8th 2021
# jack tarricone
# jemez UAVSAR slc stack data

# converting the binary look vector file (.lkv) from binary into 3 different rasters (easting, northing, elevation)
# converting binary binary lat, lon, elevation file into rasters to base the lvk data off
# using the baseline uncorrected data (BU) just bc cathleen said so, could use BC if neede

library(terra)
library(data.table)

# set paths
lkv_path <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.lkv"
llh_path <-"/Volumes/JT/projects/uavsar/jemez/look_vector/BU/alamos_35915_03_BU_s1_2x8.llh"

# read in the look vector data
lkv <-readBin(lkv_path, what = "numeric", n = 2^30, size = 4, endian = "little")
lkv_mat <-as.data.frame(lkv)
head(lkv_mat)

# seqence every third number, convert to data matrix with correct number of rows
east_mat <-matrix(lkv_mat[seq(1, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE)
north_mat <-matrix(lkv_mat[seq(2, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE)
up_mat <-matrix(lkv_mat[seq(3, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE)

# read in the lat/lon/ele data
llh <-readBin(llh_path, what = "numeric", n = 2^30, size = 4, endian = "little")
llh_mat <-as.data.frame(llh)
head(llh_mat)

# seqence every third number, convert to data matrix with correct number of rows, convert to raster
lat_mat <-matrix(llh_mat[seq(1, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE)
lon_mat <-matrix(llh_mat[seq(2, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE)
ele_mat <-matrix(llh_mat[seq(3, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE)


#ele_bin <-llh_mat[seq(3, nrow(llh_mat),3),]
#writeBin(ele_bin, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/ele_bin.grd",
#         size = "NA_integer_", endian = "little")
#?writeBin


# create list of matrix so we can flip them before converting to rasters
mat_list <-list(east_mat,north_mat,up_mat,lat_mat,lon_mat,ele_mat)
rast_list2 <-lapply(mat_list, rast)
look_stack2 <-rast(rast_list2)
ext(look_stack2) <-ext(-106.31461, -106.56371, 36.10769, 35.71848)
crs(look_stack2) <-"+proj=longlat +datum=WGS84 +ellps=WGS84"
plot(look_stack2[[3]])

# flipping function
flip_rasters <-function(x){
  x[nrow(x):1,ncol(x):1]
}

flipped_list <-lapply(mat_list, flip_rasters) # flip whole list
rast_list <-lapply(flipped_list, rast) # convert matrix list to raster list
rast_list # check, yup

look_stack <-rast(rast_list) # convert raster list to a stack
look_stack #insect
plot(look_stack[[3]]) # test plot


# save non-geocoded tiff
#writeRaster(look_stack, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/look_stack_nocrs.tiff")


# set extent by min and max of lat lon data
ext(look_stack) <-ext(-106.31461, -106.56371, 36.10769, 35.71848)
look_stack

# set crs
crs(look_stack) <-"+proj=longlat +datum=WGS84 +ellps=WGS84"
plot(look_stack2[[6]])

writeRaster(look_stack2[[3]], "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/up_test_flip.tiff")
fwrite(ele_mat, "/Volumes/JT/projects/uavsar/jemez/look_vector/BU/ele_bin.txt", row.names = FALSE, col.names = FALSE)
ele_bin <-look_stack[[6]]
