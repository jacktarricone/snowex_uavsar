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
east <-rast(matrix(lkv_mat[seq(1, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE))
north <-rast(matrix(lkv_mat[seq(2, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE))
up <-rast(matrix(lkv_mat[seq(3, nrow(lkv_mat),3),], nrow = 8636, byrow = TRUE))

# read in the lat/lon/ele data
llh <-readBin(llh_path, what = "numeric", n = 2^30, size = 4, endian = "little")
llh_mat <-as.data.frame(llh)
head(llh_mat)

# seqence every third number, convert to data matrix with correct number of rows, convert to raster
lat <-rast(matrix(llh_mat[seq(1, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE))
lon <-rast(matrix(llh_mat[seq(2, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE))
ele <-rast(matrix(llh_mat[seq(3, nrow(llh_mat),3),], nrow = 8636, byrow = TRUE))

#creat a stack
look_stack_raw <-c(east,north,up,lat,lon,ele)
look_stack_raw

look_stack <-look_stack_raw[nrow(look_stack_raw):1,ncol(look_stack_raw):1]
plot(look_stack)



lvk_crs <-crs("+proj=longlat +datum=WGS84 +ellps=WGS84")
crs(yert) <-"+proj=longlat +datum=WGS84 +ellps=WGS84"
yert <-rast(xmax = 35.732368469, ymin = -106.31452941)
yert

writeBin(test, "/Volumes/JT/projects/uavsar/jemez/look_vector/alamos_35915_01_BU_s1_2x8.lkv1.grd", 
         size = 4, endian = "little")
fwrite(test2, "/Volumes/JT/projects/uavsar/jemez/look_vector/alamos_35915_01_BU_s1_2x8.lkv2.grd",
       row.names = FALSE, col.names = FALSE)
fwrite(test3, "/Volumes/JT/projects/uavsar/jemez/look_vector/alamos_35915_01_BU_s1_2x8.lkv3.grd",
       row.names = FALSE, col.names = FALSE)


yert<-rast(test3, nrows = 8628, ncols = 4895, xmax = 35.732368469, ymin = -106.314529419)

