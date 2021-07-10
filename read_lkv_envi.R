# attempt envi reader in r

library(terra)
library(rgdal)
library(data.table)
library(caTools)
library(sf)
library(ggmap)
library(maps)
library(tmap)


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

#############################
#### rotation of axes
###########################

#### lat lon
# Corner 1 (&) = 35.734333038, -106.314704895
# Corner 2 (&) = 35.718479156, -106.553138733       
# Corner 3 (&) = 36.107688904, -106.334304810    
# Corner 4 (&) = 36.091930389, -106.563705444  

#### utm zone 13 north (easting, northing)
# Corner 1 (&) = 381109, 3955279 
# Corner 2 (&) = 359518, 3953836
# Corner 3 (&) = 379903, 3996717
# Corner 4 (&) = 359225, 3995277


#nm
nm_shape <-readOGR("/Volumes/JT/projects/uavsar/jemez/look_vector/nm_shape/tl_2017_35_tract.shp") 

# create pts df
pts<-as.matrix(rbind(c(381109, 3955279 ), c(359518, 3953836), c(379903, 3996717), c(359225, 3995277)))
names(pts) <-c('x','y')
corners <-st_multipoint(pts)
plot(corners, main = "Corners")
class(corners)

#convert to corrdinates and define initial coordinates systems
coordinates(corners) <- c('x', 'y')
proj4string(corners)=CRS("+init=epsg:32613")  #32613 is the grid for new mexico
plot(ele_resamp)
plot(corners)

#Transform the Ireland's grid to longitude & latitude
coord<-spTransform(point,CRS("+init=epsg:4326"))
coord

?tm_shape
tm_shape(nm_shape) +
  tm_polygons() +
  tm_graticules(col = "grey60") +
  tm_shape(corners) +
  tm_symbols(col = "black") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(pts) +
  tm_text("pt", ymod = -1)
