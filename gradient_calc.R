
# notes from meeting with HP October 25th

### creating new incidence angle raster from CZO LiDAR data
# will be used for SWE inversion of UAVSAR data

## steps
# 1.create slope, aspect rasters from filtered DEM
# 2.reproject and resample lidar data products to UAVSAR projection (wsg-84, lat/lon)
# 3.use these resampled products to create new .inc file

# function to calculate "gradient" of LiDAR raster (vector)
# three component vector (easting, northing, 3 component vector)
# calculate dot product and calculate the angle
# dot product of gradient from lidar raster and path length vector (n1*n2+e1*e2+up1*up2)
# cos^-1((n1*n2+e1*e2+up1*up2)/(distance calc through atm for each vector))

# packages in r or python for calculating gradients and surface normals
library(terra)
library(raster)
library(rgdal)
library(insol) # https://rdrr.io/cran/insol/man/cgrad.html

# switched back to using the raster package vs cgrad can injest only rasters not SpatRasters!

# bring in lidar dem
lidar_dem <-raster("/Volumes/JT/projects/uavsar/jemez/lidar/ele_filt/GISdata/JemezRiver/lidar/snow_off/valles_elev_filt.img")
plot(lidar_dem, col = terrain.colors(3000)) # test plot

# crop down
crop_ext <-extent(360000, 374000, 3965000, 3980000) # set vg UTM extent
lidar_crop <-crop(lidar_dem, crop_ext)
plot(lidar_crop, col = terrain.colors(3000)) # test, good

# bring in resampled lidar
# lidar_resamp <-raster("/Volumes/JT/projects/uavsar/jemez/lidar/lidar_uavsar_dem_resamp.tif")
# plot(lidar_resamp, col = terrain.colors(3000)) # test, good
# crop_ext <-extent(-106.55545, -106.40, 35.8, 35.95)
# resamp_crop <-crop(lidar_resamp, crop_ext)
#  
# plot(resamp_crop, col = terrain.colors(3000)) # test plot

# crop to there are no NaNs
# crop_ext <-ext(-106.55545, -106.40, 35.8, 35.95)
# resamp_crop <-crop(lidar_resamp, crop_ext)
# plot(resamp_crop)

# calculate the gradident in all three dimensions
grad_mat <-cgrad(lidar_crop, 1, 1, cArea = FALSE)

# make individual raster layer for each competent
# and geocode back to original crop extent
# switch back to terra

## x
x_comp <-raster(grad_mat[,,1], crs = crs(lidar_crop))
extent(x_comp) <-extent(crop_ext)
plot(x_comp)

## y
y_comp <-raster(grad_mat[,,2], crs = crs(lidar_crop))
extent(y_comp) <-extent(crop_ext)
plot(y_comp)

## z
z_comp <-raster(grad_mat[,,3], crs = crs(lidar_crop))
extent(z_comp) <-extent(crop_ext)
plot(z_comp)

# cos^-1((n1*n2+e1*e2+up1*up2)/(distance calc through atm for each vector))
surf_norm <-(x_comp*x_comp)+(y_comp*y_comp)+(z_comp*z_comp)
plot(surf_norm)

######### next steps
# resample surface normal up 5.6m
# plv is already at 5.6m
# then do .inc angle calc
# inc_angle <-cos^-1(-surf_norm*plk)


# bring in path length vector data
plv <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/plv_km_good.tif")
plot(plv)

# convert surface normal raster to spatrast for resampling
# terra can go from utm to lat/lon, raster cannot!
surf_norm_sp <-rast(surf_norm)

# resample surf norm file up to plv file
surf_norm_resamp5.6 <-resample(surf_norm_sp, plv, method = "bilinear")

plot(plv)
plot(surf_norm_resamp5.6, add = TRUE)

inc_angle_rad <-(acos)(-surf_norm_resamp5.6*plv)
inc_angle_deg <-inc_angle_rad*(180/pi)
plot(inc_angle_deg)

cos^-1((y_comp+x_comp+z_comp)/(distance calc through atm for each vector))








# bring in resampled lidar
# lidar_resamp <-raster("/Volumes/JT/projects/uavsar/jemez/lidar/lidar_uavsar_dem_resamp.tif")
# plot(lidar_resamp, col = terrain.colors(3000)) # test, good
# crop_ext <-extent(-106.55545, -106.40, 35.8, 35.95)
# resamp_crop <-crop(lidar_resamp, crop_ext)
#  
# plot(resamp_crop, col = terrain.colors(3000)) # test plot

# crop to there are no NaNs
# crop_ext <-ext(-106.55545, -106.40, 35.8, 35.95)
# resamp_crop <-crop(lidar_resamp, crop_ext)
# plot(resamp_crop)






# set random 1000x1000 cell crop extent
crop_ext <-ext(360000, 361000, 3970000, 3971000)

# crop to ext
lidar_crop <-crop(lidar_dem, crop_ext)
plot(lidar_crop,  col = terrain.colors(3000)) # test plot

# covert crop into matrix, no NaNs, good
dem_mat <-matrix(lidar_crop, nrow = 1000)





inc_angle <-cos^-1(-surf_norm*plk)
cos^-1((y_comp+x_comp+z_comp)/(distance calc through atm for each vector))


##### from stack exchange
# https://stackoverflow.com/questions/25704711/how-do-i-calculate-the-gradient-of-a-matrix-to-draw-a-vector-field-in-r

bdiff <- function(x) c(NA,diff(x))
x_grad <-t(apply(dem_mat,1,bdiff))
y_grad <-apply(dem_mat,2,bdiff)


rr <- row(dem_mat)
cc <- col(dem_mat)
dx <- t(apply(dem_mat,1,bdiff))
dy <- apply(dem_mat,2,bdiff)
sc <- 0.25
off <- -0.5 ## I *think* this is right since we NA'd row=col=1
plot(rr,cc,col="gray",pch=16)
arrows(rr+off,cc+off,rr+off+sc*dx,cc+off+sc*dy,length=0.05)
