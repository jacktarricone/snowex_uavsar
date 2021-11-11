
# november 10th, 2021
# jack tarricone
# almost done

####
# using the function cgrad, from the insol package
# calculate the 3d (x,y,z) unit vector for a valle grande crop of the lidar dem
# then add? not sur yet, to make surface normal vector

library(terra)
library(raster)
library(rgdal)
library(insol) # https://rdrr.io/cran/insol/man/cgrad.html


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



#########################################################################

  
  
# bring in lidar dem with raster not terra
# switched back to using the raster package bc cgrad can injest only rasters not SpatRasters!
lidar_dem <-raster("/Volumes/JT/projects/uavsar/jemez/lidar/ele_filt/GISdata/JemezRiver/lidar/snow_off/valles_elev_filt.img")
plot(lidar_dem, col = terrain.colors(3000)) # test plot

# crop down
crop_ext <-extent(359000, 374000, 3965000, 3980000) # set vg UTM extent
lidar_crop <-crop(lidar_dem, crop_ext)
plot(lidar_crop, col = terrain.colors(3000)) # test, good

# plv <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/plv_km_good.tif")
# 
# lidar_sp <-rast(lidar_crop)
# test_resamp <-resample(lidar_sp, plv, method = "bilinear")
# plot(plv)
# plot(test_resamp, add = TRUE)

########
# calculate the gradient in all three dimensions
# this function create a matrix for the x,y,z competent of a unit vector
########

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

# convert to spatrasts
x_comp <-rast(x_comp)
y_comp <-rast(y_comp)
z_comp <-rast(z_comp)

# calculate surface normal
# i'm not sure this is right
surf_norm <-(x_comp)+(y_comp)+(z_comp)
surf_norm
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
# resample surf norm file up to plv file
surf_norm_resamp5.6 <-resample(surf_norm, plv, method = "bilinear")

# plot path length vector with resample surf_norm on top
plot(plv)
plot(surf_norm_resamp5.6, add = TRUE)

# cos^-1((n1*n2+e1*e2+up1*up2)/(distance calc through atm for each vector))
# calculate incidence angle
inc_angle_rad <-(acos)(surf_norm_resamp5.6/(plv))
inc_angle_deg <-inc_angle_rad*(180/pi)
plot(inc_angle_deg)
plot(inc_angle_rad)
writeRaster(inc_angle_rad,"/Volumes/JT/projects/uavsar/jemez/lidar_inc_angle.tif", overwrite = TRUE)















