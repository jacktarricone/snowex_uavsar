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

# pacakages in r or python for calculating gradients and surface normals

## October 25th 2021
# updated january 27th

library(terra)

# bring in filtered dem, bare earth no strees
ele_filt <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/valles_elev_filt.img")
ele_filt <-max(ele_filt)
plot(ele_filt, col = terrain.colors(3000))

# generate slope image from filtered DEM
slope_deg <-terrain(ele_filt, v="slope", unit="degrees", neighbors=8)
plot(slope_deg)
# writeRaster(slope_deg, "/Volumes/JT/projects/uavsar/jemez/lidar/jemez_lidar_slope_deg.tif")

# generate aspect image from filtered DEM
aspect_deg <-terrain(ele_filt, v="aspect", unit="degrees", neighbors=8)
plot(aspect_deg)
# writeRaster(aspect_deg, "/Volumes/JT/projects/uavsar/jemez/lidar/jemez_lidar_aspect_deg.tif")

## 
uavsar_dem <-rast("dem_feb12-19.tif")
uavsar_dem

ele_latlon <-resample(ele_filt, uavsar_dem, method = "bilinear")

plot(uavsar_dem, col = terrain.colors(1500))
plot(ele_latlon, col = terrain.colors(1500), add = TRUE)
writeRaster(ele_latlon, "/Volumes/JT/projects/uavsar/jemez/lidar/lidar_uavsar_resamp.tif")
?plot
