### creating new incidence angle raster from CZO LiDAR data
# will be used for SWE inversion of UAVSAR data

## steps
# 1.create slope, aspect rasters from filtered DEM
# 2.reproject and resample lidar data products to UAVSAR projection (wsg-84, lat/lon)
# 3.use these resampled products to create new .inc file

## October 25th 2021

library(terra)
library(rgdal)

# bring in filtered dem, bare earth no strees
ele_filt <-rast("/Volumes/JT/projects/uavsar/jemez/lidar/ele_filt/GISdata/JemezRiver/lidar/snow_off/valles_elev_filt.img")
ele_filt <-max(ele_filt)
ele_filt
plot(ele_filt, col = terrain.colors(1500))

# bring in unfiltered dem, has vegetatoin
ele_unfilt <-rast("/Volumes/JT/projects/uavsar/jemez/lidar/ele_unfilt/GISdata/JemezRiver/lidar/snow_off/valles_elev_unfilt.img")
ele_unfilt
hist()
plot(ele_unfilt,col = terrain.colors(1500))

# generate slope image from filtered DEM
slope_deg <-terrain(ele_filt, v="slope", unit="degrees", neighbors=8)
plot(slope_deg)
# writeRaster(slope_deg, "/Volumes/JT/projects/uavsar/jemez/lidar/jemez_lidar_slope_deg.tif")

# generate aspect image from filtered DEM
aspect_deg <-terrain(ele_filt, v="aspect", unit="degrees", neighbors=8)
plot(aspect_deg)
# writeRaster(aspect_deg, "/Volumes/JT/projects/uavsar/jemez/lidar/jemez_lidar_aspect_deg.tif")

## 
uavsar_dem <-rast("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")





