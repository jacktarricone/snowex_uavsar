library(terra)

# vg aoi
## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# bring in DEM
lidar_dem_raw <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/valles_elev_filt.img")
lidar_dem <-max(lidar_dem_raw)
dem_mask <-mask(lidar_dem, vg)
plot(dem_mask)
writeRaster(dem_mask, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem.tif")


# calculate slope and save
lidar_slope <-terrain(lidar_dem, v="slope", neighbors = 8, unit = "degrees")
plot(lidar_slope)
writeRaster(lidar_slope, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/lidar_slope.tif")

# aspect
lidar_aspect <-terrain(lidar_dem, v="aspect", neighbors = 8, unit = "degrees")
plot(lidar_aspect)
writeRaster(lidar_aspect, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/lidar_aspect.tif")


# slope
slope_mask <-mask(lidar_slope, vg)
plot(slope_mask)
writeRaster(slope_mask, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_slope.tif")

# aspect
aspect_mask <-mask(lidar_aspect, vg)
plot(aspect_mask)
writeRaster(aspect_mask, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_aspect.tif")

