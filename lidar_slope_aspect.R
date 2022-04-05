library(terra)

# bring in DEM
lidar_dem <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/valles_elev_filt.img")

# calculate slope and save
lidar_slope <-terrain(lidar_dem, v="slope", neighbors = 8, unit = "degrees")
plot(lidar_slope)
writeRaster(lidar_slope, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/lidar_slope.tif")

# aspect
lidar_aspect <-terrain(lidar_dem, v="aspect", neighbors = 8, unit = "degrees")
plot(lidar_aspect)
writeRaster(lidar_aspect, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/lidar_aspect.tif")

# vg aoi
## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# slope
slope_mask <-mask(lidar_slope, vg)
plot(slope_mask)
writeRaster(slope_mask, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_slope.tif")

# aspect
aspect_mask <-mask(lidar_aspect, vg)
plot(aspect_mask)
writeRaster(aspect_mask, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_aspect.tif")

