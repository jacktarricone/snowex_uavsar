# create cumulativ swe change raster
# redid swe change code to be proper
# march 7th, 2022

library(terra)

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/")

# read in rasters we just made
dswe_feb19_26 <-rast("dswe_feb19-26.tif")
dswe_feb12_19_v1 <-rast("dswe_feb12-19.tif")

# crop down slightly
dswe_feb12_19 <-crop(dswe_feb12_19_v1, ext(dswe_feb19_26))

pair_1 <-mask(dswe_feb12_19, dswe_feb19_26, maskvalue = NA)
pair_2 <-mask(dswe_feb19_26, pair_1, maskvalue = NA)

# substract
dswe_cum <-pair_1 + pair_2
plot(dswe_cum)

# writeRaster(dswe_cum,"dswe_cum.tif")



############
# vectorize JPL DEM for A map making
#############

jpl <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
jpl_test <-jpl
values(jpl_test)[values(jpl) > 0] <-1
plot(jpl_test)

# convert to vector data
jpl_shp <-as.polygons(jpl, 
                      dissolve = TRUE,
                      multi = TRUE,
                      na.rm = TRUE, 
                      extent = FALSE, crs = crs(jpl_test))

plot(jpl_shp) # test plot

## aggregate polyongs up to just data extent
jpl_shp_v1 <- aggregate(jpl_shp, dissolve = TRUE, fun = "mean",cores = 10)
plot(jpl_shp_v1)

# writeVector(jpl_shp_v1, "/Users/jacktarricone/ch1_jemez_data/vector_data/jpl_dem_extent.shp")

