# SWE inversion for 2/12-2/19


# path length corrected unwrapped phase data
# lidar incidence angle raster
# use change in HQ met SWE as 0 point

# jack tarricone
# feb23, 2021

# update march 7th using pit location!

library(terra)
library(ggplot2)
library(sf)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files() #pwd

# import corrected unwrapped phase data
unw_raw <-rast("unw1_final.tif")
unw_raw
plot(unw_raw)

# import i_angle raster
lidar_inc_raw <-rast("lidar_inc_deg.tif")
lidar_inc_v1 <-resample(lidar_inc_raw, unw_raw)

# set crop extent and crop for better visualization
crop_ext <-ext(-106.57, -106.38, 35.81, 35.96) 
lidar_inc <-crop(lidar_inc_v1, crop_ext)

# mask and crop unwrapped phase data down to extent on new inc angle raster
unw_crop <-crop(unw_raw, crop_ext)
unw <-mask(unw_crop, lidar_inc)

# test
plot(lidar_inc)
plot(unw, add = T)

####################################
###### bring in fsca layers ########
####################################

# fsca
fsca_raw <-rast("landsat_fsca_2-18.tif")
fsca_crop <-crop(fsca_raw, ext(lidar_inc))

# crop to inc 
fsca <-mask(fsca_crop, lidar_inc)
plot(fsca)

# create snow mask
snow_mask <-fsca
values(snow_mask)[values(snow_mask) > 1] = 1
plot(snow_mask)
# writeRaster(snow_mask,"02_18_2020_snow_mask.tif")

# masked the unwrapped phase with snow mask
unw_snow_mask <- mask(unw_crop, snow_mask, maskvalue = NA)
plot(unw_crop)
plot(unw_snow_mask)

########################################################
######### converting phase change to SWE ##############
########################################################

# this is just a quick test again, will need to develop
# a method to systematically estimate these numbers
# talk to HP about this

#######################################################################
# don't pick denisty and di_elec value
# pick a density and LWC (from ryans equations and field measurements)
# vary density and LWC over range over measured values
########################################################################

# table ryan sent over
pit_info <-read.csv("/Users/jacktarricone/ch1_jemez_data/pit_data/perm_pits.csv")

## define static information from pits
# calculate density
mean_density_feb12 <- pit_info$mean_density[1]
mean_density_feb20 <- pit_info$mean_density[2]
mean_density_feb12_20 <-(mean_density_feb12 + mean_density_feb20)/2

# dielctric constant k
k_feb12 <- pit_info$mean_k[1]
k_feb20 <- pit_info$mean_k[2]
mean_k_feb12_20 <-(k_feb12+k_feb20)/2

# radar wave length from uavsar annotation file
uavsar_wL <- 23.8403545



#######################
#### swe inversion ####
#######################


# first step, define function for insar constant
insar_constant <-function(inc, wL, density, k){
  ((-4*pi)/wL)*(cos(inc) - sqrt(k - sin((inc)^2)))
  }

# create the raster
insar_constant_rast <-insar_constant(inc = lidar_inc, wL = uavsar_wL, 
                                     density = mean_density_feb12_20, k = mean_k_feb12_20)
hist(insar_constant_rast)
plot(insar_constant_rast)

#do swe change calc with masked unwrapped phase data
delta_swe_raw <-insar_constant_rast*unw_crop
plot(delta_swe_raw)
hist(delta_swe_raw, breaks = 100)
writeRaster(delta_swe_raw,"raw_delta_swe_feb12_19.tif")


#######################################
### calculating absolute SWE change ###
#######################################

# using swe change from the pit as "known" change point
# this meathod is up for debate....

# extent around gpr transect
gpr <-ext(-106.5255, -106.521, 35.856, 35.8594)
unw_gpr <-crop(unw, gpr)
plot(unw_gpr)

# pull out location info into separate df
loc <-data.frame(lat = pit_info$lat[1],
                 lon = pit_info$lon[1])

# create sf object
pit_point <-vect(loc, geom = c("lon","lat"), crs = crs(unw))
points(pit_point, cex = 1)

# extract one cell test
unw_pit <-terra::extract(unw, pit_point,  cells = TRUE, xy = TRUE)
unw_pit

# extract phase values of 8 neighboring cells
neighbor_cells <-c(adjacent(unw, cells = 6174974, directions ="queen"))
test <-unw_pit <-terra::extract(unw, neighbor_cells,  cells = TRUE, xy = TRUE)
head(test)





?adjacent

pit_phase_value <- 0.0541057 ### GET REAL PIT LOCATION FROM RYAN
delta_swe_abs <-delta_swe_raw - pit_phase_value
plot(delta_swe_abs)
hist(delta_swe_abs, breaks = 100)

# save
# writeRaster(delta_swe_abs,"delta_swe_feb12-19_v1.tif")

# create cumlative swe change raster, teathering numbers up for debate!!

# read in rasters we just made
delta_swe_feb19_26 <-rast("delta_swe_feb19-26_v2.tif")
delta_swe_feb12_19 <-rast("delta_swe_feb12-19_v1.tif")

# substract
delta_swe_cum <-delta_swe_feb12_19 + delta_swe_feb19_26
plot(delta_swe_cum)
writeRaster(delta_swe_cum,"delta_swe_cum.tif")

