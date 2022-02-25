# SWE inversion for 2/12-2/19


# path length corrected unwrapped phase data
# lidar incidence angle raster
# use change in HQ met SWE as 0 point

# jack tarricone
# feb23, 2021

library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files() #pwd

# import corrected unwrapped phase data
unw <-rast("unw1_final.tif")
unw
plot(unw)

# import i_angle raster
lidar_inc_raw <-rast("lidar_inc_deg.tif")

# resample inc angle up to unw
lidar_inc_raw <-resample(lidar_inc_raw, unw)
plot(lidar_inc_raw)

# set crop extent and crop for better visualization
crop_ext <-ext(-106.57, -106.38, 35.81, 35.96) 
lidar_inc <-crop(lidar_inc_raw, crop_ext)
plot(lidar_inc)

# mask and crop unwrapped phase data down to extent on new inc angle raster
unw_crop <-mask(unw, lidar_inc_raw)
unw_crop <-crop(unw_crop, lidar_inc)
plot(unw_crop)



####################################
###### bring in fsca layers ########
####################################

# fsca
fsca <-rast("landsat_fsca_2-18.tif")
ext(fsca) <-ext(unw)
plot(fsca)

# crop to inc 
fsca_crop <-mask(fsca, lidar_inc_raw)
fsca_crop <-crop(fsca_crop, lidar_inc)
plot(fsca_crop)
fsca_crop

# create snow mask
snow_mask <-fsca_crop
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

# FROM QGIS
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

