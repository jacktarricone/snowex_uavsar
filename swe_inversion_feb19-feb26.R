# SWE inversion for 2/19-2/26
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
unw <-rast("unw2_final.tif")
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

####### bring in UAVSAR InSAR data

# cor
cor <-rast("cor_feb12-19.tif")
cor_crop <-mask(cor, lidar_inc_raw)
cor_crop <-crop(cor, lidar_inc)
plot(cor_crop)

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
plot(unw_snow_mask)
plot(unw_crop)

########################################################
######### converting phase change to SWE ##############
########################################################

# this is just a quick test again, will need to develop
# a method to systematically estimate these numbers
# talk to HP about this

###################################################################
###################################################################
###################################################################
# don't pick denisty and di_elec value
# pick a density and LWC (from ryans equations and field measurments)
# vary density and LWC over range over measured values
###################################################################
###################################################################
###################################################################

# table ryan sent over
pit_info <-read.csv("/Users/jacktarricone/ch1_jemez_data/pit_data/perm_pits.csv")

## define static information from pits
# calculate density
mean_density_feb20 <- pit_info$mean_density[2]
mean_density_feb26 <- pit_info$mean_density[3]
mean_density_feb20_26 <-(mean_density_feb12 + mean_density_feb19)/2

# dielctric constant k
k_feb20 <- pit_info$mean_k[2]
k_feb26 <- pit_info$mean_k[3]
mean_k_feb20_26 <-(k_feb20 +k_feb26)/2

# radar wave length from uavsar annotation file
uavsar_wL <- 23.8403545

#######################
#### swe inversion ####
#######################


# first step, define funciton for insar constant
insar_constant <-function(inc, wL, density, k){
  ((-4*pi)/wL)*(cos(inc) - sqrt(k - sin((inc)^2)))
  }

# create the raster
insar_constant_rast <-insar_constant(inc = lidar_inc, wL = uavsar_wL, 
                                     density = mean_k_feb20_26, k = mean_k_feb20_26)
hist(insar_constant_rast)
plot(insar_constant_rast)

#do swe change calc with masked unwrapped phase data
delta_swe_raw <-insar_constant_rast*unw_crop
plot(delta_swe_raw)
hist(delta_swe_raw, breaks = 100)
# writeRaster(delta_swe_raw,"delta_swe_raw.tif")

# calculating absolute SWE change
# bulk density change of -.7 cm from the 12th to 19th
# relative UAVSAR value was 0.0187817 cm, therefore we're setting to a known change value
# this meathod is up for debate....

delta_swe_abs <-delta_swe_raw - (0.0187817 + .7)
writeRaster(delta_swe_abs,"delta_swe_feb12-19v1.tif")
plot(delta_swe_abs)
hist(delta_swe_abs, breaks = 100)

# maske for canopy cover so we get just unforested areas
# cc <-rast("cc_percent.tif")
# ext(cc) <-ext(delta_swe_abs)
# values(cc)[values(cc) >25] = NA
# cc_25_swe_mask <- mask(delta_swe_abs, cc, maskvalue = NA)
# plot(cc_25_swe_mask)
#writeRaster(cc_swe_mask, "/Volumes/JT/projects/uavsar/jemez/new_swe_calc/cc_25_swe_mask.tif")


list.files()










####################################### 
###### find no change point and subtract
#######################################

#import untheather raster in
delta_swe_raw <-rast("/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_swe_raw.tif")


#############################################################
############  define no change point phase ##################
#############################################################

# lets do this by taking all the areas which are not covered in snow
# and computing a histogram to see the range of phase values
# in theory, there should be no phase change 
#### things that could effect phase: changes in soil moisture, snow in canopy, wind deposition??

# masked unwrapped phase with no snow mask
unw_no_snow <-mask(unw_correct, no_snow_mask, maskvalue = NA)
plot(unw_no_snow)

################# fix this by using GG
hist(unw_no_snow, 
     main = "Jemez No Snow Cover Unwrapped Phase 2/12-2/19",
     xlab = "Phase (radians)", 
     xlim = c(-3,3),
     ylab = "Frequency",
     breaks = 100
     )
#writeRaster(unw_no_snow, "/Volumes/JT/projects/uavsar/jemez/new_swe_calc/unw_no_snow.tif")

# get some stats on the no snow phase
# 
global(unw_no_snow, "mean", na.rm = TRUE)
global(unw_no_snow, "max", na.rm = TRUE)
global(unw_no_snow, "min", na.rm = TRUE)
global(unw_no_snow, "rms", na.rm = TRUE)
global(unw_no_snow, "sd", na.rm = TRUE)
 
# test using mean
no_change_point_phase <-as.numeric(global(unw_no_snow, "mean", na.rm = TRUE))

#subtract from raster
delta_swe_abs <-delta_swe_rast-(0.0187817+.7)
plot(delta_swe_abs)

#write absolute swe value
HHHHHHHwriteRaster(delta_swe_abs,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_abs_pit_test.tif")

delta_swe_abs_no_cc <-mask(delta_swe_abs, cc_swe_mask, maskvalue = NA)
plot(delta_swe_abs_no_cc)

delta_swe_abs_cc_25 <-mask(delta_swe_abs, cc_25_swe_mask, maskvalue = NA)
plot(delta_swe_abs_cc_25)

delta_swe_abs_cc_25_pit <-mask(delta_swe_abs, cc_25_swe_mask, maskvalue = NA)
plot(delta_swe_abs_cc_25_pit)


#write absolute swe value with no canopy cover
writeRaster(delta_swe_abs_no_cc,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_swe_abs_no_cc.tif")
writeRaster(delta_swe_abs_cc_25,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_swe_abs_cc_25.tif")
writeRaster(delta_swe_abs_cc_25_pit,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_swe_abs_cc_25_pit.tif")


delta_swe_snow_mask_abs1 <-raster("/Volumes/JT/projects/uavsar/jemez/swe_calc/2-12_2-19/delta_swe_snow_mask_abs_2-12_2-19_HH.tif")

hist(delta_swe_snow_mask_abs1,
     breaks = 100,
     main= "Jemez Change in SWE 2/12-2/19 HH",
     xlab= "SWE Change (cm)",
     xlim=c(-3,2),
     col= "darkgreen"
)
