# SWE inversion for 2/12-/219
# path length corrected unwrapped phase data
# lidar incidence angle raster
# use change in HQ met SWE as 0 point
# use data for AGU poster main figure

# jack tarricone
# december 2nd, 2021

library(terra)
library(ggplot2)

# import corrected unwrapped phase data
unw <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/unw_plv_corrected.tif")
crop_ext <-ext(-106.57, -106.38, 35.81, 35.96) # set crop extent
unw
plot(unw)


# import i_angle raster
lidar_inc_raw <-rast("/Volumes/JT/projects/uavsar/jemez/new_inc/lidar_inc_deg.tif")
lidar_inc_raw <-resample(lidar_inc_raw, unw) #resample inc angle up to unw
lidar_inc <-crop(lidar_inc_raw, crop_ext)
plot(lidar_inc)

# mask and crop unw
unw_crop <-mask(unw, lidar_inc_raw)
unw_crop <-crop(unw_crop, lidar_inc)
plot(unw_crop)

#bring in UAVSAR rasters
insar_files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/HH", full.names = TRUE)
insar_files <-insar_files[-5] # delete .int
insar_files <-insar_files[-5] # delete .unw
insar_files

# format UAVSAR files for analysis
cor <-rast(insar_files[[3]])
values(cor)[values(cor) == 0] = NA
cor_crop <-mask(cor, lidar_inc_raw)
cor_crop <-crop(cor, lidar_inc)
plot(cor_crop)

# lidar dem
dem <-rast("/Volumes/JT/projects/uavsar/jemez/lidar/lidar_uavsar_dem_resamp.tif")
dem_crop <-crop(dem, lidar_inc)
plot(dem_crop)

# test plot all the needed data - unw, dem, cor, inc
plot(unw_crop)
plot(dem_crop, add = TRUE)
plot(cor_crop, add = TRUE)
plot(lidar_inc, add = TRUE)


####################################
###### bring in fsca layers ########
####################################

# fsca
fsca <-rast("/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/fsca_final.tif")
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
#writeRaster(snow_mask,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/02_18_2020_snow_mask.tif")

# create no snow mask
no_snow_mask <-fsca_crop
no_snow_mask[is.na(no_snow_mask)] <- -9999 
values(no_snow_mask)[values(no_snow_mask) > 0] = NA
no_snow_mask <- mask(no_snow_mask, cor_crop, maskvalue = NA)
plot(no_snow_mask)
#writeRaster(no_snow_mask,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/02_18_2020_no_snow_mask.tif")


# masked the unwrapped phase with snow mask
unw_snow_mask <- mask(unw_crop, snow_mask, maskvalue = NA)
plot(unw_snow_mask)
plot(unw_crop)
#writeRaster(unw_snow_mask,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/unw_snow_mask.tif")



########################################################
######### converting phase change to SWE ##############
########################################################

# this is just a quick test again, will need to develope
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

density <- .28 # get a real number and do sensitivity analysis
di_elec <- 1.3 # 
wL <- 23.8403545

# first step, define funciton for insar constant
insar_constant <-function(inc, wL, density, di_elec){
  ((-4*pi)/wL)*(cos(inc) - sqrt(di_elec - sin((inc)^2)))
  }

# create the raster
insar_constant_rast <-insar_constant(lidar_inc, wL = 23.8403545, density = .28, di_elec = 1.3)
hist(insar_constant_rast)
plot(insar_constant_rast)

#do swe change calc with masked unwrapped phase data
delta_swe_raw <-insar_constant_rast*unw_crop
plot(delta_swe_raw)
hist(delta_swe_raw, breaks = 100)
writeRaster(delta_swe_raw,"/Volumes/JT/2021_1_fall_UNR/agu/data/delta_swe_raw_new.tif")

# calculating absolute SWE change
# bulk density change of -.7 cm from the 12th to 19th
# relative UAVSAR value was 0.0187817 cm, therefore we're setting to a known change value
# this meathod is up for debate....

delta_swe_abs <-delta_swe_raw - (0.0187817 + .7)
writeRaster(delta_swe_abs,"/Volumes/JT/2021_1_fall_UNR/agu/data/delta_swe_abs_new.tif")
plot(delta_swe_abs)
hist(delta_swe_abs, breaks = 100)

# maske for canopy cover so we get just unforested areas
cc <-rast("/Volumes/JT/projects/uavsar/jemez/nlcd/cc_final.tif")
ext(cc) <-ext(delta_swe_rast)
values(cc)[values(cc) >25] = NA
cc_25_swe_mask <- mask(delta_swe_rast, cc, maskvalue = NA)
plot(cc_25_swe_mask)
#writeRaster(cc_swe_mask, "/Volumes/JT/projects/uavsar/jemez/new_swe_calc/cc_25_swe_mask.tif")













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
