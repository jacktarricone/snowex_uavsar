# jemez SWE calc 2/12/2020 - 2/19/2020 pair
# jemez_insar_swe_2-12_2-19_HH
# HH
# 9/22
# new version

#geolocating is right, talk to HP about it

library(data.table)
library(gdalUtils)
library(terra)
library(ggplot2)

# import corrected unwrapped phase data
unw_correct <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/unw_plv_corrected.tif")
unw_correct

# import i_angle raster
i_angle_deg_raw <-rast("/Volumes/JT/projects/uavsar/jemez/inc/i_angle_deg.tif")
plot(i_angle_deg_raw)
i_angle_deg_raw

#bring in UAVSAR rasters
insar_files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/HH", full.names = TRUE)
insar_files
insar_files <-insar_files[-5] # delete .int
insar_files <-insar_files[-5] # delete .unw

amp1 <-rast(insar_files[[1]])
values(amp1)[values(amp1) == 0] = NA

amp2 <-rast(insar_files[[2]])
values(amp2)[values(amp2) == 0] = NA

cor <-rast(insar_files[[3]])
values(cor)[values(cor) == 0] = NA

dem <-rast(insar_files[[4]])
values(dem)[values(dem) == -10000] = NA
ext(dem) <-ext(cor)


# resample the raster stack to the exact DEM resolution and extent, bc slightly off from so unknown reason (envi?)
# set extent first!
i_angle_deg <- resample(i_angle_deg_raw, dem, method='bilinear')
ext(i_angle_deg) <-ext(cor)
#writeRaster(i_angle_deg, "/Volumes/JT/projects/uavsar/jemez/swe_calc/i_angle_resmap.tif")

# create stack
rast_list <-list(amp1, amp2, cor, dem, unw_correct, i_angle_deg)
insar_stack <-rast(rast_list)
insar_stack
plot(insar_stack[[5]])

# mask the stack with .cor
masked_stack <- mask(insar_stack, cor, maskvalue = NA)
masked_stack
plot(masked_stack)

####################################
###### bring in fsca layers ########
####################################

# fsca
fsca <-rast("/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/fsca_final.tif")
ext(fsca) <-ext(insar_stack)
plot(fsca)

# create snow mask
snow_mask <-fsca
values(snow_mask)[values(snow_mask) > 1] = 1
plot(snow_mask)
#writeRaster(snow_mask,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/02_18_2020_snow_mask.tif")

# masked the unwrapped phase with snow mask
unw_snow_mask <- mask(unw_correct, snow_mask, maskvalue = NA)
plot(unw_snow_mask)
plot(unw_correct)
#writeRaster(unw_snow_mask,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/unw_snow_mask.tif")



########################################################
######### converting phase change to SWE ##############
########################################################

# this is just a quick test again, will need to devolp
# a method to systematically estimate these numbers
# talk to HP about this

density <- .29 # get a real number and do senativity analysis
di_elec <- 1.4 # 
wL <- 23.8403545

# first step, define funciton for insar constant
insar_constant <-function(inc, wL, density, di_elec){
  ((-4*pi)/wL)*(cos(inc) - sqrt(di_elc - sin((inc)^2)))
  }

# create the raster
insar_constant_rast <-insar_constant(i_angle_deg, wL = 23.8403545, density = .29, di_elec = 1.4)
hist(insar_constant_rast)
plot(insar_constant_rast)

#do swe change calc with masked unwrapped phase data
delta_swe_rast <-insar_constant_rast*unw_snow_mask
plot(delta_swe_rast)
hist(delta_swe_rast, breaks = 100)
#writeRaster(delta_swe_rast,"/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_swe_raster.tif")


# maske for canopy cover so we get just unforested areas
cc <-rast("/Volumes/JT/projects/uavsar/jemez/nlcd/cc_final.tif")
ext(cc) <-ext(delta_swe_rast)
values(cc)[values(cc) > 1] = NA
cc_swe_mask <- mask(delta_swe_rast, cc, maskvalue = NA)
plot(cc_swe_mask)
#writeRaster(cc_swe_mask, "/Volumes/JT/projects/uavsar/jemez/new_swe_calc/cc_swe_mask.tif")


####################################### 
###### find no change point and subtract
#######################################

#import
delta_swe_rast <-raster("/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_raster.tif")

#define no change point phase
no_change_point_phase <-0.268596

#subtract from raster
delta_swe_abs <-delta_swe_rast-no_change_point_phase
plot(delta_swe_abs)

#mask for snow cover
delta_swe_snow_mask_abs <- mask(delta_swe_abs, snow_mask, maskvalue = NA)
plot(delta_swe_snow_mask_abs)
writeRaster(delta_swe_snow_mask_abs,"/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_snow_mask_abs.tif")

delta_swe_snow_mask_abs1 <-raster("/Volumes/JT/projects/uavsar/jemez/swe_calc/2-12_2-19/delta_swe_snow_mask_abs_2-12_2-19_HH.tif")

hist(delta_swe_snow_mask_abs1,
     breaks = 100,
     main= "Jemez Change in SWE 2/12-2/19 HH",
     xlab= "SWE Change (cm)",
     xlim=c(-3,2),
     col= "darkgreen"
)
