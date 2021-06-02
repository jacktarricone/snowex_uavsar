# jemez SWE calc 2/12/2020 - 2/19/2020 pair 
# jemez_insar_swe_2-12_2-19_VH
# VH
# 4/28

#geolocating is right, talk to HP about it

library(plotly)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)
library(raster)

{
# import DEM
jemez_DEM <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090VH_01.hgt.grd.tiff")
values(jemez_DEM)[values(jemez_DEM) == -10000] = NA
hist(jemez_DEM)
jemez_DEM

# import i_angle raster
i_angle_deg_raw <-raster("/Volumes/JT/projects/uavsar/jemez/inc/i_angle_deg.tif")
plot(i_angle_deg_raw)
i_angle_deg_raw

#bring in UAVSAR rasters
files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/VH", full.names = TRUE)
files <-files[-4] # delete .int
files
stack_raw <-stack(files)
stack_raw # inspect

# resample the raster stack to the exact DEM resolution and extent, bc slightly off from so unknown reason (envi?)
# set extent first!
extent(stack_raw) <- extent(jemez_DEM)
stack_raw # check
stack <- resample(stack_raw, jemez_DEM, method='bilinear') #resample to dem resolution
i_angle_deg <- resample(i_angle_deg_raw, jemez_DEM, method='bilinear')

stack #check
i_angle_deg
#writeRaster(i_angle_deg, "/Volumes/JT/projects/uavsar/jemez/swe_calc/i_angle_resmap.tif")

#pull out cor and make 0 NA
cor <-stack[[3]]
values(cor)[values(cor) == 0] = NA
plot(cor)
hist(cor)

# mask the stack with .cor
masked_stack <- mask(stack, cor, maskvalue = NA)
masked_stack
plot(masked_stack)

####################################

# define unw and one with NA's zeroed
unw <-masked_stack[[4]]
unw_zero_na <-unw
values(unw_zero_na)[values(unw_zero_na) == 0] = NA
plot(unw_zero_na)

# bring in fsca layers
# fsca
fsca <-raster("/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/fsca_final.tif")
plot(fsca)
hist(fsca)
#hmmt<-freq(fsca, digits = 0)

# create snow mask
snow_mask <-fsca
values(snow_mask)[values(snow_mask) > 1] = 1
#writeRaster(snow_mask,"/Volumes/JT/projects/uavsar/jemez/fsca/02_18_2020/snow_mask.tif")


############################
### correct for atm delay
###########################

plot(unw)
plot(unw_zero_na)
plot(snow_mask)
unw_snow_mask <- mask(unw_zero_na, snow_mask, maskvalue = NA)
plot(unw_snow_mask)
#writeRaster(unw_snow_mask,"/Volumes/JT/projects/uavsar/jemez/unw_snow_mask.tif")

# extract lat lon information for multiplication
lon <- unw_zero_na
lat <- unw_zero_na
xy <- coordinates(unw_zero_na)
lon[] <- xy[, 1]
lat[] <- xy[, 2]
plot(lon)
plot(lat)
#writeRaster(lat,"/Volumes/JT/projects/uavsar/jemez/swe_calc/unw_lat.tif")
#writeRaster(lon,"/Volumes/JT/projects/uavsar/jemez/swe_calc/unw_lon.tif")

# correct for slope using best fit equation

#### lm results
# (Intercept)           x 
# -2115.1265    -19.8767 

slope_correct <-function(unw, lon){return((unw - ((lon * -19.8767) - 2115.1265)))}
unw_corrected <-slope_correct(unw_zero_na, lon)
plot(unw_corrected)
#writeRaster(unw_corrected,"/Volumes/JT/projects/uavsar/jemez/swe_calc/unw_corrected.tif")

# test hist
hist(unw_corrected)
hist(unw_zero_na)

# convert to points for graph
unw_corrected_points <-as.data.frame(rasterToPoints(unw_corrected))
head(unw_corrected_points)
colnames(unw_corrected_points)[3] <- "unwrapped_phase"

# plot corrected data
theme_set(theme_light(base_size =12))
p9 <-ggplot(unw_corrected_points, aes(x, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  #stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,6,2))+
  labs(title = "Jemez River Unwrapped Phase vs. Longitude Corrected VH 2/12-2/19",
       x = "Longitude Change (deg)",
       y = "Unwrapped Phase (radians)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
print(p9)

setwd("/Volumes/JT/projects/uavsar/jemez/swe_calc/")
ggsave(p9,
       file = "unw_corrected_graph_2-12_2-19_VH.png",
       width = 6, 
       height = 4,
       dpi = 400)



########################################################
### calculating 
########################################################

density <- .29 # get a real number and do senativity analysis
di_elc <- 1.4 # 
wL <- 23.8403545

# first step
insar_constant <-function(inc){((-4*pi)/wL)*(cos(inc) - sqrt(di_elc - sin((inc)^2)))}
insar_constant_rast <-insar_constant(i_angle_deg)
hist(insar_constant_rast)
plot(insar_constant_rast)

#do swe change calc
delta_swe_rast <-insar_constant_rast*unw_corrected
plot(delta_swe_rast)
hist(delta_swe_rast, breaks = 100)
writeRaster(delta_swe_rast,"/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_raster_2-12_2-19_VH.tif")

#mask for snow
delta_swe_snow_mask <- mask(delta_swe_rast, snow_mask, maskvalue = NA)
plot(delta_swe_snow_mask)
writeRaster(delta_swe_snow_mask,"/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_snow_mask_2-12_2-19_VH.tif")
hist(delta_swe_snow_mask)
#freq(delta_swe_snow_mask, digits =0)

cc <-raster("/Volumes/JT/projects/uavsar/jemez/nlcd/cc_final.tif")
hist(cc)
values(cc)[values(cc) > 1] = NA
cc_swe_mask <- mask(delta_swe_snow_mask, cc, maskvalue = NA)
plot(cc_swe_mask)
writeRaster(cc_swe_mask, "/Volumes/JT/projects/uavsar/jemez/swe_calc/cc_swe_mask_2-12_2-19_VH.tif")


####################################### 
###### find no change point and subtract
#######################################

#import
delta_swe_rast <-raster("/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_raster_2-12_2-19_VH.tif")

#define no change point phase
no_change_point_phase <-0.268596

#subtract from raster
delta_swe_abs <-delta_swe_rast-no_change_point_phase
plot(delta_swe_abs)

#mask for snow cover
delta_swe_snow_mask_abs <- mask(delta_swe_abs, snow_mask, maskvalue = NA)
plot(delta_swe_snow_mask_abs)
writeRaster(delta_swe_snow_mask_abs,"/Volumes/JT/projects/uavsar/jemez/swe_calc/delta_swe_snow_mask_abs_2-12_2-19_VH.tif")
}
