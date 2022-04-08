# compute VG incoming solar for third panel of dfsca/dswe plot
# jack tarricone
# april 17th, 2022

library(raster)
library(terra)
library(insol)

# bring in lidar dem with raster not terra
# switched back to using the raster package bc cgrad can injest only rasters not SpatRasters!
# vg_dem_raw <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem.tif")
# 
# # bring in coherence to resample to
# cor <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/cor_feb12-19.tif")
# 
# # resample up to UAVSAR resolution of 5.6
# vg_5.6 <-resample(vg_dem_raw, cor)
# plot(vg_5.6)
# 
# # bring in aoi shape file
# vg_aoi <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")
# 
# # crop to vector extent
# vg_dem <-crop(vg_5.6, ext(vg_aoi))
# plot(vg_dem, col = terrain.colors(3000)) # test plot
# writeRaster(vg_dem, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem_latlon_5.6.tif")


####################################
####################################
####################################

# bring in raster
vg_dem <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem_latlon_5.6.tif")

## Calculate insolation on for Valle Grand, feb 26th
# convert from SpatRast to raster for calc
dem_raster <-raster(vg_dem) 
grad_mat <-cgrad(dem_raster, cArea = FALSE) # compute normal vector for dem a 1m cell cize

total_daily_solar <-function(day_of_study){
  
# make julian day for
tmz <- -7 # time zone
year <- 2020
month <- 2
day <- day_of_study
timeh <- 0
jd <- JDymd(year,month,day,hour=timeh)

# lat lon of VG
lat <- 35.86534
lon <- -106.47022

# compute day length on day 1
sun_timing <-daylength(lat,lon,jd,tmz)
sunrise <-sun_timing[1]
sunset <-sun_timing[2]
print(sun_timing)

# time interval
deltat <-.5 # [hours]

# create arrays for da loop
nrow <-nrow(dem_raster)
ncol <-ncol(dem_raster)
nlay <-length(seq(sunrise,sunset,deltat)) # number of layers given by deltat

# defening the two empty arras to loop into
Iglobal <-array(0,dim=dim(dem_raster))
Iglobal_v2 <-array(numeric(),c(nrow,ncol,nlay))

## define variables for insolation function
height <-2600 # height asl [m]
visibility <-90 # [km], guess
RH <-50 #2/12 @ 11am
tempK <- 278 # [kelvin]
ozone_thickness <-.03 #[cm]
landscape_alebdo <-.5 

for (i in 1:nlay){
  hours <-seq(sun_timing[1],sun_timing[2],deltat) # sequence by half hour from sunrise to sunset
  layers <-seq(1,nlay,1) # vector to iterate through for layer creatation
  jd = JDymd(year,month,day,hour=hours[i]) # jd value for precise half hour time step
  sv = sunvector(jd,lat,lon,tmz) # sun vector based of position, day, and timezone
  hsh = as.array(hillshading(grad_mat,sv),dim=dim(dem_raster)) # create hillshade
  sh = doshade(dem_raster,sv) # shade
  zenith = sunpos(sv)[2] # calculate solar zenith angle
  
  ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
  # half hour timestep
  # values in J/m^2
  Idirdif = insolation(zenith,
                       jd,
                       height,
                       visibility,
                       RH,tempK,
                       ozone_thickness,
                       landscape_alebdo)
  
  # loop each half hour time step into array by layer
  Iglobal_v2[,,layers[i]] <- Iglobal[,,] + (Idirdif[1,1] * hsh + Idirdif[1,2])*3600*deltat
}

# convert array to rast
solar_rad_hourly_j <-rast(Iglobal_v2)
crs(solar_rad_hourly_j) <-crs(vg_dem) # set crs from orginal dem
ext(solar_rad_hourly_j) <-ext(vg_dem) # set ext 

# sum layers pixel-wise for total daily in Joules/m^2
solar_rad_total_j <-app(solar_rad_hourly_j, sum)

# convert to kilowatt-horus/m^2
solar_rad_total_kwh <-solar_rad_total_j/3.6e6

# return daily total in kWh/m^2 insolation
return(solar_rad_total_kwh)
}

# create sequence of days to calculate
days_list <-as.list(seq(12,26,1))

# apply function to list of days
solar_list <-lapply(days_list, total_daily_solar)


###########################
# test plot, changes daily!
plot(solar_list[[15]])
plot(solar_list[[1]])

# create raster stack from list
solar_stack <-rast(solar_list)
solar_stack # inspect

# take the mean of feb 12-26
avg_solar_kwh_feb12_26 <-app(solar_stack, mean)
plot(avg_solar_kwh_feb12_26)

# save
writeRaster(avg_solar_kwh_feb12_26, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/avg_solar_kwh_feb12_26_v2.tif")




