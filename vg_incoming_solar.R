# compute VG incoming solar for third panel of dfsca/dswe plot
# jack tarricone
# april 17th, 2022

library(raster)
library(terra)
library(insol)

# bring in lidar dem with raster not terra
# switched back to using the raster package bc cgrad can injest only rasters not SpatRasters!
vg_dem_raw <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem.tif")

# bring in coherence to resample to
cor <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/cor_feb12-19.tif")

# resample up to UAVSAR resolution of 5.6
vg_5.6 <-resample(vg_dem_raw, cor)
plot(vg_5.6)

# bring in aoi shape file
vg_aoi <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# crop to vector extent
vg_dem <-crop(vg_5.6, ext(vg_aoi))
plot(vg_dem, col = terrain.colors(3000)) # test plot
# writeRaster(vg_dem, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem_latlon_5.6.tif")

# bring in raster
vg_dem <-rast("/Users/jacktarricone/ch1_jemez_data/jemez_lidar/vg_dem_latlon_5.6.tif")

## Calculate insolation on for Valle Grand, feb 26th
# convert from SpatRast to raster for calc
dem_raster <-raster(vg_dem) 
grad_mat <-cgrad(dem_raster, cArea = FALSE) # compute normal vector for dem a 1m cell cize

## define variables for insolation function
height <-2600 # height asl [m]
visibility <-90 # [km]
RH <-50 #2/12 @ 11am
tempK <- 278 # temp in kelvin

# make julian day
tmz <- -7 # time zone
year <- 2020
month <- 2
day <- 26
timeh <- 0
jd <- JDymd(year,month,day,hour=timeh)

# lat lon of VG
lat <- 35.86534
lon <- -106.47022

# compute day length on day 1
day_one <-daylength(lat,lon,jd,tmz)
print(day_one)
deltat <-1

# create arrays for da loop
nrow <-nrow(dem_raster)
ncol <-ncol(dem_raster)
nlay <-length(seq(day_one[1],day_one[2],deltat))

Iglobal <-array(0,dim=dim(dem_raster))
Iglobal_v2 <-array(numeric(),c(nrow,ncol,nlay))


for (i in 1:nlay){
  hours <-seq(day_one[1],day_one[2],deltat)
  layers <-seq(1,12,1)
  jd = JDymd(year,month,day,hour=hours[1])
  sv = sunvector(jd,lat,lon,tmz)
  hsh = as.array(hillshading(grad_mat,sv),dim=dim(dem_raster))
  sh = doshade(dem_raster,sv)
  zenith = sunpos(sv)[2]
  Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.03,0.60)
  ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
  ## values in J/m^2
  Iglobal_v2[,,layers[1]] <- Iglobal[,,] + (Idirdif[1,1] * hsh + Idirdif[1,2])*3600*deltat
}


library(terra)
solar_rad_hourly_j <-rast(Iglobal_v2)
crs(solar_rad_hourly_j) <-crs(vg_dem)
ext(solar_rad_hourly_j) <-ext(vg_dem)
plot(solar_rad_hourly_j)

solar_rad_kwh <-solar_rad_hourly_j/3.6e6
plot(solar_rad_kwh)

# writeRaster(solar_rad_MJ, "/Users/jacktarricone/ch1_jemez_data/jemez_lidar/solar_rad_MJ.tif")

Iglobal = raster(Iglobal_v2,crs=projection(vg_dem))
extent(Iglobal) = extent(raster(vg_dem))
plot(Iglobal*1e-6,col=grey(1:100/100),
     legend.args = list(text=expression(paste('Insolation MJ ',m^-2)), side=4,line=2.5))
contour(vg_dem,lwd = 0.5,col='sienna1',add=TRUE,levels=seq(0,2500,500))
contour(vg_dem,lwd = 0.25,col='sienna1',add=TRUE,levels=seq(0,2500,50),drawlabels=FALSE)
unlink(demfile)

# 1 Kw·h = 1 kW x 3600 s = 3.6 MJ = 3.6*1e6 J
# conver kilowatt-hours to megajoules

total_kwh_feb26 <-5.51
total_MJ_feb26 <-total_kwh_feb26*3.6

J_feb12 <-3.65*1e6

demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/demlapalma.tif",demfile)
dem = raster(demfile)
plot(dem)


