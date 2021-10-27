##### jemez gpr first go
# october 27th, 2021

library(terra)
library(rgdal)
library(sp)
library(rgeos)
library(tidyverse)
library(raster)

# read in DEM
uavsar_dem <-rast("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")
values(uavsar_dem)[values(uavsar_dem) == -10000] <-NA
crs(uavsar_dem)
dswe <-rast("/Volumes/JT/projects/uavsar/jemez/new_swe_calc/delta_abs_pit.tif")

# read in csv
gpr_full <-read.csv("/Users/jacktarricone/Desktop/SNEX20_J_UNM_GPR.csv")
head(gpr_full)
dates <-as.list(unique(gpr_full$date_mmddyy))
print(dates)

# format dates
# gsub(search_term, replacement_term, string_searched)
gpr_full$date_mmddyy <-gsub(dates[[1]],"2019-12-20", gpr_full$date_mmddyy)
gpr_full$date_mmddyy <-gsub(dates[[2]],"2020-02-12", gpr_full$date_mmddyy)
gpr_full$date_mmddyy <-gsub(dates[[3]],"2020-02-20", gpr_full$date_mmddyy)
gpr_full$date_mmddyy <-gsub(dates[[4]],"2020-02-26", gpr_full$date_mmddyy)
gpr_full$date_mmddyy <-gsub(dates[[5]],"2020-03-04", gpr_full$date_mmddyy)

# check
unique(gpr_full$date_mmddyy)
gpr_full$date <-ymd(gpr_full$date_mmddyy)

# filter for one day
gpr_02_12 <-filter(gpr_full, date =="2020-02-12")
args("SpatialPoints")
coords <-as.data.frame(cbind(gpr_02_12$Long, gpr_02_12$Lat, gpr_02_12$SWE_mm))
gpr_points <-SpatialPoints(coords = coords, proj4string = CRS(crs(uavsar_dem)))

# uavsar plot with point
plot(uavsar_dem)
points(gpr_points)

# crop for vis
crop <-c(-106.526, -106.52, 35.855, 35.860)
crop_ext <-ext(crop)
dem_crop <-crop(uavsar_dem, crop_ext)

dswe_crop <-crop(dswe, crop_ext)

plot(dswe_crop, col = hcl.colors(255, palette = "RdBu"))
#plot(dem_crop)
points(gpr_points, pch = 3, cex = 0.005)

data(meuse) 
coordinates(coords) <- coords

spplot(coords, zcol="V3",cuts=8) + as.layer(spplot(dswe_crop))

dswe_crop <-raster(dswe_crop)
spplot(dswe_crop)
spplot(r, scales = list(draw = TRUE), 
       col.regions = terrain.colors(100), at = seq(0, 1, .01)) + 
  layer(sp.polygons(es, lwd = 2))

summary(gpr_02_12$SWE_mm)
summary(gpr_02_12$SWE_mm)
?spplot

geom_sf(
  data = gpr_points, 
  aes(color = SWE_mm), 
  show.legend = 'point'
)

