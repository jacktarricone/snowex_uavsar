#ENVI
#description = {/Volumes/JT/projects/uavsar/jemez/raw_data/02192020_02262020/HH}
#samples = 4631
#lines = 6315
#bands = 1
#header offset = 0
#data type = 4
#interleave = bsq
#sensor type = UAVSAR L-Band
#byte order = 0
#map info = {Geographic Lat/Lon, 
 # 1.000, 
  #1.000, 
  #-106.565357880000008, 
  #36.064996080000000  ,  
  #0.0000555600000000, 
  #0.0000555600000000, 
  #WGS-84, units=Degrees}
#wavelength units = Unknown

options(digits = 22)  
?options

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
lon <-as.numeric(specify_decimal(-106.565357880000008, 15))


pixel_size <- 0.0000555600000000
lat <- 36.064996080000000
lat <-format(round(lat, 15), nsmall = 15)
lat <-as.numeric(lat)
lon <- -106.565357880000008
hp <-pixel_size/2
lon <-format(round(lon, 15), nsmall = 15) 
lon <-as.numeric(lon)

print(-106.565357881111118, digits = 15)

tlc_lat <-lat + hp
tlc_lat
tlc_lat - lat
tlc_lon <-lon - hp
tlc_lon
tlc_lon - lon
