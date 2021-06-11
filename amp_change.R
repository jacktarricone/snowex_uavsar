# jemez amp change

library(gridExtra)
library(data.table)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)
library(raster)
library(zoo)

### first pair

#bring in UAVSAR rasters
files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/HH", full.names = TRUE)
files <-files[-4] # delete .int
files
stack_raw1 <-stack(files)
stack_raw1 # inspect


## amp1 
amp1 <-stack_raw1[[1]]
values(amp1)[values(amp1) == 0] = NA
plot(amp1)
hist(amp1)

# convert to db
amp1_db <-10*log10(amp1)
plot(amp1_db)
hist(amp1_db)
writeRaster(amp1_db, "/Volumes/JT/projects/uavsar/jemez/amp/amp1_db.tif")

## amp 2

amp2 <-stack_raw1[[2]]
values(amp1)[values(amp2) == 0] = NA
plot(amp2)
hist(amp2)

# convert to db
amp2_db <-10*log10(amp2)
plot(amp2_db)
hist(amp2_db)
writeRaster(amp2_db, "/Volumes/JT/projects/uavsar/jemez/amp/amp2_db.tif")

amp_db_diff <-amp1_db-amp2_db
plot(amp_db_diff)
hist(amp_db_diff)


writeRaster(amp_db_diff, "/Volumes/JT/projects/uavsar/jemez/amp/amp_db_diff_0212_0219.tif")



##### second flight


#bring in UAVSAR rasters
files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02192020_02262020/HH", full.names = TRUE)
files <-files[-4] # delete .int
files
stack_raw2 <-stack(files)
stack_raw2 # inspect

# amp 2b
amp2b <-stack_raw2[[1]]
values(amp2b)[values(amp2b) == 0] = NA
plot(amp2b)
hist(amp2b)

# convert to db
amp2b_db <-10*log10(amp2b)
plot(amp2b_db)
hist(amp2b_db)

writeRaster(amp2b_db, "/Volumes/JT/projects/uavsar/jemez/amp/amp2b_db.tif")

# amp 3
amp3 <-stack_raw2[[2]]
values(amp3)[values(amp3) == 0] = NA
plot(amp3)
hist(amp3)

# convert to db
amp3_db <-10*log10(amp3)
plot(amp3_db)
hist(amp3_db)

writeRaster(amp3_db, "/Volumes/JT/projects/uavsar/jemez/amp3_db.tif")


amp_db_diff2 <-amp2b_db - amp3_db
plot(amp_db_diff2)
hist(amp_db_diff2)

writeRaster(amp_db_diff2, "/Volumes/JT/projects/uavsar/jemez/amp_db_diff_0219_0226.tif")




