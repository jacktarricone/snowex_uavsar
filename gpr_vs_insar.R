# gpr vs insar comparison start
# jack tarricone
# november 17th, 2021

library(terra)
library(sf)
library(sp)
library(rgdal)
library(dplyr)
 

### read in the ROUGH (is not an absolute change yet) SWE data
d_swe <-rast("/Volumes/JT/2021_1_fall_UNR/agu/data/delta_swe_abs_new.tif")
plot(d_swe) # test plot

##### read in the three days of gpr data

# feb 12
feb12_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb12_GPR_ver2.2.csv")
feb12 <-vect(feb12_csv, geom = c("Long","Lat"), crs = crs(d_swe))

# feb 20
feb20_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb20_GPR_ver2.csv")
feb20 <-vect(feb20_csv, geom = c("Long","Lat"), crs = crs(d_swe))

# feb 26
feb26_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb26_GPR_ver2.csv")
feb26 <-vect(feb26_csv, geom = c("Long","Lat"), crs = crs(d_swe))


# crop down to area around the GPR
vg <-ext(-106.5255, -106.521, 35.856, 35.8594)
swe_crop <-crop(d_swe, vg)
plot(swe_crop)
# writeRaster(swe_crop, "/Volumes/JT/projects/uavsar/jemez/swe_inversion_lidar/swe_vg_crop.tif")

# plot
plot(swe_crop)
points(feb12, cex = .1)
points(feb20, col = "red", cex = .1)
points(feb26, col = "green", cex = .1)

#### mask SWE values for feb12 
# feb 12
feb12_gpr <-mask(swe_crop, feb12)
plot(feb12_gpr)
points(feb12, cex = .1)
hist(feb12_gpr, breaks = 100)

# feb 20
feb20_gpr <-mask(swe_crop, feb20)
plot(feb20_gpr)
points(feb20, col = "red", cex = .1)
hist(feb20_gpr, breaks = 100)

#feb 26
feb26_gpr <-mask(swe_crop, feb26)
plot(feb26_gpr)
points(feb26, col = "green", cex = .1)
hist(feb26_gpr, breaks = 100)

# create columns for cell numbers for each date
# feb 12
cell_numbers_feb12 <-cellFromXY(feb12_gpr, cbind(feb12_csv$Long, feb12_csv$Lat))
feb12_csv <-cbind(feb12_csv, cell_numbers_feb12)
head(feb12_csv)
feb12_cells <-cells(feb12_gpr)

bp <- ggplot(feb12_cells, aes(x=cell_numbers, y=SWE_mm, group=cell_numbers)) + 
  geom_boxplot(aes(fill=dose))
bp

# feb 20
cell_numbers_feb20 <-cellFromXY(feb20_gpr, cbind(feb20_csv$Long, feb20_csv$Lat))
feb20_csv <-cbind(feb20_csv, cell_numbers_feb20)
head(feb20_csv)
feb20_cells <-cells(feb20_gpr)

# feb 26
cell_numbers_feb26 <-cellFromXY(feb26_gpr, cbind(feb26_csv$Long, feb26_csv$Lat))
feb26_csv <-cbind(feb26_csv, cell_numbers_feb26)
head(feb26_csv)
feb26_cells <-cells(feb26_gpr)


###### testing plotting by cell number


swe_matix <-matrix(swe_crop, nrow = 62)
all_cells_not_na <-cells(swe_crop)

plot(swe_crop)
ncell(swe_crop)
nsrc(swe_crop)
nrow(swe_crop)
ncol(swe_crop)



?extract
test <-extract(gpr_only_pixels, 
               feb12)

test <-as.data.frame(cellFromXY(gpr_only_pixels, cbind(feb12_csv$Long,feb12_csv$Lat)))


