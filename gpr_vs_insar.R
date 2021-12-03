# gpr vs insar comparison start
# jack tarricone
# november 17th, 2021

library(terra)
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
 

### read in the ROUGH (is not an absolute change yet) SWE data
d_swe <-rast("/Volumes/JT/2021_1_fall_UNR/agu/data/delta_swe_abs_new.tif")
plot(d_swe) # test plot

##### read in the three days of gpr data

# feb 12
feb12_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb12_GPR_ver2.2.csv")
feb12_csv <-mutate(feb12_csv, SWE_cm = SWE_mm*(1/10)) # add cm col
feb12 <-vect(feb12_csv, geom = c("Long","Lat"), crs = crs(d_swe))

# feb 20
feb20_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb20_GPR_ver2.csv")
feb20_csv <-mutate(feb20_csv, SWE_cm = SWE_mm*(1/10))
feb20 <-vect(feb20_csv, geom = c("Long","Lat"), crs = crs(d_swe))

# feb 26
feb26_csv <-read.csv("/Volumes/JT/projects/uavsar/jemez/gpr/SnowEx_Jemez_Feb26_GPR_ver2.csv")
feb26_csv <-mutate(feb26_csv, SWE_cm = SWE_mm*(1/10))
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
cell_numbers_feb12 <-as.integer(cellFromXY(feb12_gpr, cbind(feb12_csv$Long, feb12_csv$Lat)))
uavsar_swe12 <-extract(feb12_gpr, cell_numbers_feb12, cells = TRUE, xy = TRUE)
feb12_csv <-cbind(feb12_csv, cell_numbers_feb12, uavsar_swe12)
names(feb12_csv)[15]<- "uavsar_d_swe_cm"
head(feb12_csv)
#write.table(feb12_csv, "/Volumes/JT/2021_1_fall_UNR/agu/data/feb12_cells.csv")

# change cell number to 1 - how many ever there or for plotting purposes
# feb12_cells <-as.integer(cells(feb12_gpr))
# cell_seq <-as.integer(seq(1,220,1))
# new_cell_numbers <-as.data.frame(cell_numbers_feb12)

# for (i in 1:length(cell_seq)){
#   new_cell_numbers[new_cell_numbers == feb12_cells[i]] <- cell_seq[i]
# }

# tail(feb12_cells)
# names(new_cell_numbers)[1]<-"cell_number"
# hist(new_cell_numbers$cell_number)

# feb 20
cell_numbers_feb20 <-cellFromXY(feb20_gpr, cbind(feb20_csv$Long, feb20_csv$Lat))
uavsar_swe20 <-extract(feb12_gpr, cell_numbers_feb20, cells = TRUE, xy = TRUE)
feb20_csv <-cbind(feb20_csv, cell_numbers_feb20, uavsar_swe20)
names(feb20_csv)[15]<- "uavsar_d_swe_cm"
head(feb20_csv)
#write.table(feb20_csv, "/Volumes/JT/2021_1_fall_UNR/agu/data/feb20_cells.csv")
feb20_cells <-cells(feb20_gpr)

# feb 26
cell_numbers_feb26 <-cellFromXY(feb26_gpr, cbind(feb26_csv$Long, feb26_csv$Lat))
uavsar_swe26 <-extract(feb12_gpr, cell_numbers_feb26, cells = TRUE, xy = TRUE)
feb26_csv <-cbind(feb26_csv, cell_numbers_feb26, uavsar_swe26)
names(feb26_csv)[15]<- "uavsar_d_swe_cm"
head(feb26_csv)
#write.table(feb26_csv, "/Volumes/JT/2021_1_fall_UNR/agu/data/feb26_cells.csv")
feb26_cells <-cells(feb26_gpr)

##########################################################################
###### create new data frames for plotting UAVSAR and GPR together #######
##########################################################################

# summarize from feb 12 all the gpr data by cell number
head(feb12_csv)
feb12_plotting <-feb12_csv%>%
              group_by(cell_numbers_feb12)%>% 
              summarise(feb12_gpr_median_swe=median(SWE_cm, na.rm = TRUE),
                   feb12_gpr_QR1 = quantile(SWE_cm, c(0.25), na.rm = TRUE),
                   feb12_gpr_QR3 = quantile(SWE_cm, c(0.75), na.rm = TRUE),
                   feb12_gpr_mean_swe = mean(SWE_cm), 
                   feb12_gpr_max_swe = max(SWE_cm), 
                   feb12_gpr_min_swe = min(SWE_cm),
                   feb12_gpr_std_swe = sd(SWE_cm), 
                   uavsar_d_swe_cm = max(uavsar_d_swe_cm))
# write.csv(feb12_plotting, "/Volumes/JT/2021_1_fall_UNR/agu/data/feb12_plotting.csv", row.names = FALSE)


# summarize from feb 20 all the gpr data by cell number
feb20_plotting <-feb20_csv%>%
  group_by(cell_numbers_feb20)%>% 
  summarise(feb20_gpr_median_swe=median(SWE_cm, na.rm = TRUE),
            feb20_gpr_QR1 = quantile(SWE_cm, c(0.25), na.rm = TRUE),
            feb20_gpr_QR3 = quantile(SWE_cm, c(0.75), na.rm = TRUE),
            feb20_gpr_mean_swe = mean(SWE_cm), 
            feb20_gpr_max_swe = max(SWE_cm), 
            feb20_gpr_min_swe = min(SWE_cm),
            feb20_gpr_std_swe = sd(SWE_cm))
# write.csv(feb20_plotting, "/Volumes/JT/2021_1_fall_UNR/agu/data/feb20_plotting.csv", row.names = FALSE)

# filter for only the cell numbers that apear in both data frames
feb12_same_cells <-filter(feb12_plotting, cell_numbers_feb12 %in% feb20_plotting$cell_numbers_feb20)
feb20_same_cells <-filter(feb20_plotting, cell_numbers_feb20 %in% feb12_plotting$cell_numbers_feb12)

# stitch back together and save
first_flight_gpr_cells <-cbind(feb12_same_cells, feb20_same_cells)
head(first_flight_gpr_cells)
# write.csv(first_flight_gpr_cells, "/Volumes/JT/2021_1_fall_UNR/agu/data/first_flight_gpr_cells.csv", row.names = FALSE)

##############################################
###### testing plotting by cell number #######
##############################################

mini_feb12 <-filter(feb12_csv, cell_numbers_feb12 == 2096)
mini_feb20 <-filter(feb20_csv, cell_numbers_feb20 == 2096)
mini_feb26 <-filter(feb26_csv, cell_numbers_feb26 == 2096)

theme_set(theme_light(18))
p2 <-ggplot() + 
  geom_pointrange(feb12_csv, mapping = aes(x = cell_numbers_feb12, y = SWE_cm, group = cell_numbers_feb12),
                  stat = "summary",
                  fun.min = function(z) { quantile(z,0.25) },
                  fun.max = function(z) { quantile(z,0.75) },
                  fun = median,
                  size = .2,
                  color = "firebrick")+
  geom_pointrange(feb20_csv, mapping = aes(x = cell_numbers_feb20, y = SWE_cm, group = cell_numbers_feb20),
                  stat = "summary",
                  fun.min = function(z) { quantile(z,0.25) },
                  fun.max = function(z) { quantile(z,0.75) },
                  fun = median,
                  size = .2,
                  color = "black")+
  geom_pointrange(feb26_csv, mapping = aes(x = cell_numbers_feb26, y = SWE_cm, group = cell_numbers_feb26),
                  stat = "summary",
                  fun.min = function(z) { quantile(z,0.25) },
                  fun.max = function(z) { quantile(z,0.75) },
                  fun = median,
                  size = .2,
                  color = "green")

ggsave(p2,
       file = "/Volumes/JT/2021_1_fall_UNR/agu/data/gpr_swe_v2.png",
       width = 11, 
       height = 4,
       dpi = 400)










