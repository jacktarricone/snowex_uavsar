# quick poster calcs
# dec 8th 2020

library(BiocManager)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)
library(terra)
library(spatialEco)
library(EnvStats)
library(Kendall)
library(remotes)
library(spatstat)
library(hexbin)
library(RColorBrewer)

###########################################
############# calculations testing
###########################################

# max
max_p_value <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_p_value_full.tif")
max_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_slope_full.tif")
max_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_sig_slope.tif")

max_mean<-cellStats(max_sig_slope, stat='mean', na.rm=TRUE)
hist(max_sig_slope)


plot(max_slope)
values(max_sig_slope)[values(max_sig_slope) == 0] = NA
hist(max_slope)
freq(max_sig_slope)

full_slope<-as.data.frame(freq(max_slope))
sig_slope<-as.data.frame(freq(max_sig_slope))

number_sig <-sum(sig_slope$count) - 32680987 - 4472756
total <-sum(full_slope$count) - 32585991
percent_sig<-(number_sig/total)*100

pos <-filter(sig_slope, value > 0)
num_pos <-sum(pos$count)
num_pos/number_sig*100

max_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max/mk_results/max_sig_slope.tif")


# sdd
sdd_p_value <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_p_value_full.tif")
sdd_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_slope_full.tif")
sdd_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_sig_slope.tif")
sdd_sig_ele <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sig_ele.tif")


crs(sdd_sig_slope)<-"+proj=leac +ellps=clrk66"
crs(sdd_sig_ele)<-"+proj=leac +ellps=clrk66"

plot(sdd_slope)
values(sdd_slope)[values(sdd_slope) == 0.000] = NA
hist(sdd_slope)
freq(sdd_slope)

full_slope<-as.data.frame(freq(sdd_slope, digits = 3))
sig_slope<-as.data.frame(freq(sdd_sig_slope))

sdd_mean<-cellStats(sdd_sig_slope, stat='mean', na.rm=TRUE)

number_sig <-sum(sig_slope$count) - 37235246
total <-sum(full_slope$count) - 33703397
percent_sig<-(number_sig/total)*100

sdd_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_sig_slope.tif")


####### sdd

# convert raster values to dataframe
sdd_sig_points <- as.data.frame(rasterToPoints(sdd_sig_slope)) 
sdd_ele_points <-as.data.frame(rasterToPoints(sdd_sig_ele))

# bind together the sig_slope with sig_ele
points <-cbind(sig_points, ele_points$sig_ele)

# rename columns
names(points_no_0)[3] <- "slope"
names(points_no_0)[4] <- "ele"

# filter out 0 ele
points_no_0 <-filter(points, ele >= 1)

# filter out 0 slope
points_no_0 <-filter(points_no_0, slope != 0)

# plot in bins density of points
ggplot(points_no_0, aes(slope, ele))+
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_light()

#same thing but hex plot
ggplot(points_no_0, aes(slope, ele))+
  geom_hex(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  theme_light()


# scf
scf_p_value <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value_full.tif")
scf_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope_full.tif")
plot(scf_slope)
values(scf_slope)[values(scf_slope) == 0.000] = NA
hist(scf_slope)
freq(scf_slope)

sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_pval.tif")
sig_ele <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_ele.tif")
sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_slope.tif")
crs(sig_pval)<-"+proj=leac +ellps=clrk66"
crs(sig_ele)<-"+proj=leac +ellps=clrk66"
crs(sig_slope)<-"+proj=leac +ellps=clrk66"

freq(sig_ele)
freq(sig_slope, digits =5)

full_slope<-as.data.frame(freq(scf_slope, digits = 3))
sig_slope_<-as.data.frame(freq(sig_slope, digits = 3))


scf_mean<-cellStats(sig_slope, stat='mean', na.rm=TRUE)
scf_mean*100

scf_stack <-stack(sig_pval, sig_ele, sig_slope)
scf_stack*100

num_slopes <-sum(full_slope$count) - 33002982
num_sig_slopes <-sum(sig_slope$count) - 32681194
positive_sig_slope <- 62
percent_pos <-(positive_sig_slope/num_sig_slopes)*100
100-percent_pos

plot(scf_stack)


values(sig_ele)[values(sig_ele) <= 0] = NA
hist(sig_ele)


values(sig_slope)[values(sig_slope) == 0.000] = NA
hist(sig_slope)
freq(sig_slope, digits = 3)

plot(values(sig_slope), values(sig_ele))
freq(sig_slope, digits = 3)


####### scf

# convert raster values to dataframe
sig_points <- as.data.frame(rasterToPoints(sig_slope)) 
ele_points <-as.data.frame(rasterToPoints(sig_ele))

# bind together the sig_slope with sig_ele
points <-cbind(sig_points, ele_points$sig_ele)

# rename columns
names(points_no_0)[3] <- "slope"
names(points_no_0)[4] <- "ele"

# filter out 0 ele
points_no_0 <-filter(points, ele >= 1)

# filter out 0 slope
points_no_0 <-filter(points_no_0, slope != 0)

# plot in bins density of points
ggplot(points_no_0, aes(slope, ele))+
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_light()

#same thing but hex plot
ggplot(points_no_0, aes(slope, ele))+
  geom_hex(bins = 27) +
  scale_fill_gradient(low = "gray90", high = "darkred")+
  theme_light()

# contours
ggplot(points_no_0, aes(slope, ele)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white")+
  theme_light()

# contours another way
ggplot(points_no_0, aes(slope, ele)) +
  geom_density_2d_filled(alpha = 1)



?stat_density_2d()






# read in static rasters to stack for calculations
static_stack <-stack("/Volumes/jt/projects/margulis/static/rasters/static_stack.tif")
crs(static_stack)<-"+proj=leac +ellps=clrk66"
dem <-static_stack[[3]]
plot(scf_slope)
values(dem)[values(dem) >= 1500] = 1
plot(dem)
freq(dem)
static_stack