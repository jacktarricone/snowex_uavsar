# jemez mask data bc cant do it with python reader
# 3/22

#geolocating is right, talk to HP about it

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)
library(ggplot2)



# import DEM
jemez_DEM <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")
values(jemez_DEM)[values(jemez_DEM) == -10000] = NA
hist(jemez_DEM)
jemez_DEM

#bring in UAVSAR rasters
files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/HH", full.names = TRUE)
files <-files[-4] # delete .int
files
stack_raw <-stack(files)
stack_raw # inspect

# resample the raster stack to the exact DEM resolution and extent, bc slightly off from so unknown reason (envi?)
# set extent first!
extent(stack_raw) <- extent(jemez_DEM)
stack_raw # check
stack <- resample(stack_raw, jemez_DEM, method='bilinear') #resample to dem resolution
stack #check

#pull out cor and make 0 NA
cor <-stack[[3]]
values(cor)[values(cor) == 0] = NA
plot(cor)
hist(cor)

# mask the stack with .cor
masked_stack <- mask(stack, cor, maskvalue = NA)

# pull out .unw for na convert
unw <-masked_stack[[4]]
values(unw)[values(unw) == 0] = NA
plot(unw)
hist(unw)

# mask .cor raster .unw (takes out area where phase unwrapping algorithm breaks down)
cor_unw <-mask(cor, unw, maskvalue =NA)
plot(cor_unw)
hist(cor_unw)

#########################################################
### cor vs. unw plot
#########################################################

# dem with .unw
dem_unw <-mask(jemez_DEM, unw, maskvalue =NA)
plot(dem_unw)
hist(dem_unw)

# transform .cor and dem into df
unw_points <-as.data.frame(rasterToPoints(unw))
dem_unw_points <-as.data.frame(rasterToPoints(dem_unw))

# rename and bind columns
colnames(dem_unw_points)[3] <- "elevation" #change binded col name
colnames(unw_points)[3] <- "unwrapped_phase" #change binded col name
dem_unw_points <-cbind(dem_unw_points, unw_points$unwrapped_phase)
colnames(dem_unw_points)[4] <- "unwrapped_phase" #change binded col name
head(dem_unw_points)

#fwrite(dem_unw_points, "/Volumes/JT/projects/uavsar/jemez/dem_unw_HH_0212-0219.csv")
#dem_unw_points <-fread("/Volumes/JT/projects/uavsar/jemez/dem_unw_HH_0212-0219.csv")

#plot
theme_set(theme_light(base_size =11))
dem_unw_plot <-ggplot(dem_unw_points, aes(elevation, unwrapped_phase)) +
  geom_hex(bins = 35) +
  #scale_fill_viridis(option="magma")+
  scale_fill_gradient(low = "grey96", high = "darkred") +
  labs(title = "Jemez River Elevation vs. Unwrapped Phase 2/12-2/19 HH",
       x = "Elevation (m)",
       y = "Unwrapped Phase (radians)")
print(dem_unw_plot)


setwd("/Volumes/JT/projects/uavsar/jemez/scatter_plots/")
ggsave(dem_unw_plot,
       file = "dem_unw_hex.png",
       width = 6, 
       height = 4,
       dpi = 400)

#########################################################
### cor vs. dem plot
#########################################################


# transform .cor and dem into df
masked_dem <-mask(jemez_DEM, cor, maskvalue = NA) # crop to slightly smaller .cor
cor_points <-as.data.frame(rasterToPoints(cor))
dem_points <-as.data.frame(rasterToPoints(masked_dem))

# rename and bind columns
colnames(dem_points)[3] <- "elevation" #change binded col name
colnames(cor_points)[3] <- "coherence" #change binded col name
cor_dem_points <-cbind(cor_points, dem_points$elevation)
colnames(cor_dem_points)[4] <- "elevation" #change binded col name

#fwrite(cor_dem_points, "/Volumes/JT/projects/uavsar/jemez/cor_dem_HH_0212-0219.csv")
cor_dem_points <-fread("/Volumes/JT/projects/uavsar/jemez/cor_dem_HH_0212-0219.csv")

#plot
theme_set(theme_light(base_size =11))
p2 <-ggplot(cor_dem_points, aes(elevation, coherence)) +
  geom_hex(bins = 25) +
  #scale_fill_viridis(option="magma")+
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Jemez River Elevation vs. Coherence 2/12-2/19 HH ",
       x = "Elevation (m)",
       y = "Coherence")
print(p2)

ggsave(p2,
       file = "dem_cor_hex.png",
       width = 6, 
       height = 4,
       dpi = 400)

#########################################################
### cor vs. unw plot
#########################################################


# mask cor with unw
masked_cor <-mask(cor, unw, maskvalue = NA) # crop to slightly smaller .cor
plot(masked_cor)

# transform masked_cor in points
masked_cor_points <-as.data.frame(rasterToPoints(masked_cor))
# already have unw from previous work

# rename and bind columns
colnames(masked_cor_points)[3] <- "coherence" #change binded col name
unw_cor_points <-cbind(unw_points, masked_cor_points$coherence)
colnames(unw_cor_points)[4] <- "coherence" #change binded col name

#fwrite(unw_cor_points, "/Volumes/JT/projects/uavsar/jemez/unw_cor_HH_0212-0219.csv")
unw_cor_points <-fread("/Volumes/JT/projects/uavsar/jemez/unw_cor_HH_0212-0219.csv")

#plot
theme_set(theme_light(base_size =11))
p3 <-ggplot(unw_cor_points, aes(coherence, unwrapped_phase)) +
  geom_hex(bins = 25) +
  #scale_fill_viridis(option="magma")+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Jemez River Unwrapped Phase vs. Coherence 2/12-2/19 HH ",
       x = "Coherence",
       y = "Unwrapped Phase (radians)")
print(p3)

ggsave(p3,
       file = "unw_cor_hex.png",
       width = 6, 
       height = 4,
       dpi = 400)

