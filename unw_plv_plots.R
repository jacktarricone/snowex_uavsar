# unwrapped phase vs path length vector plot
# sept 20th

library(terra)
library(tidyverse)
library(data.table)

# path length raster 
plv_km <-rast("/Volumes/JT/projects/uavsar/jemez/look_vector/good_llh_vrt/plv_km.tif")
plv_km

#bring in UAVSAR rasters from the 2/12-2/19 pair
files <-list.files("/Volumes/JT/projects/uavsar/jemez/rasters/02122020_02192020/HH/", pattern = "*.grd.tiff", full.names = TRUE)
files <-files[-4] # delete .int
files
stack_raw <-rast(files)
stack_raw # inspect

## clean rasters
amp1 <-stack_raw[[1]]
values(amp1)[values(amp1) == 0] = NA
plot(amp1)

amp2 <-stack_raw[[2]]
values(amp1)[values(amp1) == 0] = NA
plot(amp1)

# cor
cor <-stack_raw[[3]]
values(cor)[values(cor) == 0] = NA
plot(cor)

# define unw and one with NA's zeroed
unw_raw <-stack_raw[[4]]
unw <-unw_raw
values(unw)[values(unw) == 0] = NA
hist(unw, breaks = 100)

plot(unw)
plot(plv_km, add = TRUE)

### convert rasters to dataframes, rename data columns
unw_df <-as.data.frame(unw, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(unw_df)[4] <- "unwrapped_phase"
head(unw_df)

plv_df <-as.data.frame(plv_km, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(plv_df)[4] <- "plv_km"
head(plv_df)

# plot unw data vs longitude (this should be vs cell in reality need to update)
theme_set(theme_light(base_size =12))
p9 <-ggplot(unw_df, aes(x, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  #stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,6,2))+
  labs(title = "Jemez River Unwrapped Phase 2/12-2/19",
       x = "Longitude Change (deg)",
       y = "Unwrapped Phase (radians)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
print(p9)

#
p10 <-ggplot(plv_df, aes(x, plv_km)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "blue") +
  #stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,6,2))+
  labs(title = "Jemez River Radar Path Length 2/12-2/19",
       x = "Longitude Change (deg)",
       y = "Path Length to Sensor (km)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
print(p10)
