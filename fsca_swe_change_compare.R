# jack tarricone
# fsca loss vs cum swe change

library(terra)
library(ggplot2)
library(sf)
theme_set(theme_light(11)) # set theme

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files()

# read in fsca files
fsca_0218 <-rast("landsat_fsca_2-18v2.tif")
fsca_0305 <-rast("landsat_fsca_3-05v2.tif")

# calc change 
d_fsca_raw <-fsca_0305 - fsca_0218 
plot(d_fsca_raw)

# read in cum swe
d_swe <-rast("delta_swe_cum.tif")
plot(d_swe)

# crop to d_swe
fsca_crop <-crop(d_fsca_raw, ext(d_swe))
d_fsca <-mask(fsca_crop, d_swe)
plot(d_fsca)

# save
# writeRaster(fsca_crop, "fsca_change_0218_0305.tif")
# writeRaster(d_fsca, "mask_fsca_change_0218_0305.tif")

#####
# compare change in SWE to change in fsca

# mask out fsca values that stayed constant
# no change, therefore not good for comparison

fsca_no0 <-d_fsca # make copy
values(fsca_no0)[values(fsca_no0) == 0] <- NA 
plot(fsca_no0)

# mask swe change raster to new fsca one
mask_d_swe <-mask(d_swe, fsca_no0, maskvalue = NA)
plot(mask_d_swe)

# read in jemez extent
jemez_wkt <-read_sf("/Users/jacktarricone/ch1_jemez_data/vector_data/jemez_ext.geojson")

# crop fsca and swe down
mask_d_swe_crop <-crop(mask_d_swe, jemez_wkt)
fsca_no0_crop <-crop(fsca_no0, jemez_wkt)
plot(mask_d_swe_crop)
plot(fsca_no0_crop)

# convert rasters to dataframe
swe_df <-as.data.frame(mask_d_swe_crop, xy = TRUE, cells = TRUE, na.rm = TRUE)
fsca_df <-as.data.frame(fsca_no0_crop, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(swe_df)
head(fsca_df)

# bind the data frame from plotting
df <-cbind(swe_df, fsca_df[,4])
head(df)
colnames(df)[4] <- "d_swe_cm" # rename col 4
colnames(df)[5] <- "d_fsca_percent" # rename col 5
head(df)

# quick hist
hist(df$d_swe_cm, breaks = 100)
hist(df$d_fsca_percent, breaks = 100)

## scatter
ggplot(df, aes(x = d_fsca_percent, y = d_swe_cm)) +
  geom_point(alpha = .1) +
  #scale_fill_gradient(low = "white", high = "darkred")+
  #geom_hex(bins = 100) +
  labs(title = "fSCA vs delta SWE",
       x = Delta~"fSCA [%]",
       y = Delta~"SWE [cm]")
