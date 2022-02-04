# gpr twt vs unwrapped phase scatter plots
# jack tarricone
# february 3rd 2022

library(terra)
library(ggplot2)
library(dplyr)

# bring in gpr
gpr_feb12_20 <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/Rasters_for_Jack/feb20_minus_feb121.tif")
plot(gpr_feb12_20)

# bring in unwrapped phase
unw_feb12_19 <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_corrected_feb12-19.tif")
plot(unw_feb12_19)

# resample gpr to same grid as unw
gprv1 <-resample(gpr_feb12_20, unw_feb12_19, method = "bilinear")

# crop both down to just gpr extent
gpr <-crop(gprv1, ext(gpr_feb12_20))
unw_crop <-crop(unw_feb12_19, ext(gpr_feb12_20))
plot(unw_crop)
plot(gpr, add = TRUE)

# mask both layers so they have the same number of pixels
unw_mask <-mask(unw_crop, gpr, maskvalues = NA)
gpr_mask <-mask(gpr, unw_mask, maskvalues = NA)

# test plot 
plot(unw_gprmask)
plot(gpr_mask, add = TRUE)

# convert raster to dataframe
gpr_df <-as.data.frame(gpr_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
unw_df <-as.data.frame(unw_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(unw_df)

# bind the data frames
plotting_df <-full_join(gpr_df, unw_df)
colnames(plotting_df)[5] <- "unw" # rename col 5
head(plotting_df)

# test plot
theme_set(theme_light(11)) # set theme
ggplot(plotting_df) +
  geom_point(aes(y = unw, x = feb20_minus_feb121))


