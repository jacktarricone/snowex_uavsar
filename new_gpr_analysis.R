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
plot(unw_mask)
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


# bring in 19-26th pair
unw_feb19_26 <-rast(" /Users/jacktarricone/ch1_jemez_data/feb19-26v2/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_02.unw.grd")
values(unw_feb19_26)[values(unw_feb19_26) == 0] <-NA
plot(unw_feb19_26)

##### resample and mask
# resample 19-26 to the 12-19 grid, slightly off for some reason
unw2_resamp <-resample(unw_feb19_26, unw_feb12_19, method = "bilinear") 
unw1_final <-mask(unw_feb12_19, unw2_resamp, maskvalues = NA) # mask for only same pixels

# test plot of both pairs
plot(unw1_final)
plot(unw2_resamp, add = TRUE)

# remask to sites will have the exact same pixels
unw2_final <-mask(unw2_resamp, unw1_final, maskvalues = NA)
plot(unw1_final) # test
plot(unw2_final, add = TRUE)

# cumulative phase
unw_cum <-unw1_final+unw2_final

# save rasters
# writeRaster(unw1_final, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw1_final.tif")
# writeRaster(unw2_final, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw2_final.tif")
# writeRaster(unw_cum, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_cum.tif")


#######
#######
## bring in added phase raster aka 2/12-2/26
# unw_cum <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_cum.tif")

# bring in 2/12-2/26 gpr rdata