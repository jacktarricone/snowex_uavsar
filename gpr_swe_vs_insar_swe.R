# gpr swe vs insar swe
# jack tarricone
# february 3rd 2022

################
## NEED TO UPDATE WITH PROPER SWE RASTERS
## AVERAGE ALL PIXELS AROUND SNOWPIT TO GENERATE NO CHANGE POINT!!

library(terra)
library(ggplot2); theme_set(theme_classic(12)) # set theme
library(dplyr)

# bring in gpr
gpr_swe_feb12_20 <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/ryan_gpr_swe/feb20_minus_feb121.tif")
plot(gpr_swe_feb12_20)
gpr_swe_feb12_20

# bring in unwrapped phase
i_swe_feb12_19 <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/delta_swe_feb12-19_v1.tif")
plot(i_swe_feb12_19)

# resample gpr to same grid as unw
gprv1 <-resample(gpr_swe_feb12_20, i_swe_feb12_19, method = "bilinear")
gprv1

# crop both down to just gpr extent
gpr <-crop(gprv1, ext(gpr_swe_feb12_20))
i_swe_crop <-crop(i_swe_feb12_19, ext(gpr_swe_feb12_20))
plot(i_swe_crop)
plot(gpr, add = TRUE, col = "red")

# mask both layers so they have the same number of pixels
i_swe_mask <-mask(i_swe_crop, gpr, maskvalues = NA)
gpr_mask <-mask(gpr, i_swe_mask, maskvalues = NA)

# test plot 
plot(i_swe_mask)
plot(gpr_mask, add = TRUE, col = "red")

# convert raster to dataframe
gpr_df <-as.data.frame(gpr_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
unw_df <-as.data.frame(i_swe_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(unw_df)

# bind the data frames
plotting_df <-full_join(gpr_df, unw_df)
colnames(plotting_df)[5] <- "unw" # rename col 5
head(plotting_df)

# test plot
ggplot(plotting_df) +
  geom_point(aes(y = unw, x = feb20_minus_feb121))


# # bring in 19-26th pair
# unw_feb19_26 <-rast(" /Users/jacktarricone/ch1_jemez_data/feb19-26v2/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_02.unw.grd")
# values(unw_feb19_26)[values(unw_feb19_26) == 0] <-NA
# plot(unw_feb19_26)
# 
# ##### resample and mask
# # resample 19-26 to the 12-19 grid, slightly off for some reason
# unw2_resamp <-resample(unw_feb19_26, unw_feb12_19, method = "bilinear") 
# unw1_final <-mask(unw_feb12_19, unw2_resamp, maskvalues = NA) # mask for only same pixels
# 
# # test plot of both pairs
# plot(unw1_final)
# plot(unw2_resamp, add = TRUE)
# 
# # remask to sites will have the exact same pixels
# unw2_final <-mask(unw2_resamp, unw1_final, maskvalues = NA)
# plot(unw1_final) # test
# plot(unw2_final, add = TRUE)
# 
# # cmulative phase
# unw_cm <-unw1_final+unw2_final

# save rasters
# writeRaster(unw1_final, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw1_final.tif")
# writeRaster(unw2_final, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw2_final.tif")
# writeRaster(unw_cm, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/unw_cm.tif")


#######
#######
## bring in added phase raster aka 2/12-2/26
i_swe_cum <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/delta_swe_cum.tif")
plot(i_swe_cum)

# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12 <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/ryan_gpr_swe/feb26_minus_feb121.tif")
plot(gpr_feb26_minus_feb12)

# resample gpr to same grid as unw, crop ext
i_swe_cum_crop <-crop(i_swe_cum, ext(gpr_feb26_minus_feb12)) # crop
gpr_feb26_minus_feb12 
i_swe_cum_crop # check

# test plot
plot(i_swe_cum_crop)
plot(gpr_feb26_minus_feb12, add = TRUE, col = "red")

# mask unw data with gpr
i_swe_cum_crop_mask <-mask(i_swe_cum_crop, gpr_feb26_minus_feb12, maskvalue = NA)
f26_m_12_mask <-mask(gpr_feb26_minus_feb12, i_swe_cum_crop_mask, maskvalue = NA)

# plot only pixels that have data for both gpr and unw
plot(i_swe_cum_crop_mask)
plot(f26_m_12_mask, add = TRUE, col = hcl.colors(12, "Berlin"))

# convert raster to dataframe
swe_df <-as.data.frame(i_swe_cum_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
gpr_df <-as.data.frame(f26_m_12_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(gpr_df)
head(swe_df)

# bind the data frames
cm_plotting_df <-cbind(swe_df, gpr_df$feb26_minus_feb121)
head(cm_plotting_df)
colnames(cm_plotting_df)[4] <- "d_swe_insar" # rename col 4
colnames(cm_plotting_df)[5] <- "d_swe_gpr" # rename col 5
head(cm_plotting_df)

# test plot
theme_set(theme_light(11)) # set theme

ggplot(cm_plotting_df) +
  #geom_abline(slope = 1) +
  geom_point(aes(y = d_swe_insar, x = d_swe_gpr), color = "black", alpha = .4) +
  xlim(-4,2) +  ylim(-3,2) +
  labs(title = "Delta SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~" SWE InSAR [cm]",
       y = Delta~"SWE GPR [cm]")

# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/swe_gpr_vs_insar_feb12_26.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)
####

cm_lm <-lm(unw ~ gpr_twt_change_ns, cm_plotting_df)
summary(cm_lm)

ggplot(cm_plotting_df, aes(y = unw, x = gpr_twt_change_ns)) +
  geom_point()+
  geom_abline(slope = coef(cm_lm)[["gpr_twt_change_ns"]], 
              intercept = coef(cm_lm)[["(Intercept)"]])

#### testing putting things on same scale
cm_test <-cm_plotting_df

# check min
cm_test$unw_norm <-cm_test$unw-min(cm_test$unw)
cm_test$gpr_norm <-cm_test$gpr_twt_change_ns-min(cm_test$gpr_twt_change_ns)
head(cm_test)

ggplot(cm_test) +
  geom_point(aes(y = unw_norm, x = gpr_norm))

# add lm
cm_lm_norm <-lm(unw_norm ~ gpr_norm, cm_test)
summary(cm_lm_norm)

ggplot(cm_test, aes(y = unw_norm, x = gpr_norm)) +
  geom_point()+
  geom_abline(slope = coef(cm_lm_norm)[["gpr_norm"]], 
              intercept = coef(cm_lm_norm)[["(Intercept)"]])
