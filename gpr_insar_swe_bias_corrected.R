# gpr swe vs insar swe
# june 15th, 2022

# updating my old code with the new feb12-26 bias corrected raster ryan sent me


library(terra)
library(ggplot2); theme_set(theme_classic(12)) # set theme
library(dplyr)

setwd("/Users/jacktarricone/ch1_jemez_data/")

#######
#######
## bring in added phase raster aka 2/12-2/26
i_swe_cum <-rast("./gpr_rasters_ryan/final_swe_change/dswe_cum.tif")
plot(i_swe_cum)

# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12_v1 <-rast("./gpr_swe_bias/feb26_minus_Feb12_bias_corrected1.tif")
plot(gpr_feb26_minus_feb12_v1)
gpr_feb26_minus_feb12 <-gpr_feb26_minus_feb12_v1/10 # convert to cm from mm
plot(gpr_feb26_minus_feb12)

# resample gpr to same grid as unw, crop ext
i_swe_cum_crop <-crop(i_swe_cum, ext(gpr_feb26_minus_feb12)) # crop
gpr_feb26_minus_feb12 # check
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
cm_plotting_df <-cbind(swe_df, gpr_df$feb26_minus_Feb12_bias_corrected1)
head(cm_plotting_df)
colnames(cm_plotting_df)[4] <- "dswe_insar" # rename col 4
colnames(cm_plotting_df)[5] <- "dswe_gpr" # rename col 5
head(cm_plotting_df)

# quick hists
hist(cm_plotting_df$dswe_gpr, breaks = 10)
hist(cm_plotting_df$dswe_insar, breaks = 10)

####
# plotting
####

# scattter
#theme_set(theme_light(11)) # set theme
ggplot(cm_plotting_df) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylim(c(-10,10)) + xlim(c(-10,10))+
  geom_point(aes(y = dswe_insar, x = dswe_gpr), color = "black", alpha = .3) +
  labs(title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")

ggsave("/Users/jacktarricone/ch1_jemez_data/plots/dswe_gpr_vs_insar_feb26_12_bias.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)

# density scatter
ggplot(cm_plotting_df, aes(y = dswe_insar, x = dswe_gpr)) +
  #geom_abline(slope = 1) +
  xlim(c(-2,2)) + ylim(c(-2,2))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour_var = "count")+
  scale_fill_continuous(type = "viridis") +
  labs(title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm",
       y = Delta~"SWE InSAR [cm]]")

# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/swe_gpr_vs_insar_feb12_26.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)


m1 <-lm(d_swe_gpr ~ d_swe_insar, cm_plotting_df)
summary(m1)


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
