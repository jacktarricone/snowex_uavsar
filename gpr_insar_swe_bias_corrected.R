# gpr swe vs insar swe
# june 15th, 2022

# updating my old code with the new feb12-26 bias corrected raster ryan sent me


library(terra)
library(ggplot2)
library(dplyr)

setwd("/Users/jacktarricone/ch1_jemez_data/")

#######
#######
## bring in added phase raster aka 2/12-2/26
i_swe_cum <-rast("./gpr_rasters_ryan/final_swe_change/dswe_cum.tif")
plot(i_swe_cum)

# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12_v1 <-rast("./gpr_swe_bias/feb26_minus_Feb12_bias_corrected1.tif")
gpr_feb26_minus_feb12 <-gpr_feb26_minus_feb12_v1/10 # convert to cm from mm
plot(gpr_feb26_minus_feb12)
hist(gpr_feb26_minus_feb12, breaks = 50)

global(gpr_feb26_minus_feb12,mean,na.rm=T)


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
theme_set(theme_bw(14)) # set theme
ggplot(cm_plotting_df) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylim(c(-10,10)) + xlim(c(-10,10))+
  geom_point(aes(y = dswe_insar, x = dswe_gpr), color = "darkred", alpha = .3) +
  labs(#title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

# lm_df <-cm_plotting_df[-c(1:3)]
# names(lm_df)[1:2] <-c("x","y")
# 
# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }
# 
# p1 <- p + geom_text(x = -5, y = 9, label = lm_eqn(lm_df), parse = TRUE)
# print(p1)

ggsave("/Users/jacktarricone/ch1_jemez_data/plots/dswe_gpr_vs_insar_feb26_12_bias_v5.png",
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
  labs(#title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")

# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/swe_gpr_vs_insar_feb12_26.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)



