library(terra)

# vg extent
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/jemez_ext.geojson")

# cumlative swe change
cum_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_cum.tif")
cum <-crop(cum_r,vg)

# pair 1
feb12_19_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_feb12-19.tif")
feb12_19 <-crop(feb12_19_r, vg)

# pair 2
feb19_26_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_feb19-26.tif")
feb19_26 <-crop(feb19_26_r, vg)

# convert to data frames for plotting
p1_df <-as.data.frame(feb12_19, xy = TRUE, cells = TRUE, na.rm = TRUE)
p2_df <-as.data.frame(feb19_26, xy = TRUE, cells = TRUE, na.rm = TRUE)
cum_df <-as.data.frame(cum, xy = TRUE, cells = TRUE, na.rm = TRUE)


# plot
theme_set(theme_classic(base_size = 11)) 
ggplot()+
  geom_histogram(p2_df, mapping = aes(x=lyr.1), alpha=0.4, fill = "green", color = "green", bins = 100) +
  geom_histogram(p1_df, mapping = aes(x=lyr.1), alpha=0.4, fill = "red", color = "red", bins = 100) +
  geom_histogram(cum_df, mapping = aes(x=lyr.1), alpha=0.4, fill = "black", color = "black", bins = 100) +
  xlim(c(-2,1))

ggplot()+
  geom_density(p2_df, mapping = aes(x=lyr.1), alpha=0.4, color = "green", bins = 100) +
  geom_density(p1_df, mapping = aes(x=lyr.1), alpha=0.4,  color = "red", bins = 100) +
  geom_density(cum_df, mapping = aes(x=lyr.1), alpha=0.4,  color = "black", bins = 100) +
  xlim(c(-2,1)) + xlab("dSWE [cm]")



  