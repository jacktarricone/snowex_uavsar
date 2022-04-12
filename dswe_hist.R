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
  geom_density(p2_df, mapping = aes(x=lyr.1, fill = "Feb 12-19th", color = "Feb 12-19th"), alpha=0.4) +
  geom_density(p1_df, mapping = aes(x=lyr.1, fill = "Feb 19-26th", color = "Feb 19-26th"), alpha=0.4) +
  geom_density(cum_df, mapping = aes(x=lyr.1, fill = "Feb 12-26th", color = "Feb 12-26th"), alpha=0.4) +
  scale_colour_manual(name = "Date Range",
                      labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                      values=c("firebrick","goldenrod","black"))+
  scale_fill_manual(name = "Date Range",
                    labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                    values = c("firebrick","goldenrod","black"))+
  xlim(c(-2,1)) + xlab("dSWE [cm]")



  