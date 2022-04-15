library(terra)

# vg extent
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# cumlative swe change
cum_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_cum.tif")
cum <-crop(cum_r,vg)
plot(cum)

# pair 1
feb12_19_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_feb12-19_same_pixels.tif")
feb12_19 <-crop(feb12_19_r, vg)
plot(feb12_19)

# pair 2
feb19_26_r <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/final_swe_change/dswe_feb19-26_same_pixels.tif")
feb19_26 <-crop(feb19_26_r, vg)
plot(feb19_26)

# convert to data frames for plotting
p1_df <-as.data.frame(feb12_19, xy = TRUE, cells = TRUE, na.rm = TRUE)
p2_df <-as.data.frame(feb19_26, xy = TRUE, cells = TRUE, na.rm = TRUE)
cum_df <-as.data.frame(cum, xy = TRUE, cells = TRUE, na.rm = TRUE)


# plot
# theme_set(theme_classic(base_size = 11)) 
ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_histogram(cum_df, mapping = aes(x=lyr.1, fill = "Feb 12-26th", color = "Feb 12-26th"), alpha=0.4, bins = 80) +
  geom_histogram(p1_df, mapping = aes(x=lyr.1, fill = "Feb 12-19th", color = "Feb 12-19th"), alpha=0.4, bins = 80) +
  geom_histogram(p2_df, mapping = aes(x=lyr.1, fill = "Feb 19-26th", color = "Feb 19-26th"), alpha=0.4, bins = 80) +
  scale_colour_manual(name = "Date Range",
                      labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                      values=c("firebrick","goldenrod","black"))+
  scale_fill_manual(name = "Date Range",
                    labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                    values = c("firebrick","goldenrod","black"))+
  xlim(c(-2,1)) + xlab("dSWE [cm]")

# density plot
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(cum_df, mapping = aes(x=lyr.1, y=stat(count),fill = "Feb 12-26th", color = "Feb 12-26th"), alpha=0.1) +
  geom_density(p1_df, mapping = aes(x=lyr.1, y=stat(count),fill = "Feb 12-19th", color = "Feb 12-19th"), alpha=0.1) +
  geom_density(p2_df, mapping = aes(x=lyr.1, y=stat(count),fill = "Feb 19-26th", color = "Feb 19-26th"), alpha=0.1) +
  scale_colour_manual(name = "InSAR Dates",
                      labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                      values=c("firebrick","goldenrod","black"))+
  scale_fill_manual(name = "InSAR Dates",
                    labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th"),
                    values = c("firebrick","goldenrod","black"))+
  xlim(c(-2,1)) + xlab("dSWE [cm]")+ylab("Count")+
  scale_y_continuous(labels=fancy_scientific) +
  theme(legend.position = c(.22,.75))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave("dswe_hist.png",
       width = 5, 
       height = 3,
       units = "in",
       dpi = 300)

# global(cum, mean, na.rm = TRUE)
# global(feb12_19, mean, na.rm = TRUE)
# global(feb19_26, mean, na.rm = TRUE)
  