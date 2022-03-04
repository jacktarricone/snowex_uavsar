# fsca transformation 
# march 3rd
# comparing change in fsca to delta swe
# using native 30m

library(terra)
library(sf)

#bring in DEM
dem <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dem_feb12-19.tif")
dem

# feb 18th 2020
# bringg in raw fSCA feb 18th data downloaded from landsat web portal
fsca_0218_raw <-rast("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/feb_18/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif")
fsca_0218_raw <-fsca_0218_raw/10 # correct [%] scale
fsca_0218_raw # check
plot(fsca_0218_raw) # test plot

# march 5th 2020
# bringg in raw fSCA feb 18th data downloaded from landsat web portal
fsca_0305_raw <-rast("/Users/jacktarricone/ch1_jemez_data/landsat_fsca/fsca_mar_05/fsac_mar_05.tif")
fsca_0305_raw <-fsca_0305_raw/10 # correct [%] scale
fsca_0305_raw # check
plot(fsca_0305_raw) # test plot

# create delta fsca product
dfsca <- fsca_0305_raw - fsca_0218_raw
plot(dfsca)

# reproject to lat/lon
dfsca <- project(dfsca, "EPSG:4326")
plot(dfsca)

#bring in DEM
dswe_cum <-rast("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/delta_swe_cum.tif")
dswe_cum
plot(dswe_cum)

# crop to extent of SWE data
dfsca_crop <-crop(dfsca, ext(dswe_cum))
plot(dfsca_crop)
# writeRaster(dfsca_crop, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dfsca_30m.tif")

# resample SWE data up to 30m landsat
?resample
dswe_cum30m <-resample(dswe_cum, dfsca_crop, method = "bilinear")
dswe_cum30m
plot(dswe_cum30m)
# writeRaster(dswe_cum30m, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dswe_cum30m.tif")


# mask for missing pixels in SWE data
dfsca_crop_mask <-mask(dfsca_crop, dswe_cum30m)
plot(dfsca_crop_mask)

# check both files
dswe_cum30m
dfsca_crop_mask


# mask for non 0 fsca pixels
dfsca_no0 <-dfsca_crop_mask
values(dfsca_no0)[values(dfsca_no0) == 0] <- NA
plot(dfsca_no0)

# mask the swe raster
dswe_no0 <-mask(dswe_cum30m, dfsca_no0, maskvalue = NA)
dswe_no0
# values(dswe_no0)[values(dswe_no0) > -2] <- NA
# dfsca_no0_v1 <-mask(dfsca_no0, dswe_no0)
plot(dswe_no0)

####### now we can plot

# read in jemez valle grande extent
jemez_wkt <-read_sf("/Users/jacktarricone/ch1_jemez_data/vector_data/jemez_ext.geojson")

# crop fsca and swe down to just VG
dswe_vg <-crop(dswe_no0, jemez_wkt)
dfsca_vg <-crop(dfsca_no0, jemez_wkt)

# test plot, stupid color scale....
# my.palette1 <- RColorBrewer::brewer.pal(n = 11, name = "RdBu")
plot(dswe_vg)
plot(dfsca_vg)

# convert rasters to dataframe
swe_df <-as.data.frame(dswe_vg, xy = TRUE, cells = TRUE, na.rm = TRUE)
fsca_df <-as.data.frame(dfsca_vg, xy = TRUE, cells = TRUE, na.rm = TRUE)
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
  #xlim(c(-100,50)) + ylim(c(-3,.5))+
  # scale_fill_gradient(low = "grey90", high = "darkred")+
  #geom_hex(bins = 50)+
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  labs(title = "fSCA vs delta SWE",
       x = Delta~"fSCA [%]",
       y = Delta~"SWE [cm]")


