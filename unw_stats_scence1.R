library(terra)

# calculate phase statistics
# 2/12-2/19

# set working dir
setwd("/Users/jacktarricone/ch1_jemez_data/feb12-19")

#  calculate the total number of pixels from the coherence image
hh_list <-list.files(pattern = "*HH_01.cor*")
hh_cor <-rast(hh_list[1])
values(hh_cor)[values(hh_cor) == 0] <-NA
fr <-as.data.frame(freq(hh_cor, digits = 3))
total_pixels <-sum(fr$count)


# hh
hh_list <-list.files(pattern = "*HH_01.unw*")
hh <-rast(hh_list[1])
values(hh)[values(hh) == 0] <-NA
plot(hh)
hh_freq <-as.data.frame(freq(hh, digits = 3))
hh_pixels <-sum(hh_freq$count)
hh_perc_lost <-100-(hh_pixels/total_pixels)*100

# hv
hv_list <-list.files(pattern = "*HV_01.unw*")
hv <-rast(hv_list[1])
values(hv)[values(hv) == 0] <-NA
plot(hv)
hv_freq <-as.data.frame(freq(hv, digits = 3))
hv_pixels <-sum(hv_freq$count)
hv_perc_lost <-100-(hv_pixels/total_pixels)*100

# vh
vh_list <-list.files(pattern = "*VH_01.unw*")
vh <-rast(vh_list[1])
values(vh)[values(vh) == 0] <-NA
plot(vh)
vh_freq <-as.data.frame(freq(vh, digits = 3))
vh_pixels <-sum(vh_freq$count)
vh_perc_lost <-100-(vh_pixels/total_pixels)*100


# vv
vv_list <-list.files(pattern = "*VV_01.unw*")
vv <-rast(vv_list[1])
values(vv)[values(vv) == 0] <-NA
plot(vv)
vv_freq <-as.data.frame(freq(vv, digits = 3))
vv_pixels <-sum(vv_freq$count)
vv_perc_lost <-100-(vv_pixels/total_pixels)*100

## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

hh_list <-list.files(pattern = "*HH_01.cor*")
hh_cor <-rast(hh_list[1])
hh_cor_vg_v1 <-mask(hh_cor, vg)
hh_cor_vg <-crop(hh_cor_vg_v1, vg)
plot(hh_cor_vg)
fr_vg <-as.data.frame(freq(hh_cor_vg, digits = 3))
total_pixels_vg <-sum(fr_vg$count)


# hh
hh_vg_v1 <-mask(hh, vg)
hh_vg <-crop(hh_vg_v1, vg)
plot(hh_vg)
hh_vg_freq <-as.data.frame(freq(hh_vg, digits = 3))
hh_vg_pixels <-sum(hh_vg_freq$count)
hh_perc_lost_vg <-100-(hh_vg_pixels/total_pixels_vg)*100


# hv
hv_vg_v1 <-mask(hv, vg)
hv_vg <-crop(hv_vg_v1, vg)
plot(hv_vg)
hv_vg_freq <-as.data.frame(freq(hv_vg, digits = 3))
hv_vg_pixels <-sum(hv_vg_freq$count)
hv_perc_lost_vg <-100-(hv_vg_pixels/total_pixels_vg)*100

# vh
vh_vg_v1 <-mask(vh, vg)
vh_vg <-crop(vh_vg_v1, vg)
plot(vh_vg)
vh_vg_freq <-as.data.frame(freq(vh_vg, digits = 3))
vh_vg_pixels <-sum(vh_vg_freq$count)
vh_perc_lost_vg <-100-(vh_vg_pixels/total_pixels_vg)*100


# vv
vv_vg_v1 <-mask(vv, vg)
vv_vg <-crop(vv_vg_v1, vg)
plot(vv_vg)
vv_vg_freq <-as.data.frame(freq(vv_vg, digits = 3))
vv_vg_pixels <-sum(vv_vg_freq$count)
vv_perc_lost_vg <-100-(vv_vg_pixels/total_pixels_vg)*100

