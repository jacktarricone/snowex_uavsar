library(terra)

# 2/19-2/26 cor calcs
# set working dir
setwd("/Users/jacktarricone/ch1_jemez_data/feb19-26v1")

# hh
hh_list <-list.files(pattern = "*HH_01.cor*")
hh <-rast(hh_list[1])
values(hh)[values(hh) == 0] <-NA
plot(hh)
hh_full_mean <-global(hh, mean, na.rm = TRUE)

# hv
hv_list <-list.files(pattern = "*HV_01.cor*")
hv <-rast(hv_list[1])
values(hv)[values(hv) == 0] <-NA
plot(hv)
hv_full_mean <-global(hv, na.rm = TRUE)

# vh
vh_list <-list.files(pattern = "*VH_01.cor*")
vh <-rast(vh_list[1])
values(vh)[values(vh) == 0] <-NA
plot(vh)
vh_full_mean <-global(vh, mean, na.rm = TRUE)


# vv
vv_list <-list.files(pattern = "*VV_01.cor*")
vv <-rast(vv_list[1])
values(vv)[values(vv) == 0] <-NA
plot(vv)
vv_full_mean <-global(vv, mean, na.rm = TRUE)

## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# hh
hh_vg_v1 <-mask(hh, vg)
hh_vg <-crop(hh_vg_v1, vg)
plot(hh_vg)
hh_vg_mean <-global(hh_vg, mean, na.rm = TRUE)

# hv
hv_vg_v1 <-mask(hv, vg)
hv_vg <-crop(hv_vg_v1, vg)
plot(hv_vg)
hv_vg_mean <-global(hv_vg, mean, na.rm = TRUE)

# vh
vh_vg_v1 <-mask(vh, vg)
vh_vg <-crop(vh_vg_v1, vg)
plot(vh_vg)
vh_vg_mean <-global(vh_vg, mean, na.rm = TRUE)

# vv
vv_vg_v1 <-mask(vv, vg)
vv_vg <-crop(vv_vg_v1, vg)
plot(vv_vg)
vv_vg_mean <-global(vv_vg, mean, na.rm = TRUE)

r1 <-cbind(hh_full_mean, hv_full_mean, vh_full_mean, vv_full_mean)
names(r1) <-c("hh","hv","vh","vv")
r2 <-cbind(hh_vg_mean, hv_vg_mean, vh_vg_mean, vv_vg_mean)
names(r2) <-c("hh","hv","vh","vv")

df <-rbind(r1,r2)
print(df)
