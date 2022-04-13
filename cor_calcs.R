library(terra)

# find polarization with highest coherence

# set working dir
setwd("/Users/jacktarricone/ch1_jemez_data/feb12-19")

# hh
hh_list <-list.files(pattern = "*HH_01.cor*")
hh <-rast(hh_list[1])
values(hh)[values(hh) == 0] <-NA
plot(hh)
hh_mean <-global(hh, mean, na.rm = TRUE)

# hv
hv_list <-list.files(pattern = "*hv_01.cor*")
hv <-rast(hv_list[1])
values(hv)[values(hv) == 0] <-NA
plot(hv)
hv_mean <-global(hv, na.rm = TRUE)


# vh
vh_list <-list.files(pattern = "*VH_01.cor*")
vh <-rast(vh_list[1])
values(vh)[values(vh) == 0] <-NA
plot(vh)
vh_mean <-global(vh, mean, na.rm = TRUE)


# vv
vv_list <-list.files(pattern = "*VV_01.cor*")
vv <-rast(vv_list[1])
values(vv)[values(vv) == 0] <-NA
plot(vv)
vv_mean <-global(vv, mean, na.rm = TRUE)

