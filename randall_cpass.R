# testing cameron pass data for randall
# import libraries

library(terra)
library(ggplot2)

# phase
unw <-rast("/Users/jacktarricone/hackweek2022/rockmt_14107_21004-001_21007-001_0007d_s01_L090HH_01.unw.grd.tiff")
unw
hist(unw)

# inc
inc_raw <-rast("/Users/jacktarricone/hackweek2022/rockmt_14107_21004_001_210120_L090_CX_01.inc.tiff")
inc_raw

# align grids
inc <-resample(inc_raw, unw)
inc

# import depth_from_phase function
# i translated this from the uavsar_pytools package
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/snowex_uavsar/master/insar_swe_functions.R")

# radar wave length from uavsar annotation file
uavsar_wL <- 23.8403545

# testing
depth_change <-depth_from_phase(delta_phase = unw,
                                inc_angle = inc,
                                perm = 1.2,
                                wavelength = uavsar_wL)

# test plot
plot(depth_change)
hist(depth_change, breaks = 100)
IQR(depth_change)

writeRaster(depth_change, "/Users/jacktarricone/hackweek2022/rockmt_jan20_27_depth_change.tif")

