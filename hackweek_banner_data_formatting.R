# data formatting for hackweek 2022 tutorial

library(terra)

# setwd and list
setwd("/Users/jacktarricone/hackweek2022/tutorial_data/")
list.files()

# readin in banner hlf rbg
rgb <-rast("banner_hls_rgb.tif")
rgb # check
plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin")

# read in unwrapped phase data
unw <-rast("lowman_23205_21002-004_21004-003_0005d_s01_L090HH_01.unw.grd.tiff")
unw
plot(unw)
#plot(rgb[[1]], add = TRUE)

# coherence
cor <-rast("lowman_23205_21002-004_21004-003_0005d_s01_L090HH_01.cor.grd.tiff")
cor
plot(cor)

# stack all 3
stack1 <-c(unw,cor)

# jan 15th amp
amp <-rast("lowman_23205_21002_004_210115_L090HHHH_CX_01.grd.tiff")
amp_db <-10*log10(amp) # conver to db
amp_db <-resample(amp_db, stack1) # crop to same extent as unw and cor
amp_db
hist(amp_db, breaks = 50)
plot(amp_db)

# stack all 3
stack2 <-c(stack1,amp_db)
plot(stack2)

# crop stack to rbg extent
stack_crop <-crop(stack2, rgb)
plot(stack_crop[[3]])

# resample rgb to uavsar res
rgb_resamp <-resample(rgb, stack_crop, method = "bilinear")

# mask
rgb_mask <-mask(rgb_resamp, stack_crop[[2]])
plotRGB(rgb_mask, r = 3, g = 2, b = 1, stretch = "lin")

# test
full_stack <-c(stack_crop, rgb_mask)
plot(full_stack[[1]])
ext(full_stack)


# crop down to get each raster under 50mb
crop_ext <-ext(-115.30, -115.15, 44.23, 44.34)
p <- as.polygons(crop_ext)
crs(p) <- crs(full_stack)
plot(unw)
plot(p, add = TRUE)
writeVector(p, 'file.shp')

test <-crop(full_stack, crop_ext)
plot(test)


# test save
writeRaster(test[[1]],"test_unw5.tif")
