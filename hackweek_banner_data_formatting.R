# data formatting for hackweek 2022 tutorial

library(terra)

# setwd and list
setwd("/Users/jacktarricone/hackweek2022/tutorial_data/")
list.files()

# r <-rast("lowman_red.tif")
# g <-rast("lowman_green.tif")
# b <-rast("lowman_blue.tif")
# 
# rgb <-c(r,g,b)
# writeRaster(rgb,"lowman_rgb.tif")

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

# jan 15th amp
amp <-rast("lowman_23205_21002_004_210115_L090HHHH_CX_01.grd.tiff")
amp_db <-10*log10(amp) # conver to db
amp_db <-resample(amp_db, cor) # crop to same extent as unw and cor
amp_db
hist(amp_db, breaks = 50)
plot(amp_db)

# dem
dem <-rast("lowman_23205_21002_004_210115_L090_CX_01.hgt.tiff")
dem <-resample(dem, cor) # crop to same extent as unw and cor
dem
plot(dem)

# inc
inc <-rast("lowman_23205_21002_004_210115_L090_CX_01.inc.tiff")
inc <-resample(inc, cor) # crop to same extent as unw and cor
inc
plot(inc)

# stack all 3
stack <-c(unw,cor,amp_db,dem,inc)
plot(stack)

# crop stack to rbg extent
stack_crop <-crop(stack, rgb)
plot(stack_crop[[5]])

# resample rgb to uavsar res
rgb_resamp <-resample(rgb, stack_crop, method = "bilinear")

# mask
rgb_mask <-mask(rgb_resamp, stack_crop[[2]])
plotRGB(rgb_mask, r = 3, g = 2, b = 1, stretch = "lin")

# test
full_stack <-c(stack_crop, rgb_mask)
plot(full_stack[[6]])
ext(full_stack)

# crop down to get each raster under 50mb
crop_ext <-ext(-115.30, -115.15, 44.23, 44.34)
p <- as.polygons(crop_ext)
crs(p) <- crs(full_stack)
plot(unw)
plot(p, add = TRUE)
#writeVector(p, 'hackweek_banner.shp')

# cop stack
test <-crop(full_stack, crop_ext)
plot(test)

# convert inc to deg
inc_deg <-(test[[5]]*180)/pi
plot(inc_deg)


# save individual layers
# writeRaster(test[[1]],"lowman_unw.tif")
# writeRaster(test[[2]],"lowman_cor.tif")
# writeRaster(test[[3]],"lowman_amb_db.tif")
# writeRaster(test[[4]],"lowman_dem.tif")
# writeRaster(inc_deg,"lowman_inc_deg.tif")
# writeRaster(test[[6]],"lowman_blue.tif")
# writeRaster(test[[7]],"lowman_green.tif")
# writeRaster(test[[8]],"lowman_red.tif")



# create shape file from cor data
cor_shp <-cor
cor_shp[cor_shp > 0] <- 1
plot(cor_shp)

# convert to vector data
cor_shp_file <-as.polygons(cor_shp)

## aggregate polyongs up to just data extent
cor_shp_v1 <- aggregate(cor_shp_file, dissolve = TRUE, fun = "mean",cores = 10)
plot(cor_shp_v1)

#writeVector(cor_shp_v1, "lowman_ext.shp")


