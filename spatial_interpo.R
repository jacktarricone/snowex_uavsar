library(terra)
library(data.table)
library(randomForest)
library(raster)
library(fields) 


unw <-rast("/Volumes/JT/projects/uavsar/gmesa/data/2020/02-01-2020_02-13-2020/grmesa_27416_20003-028_20005-007_0011d_s01_L090_01_int_grd/sub/grmesa_27416_20003-028_20005-007_0011d_s01_L090HH_01.unw.grd")
values(unw)[values(unw) == 0] <-NA

breaks <- seq(-3, 2, by = .2)
plot(unw,
     main = "unw",
     col = hcl.colors(255, palette = "viridis"),
     breaks = breaks)

tl <-ext(-108.22, -108.2, 39.05, 39.07)
tl
unw_tl<-crop(unw, ext(tl))
plot(unw_tl)
unw_tl

ra <- aggregate(unw_tl, 10)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- values(ra)
i <- !is.na(v)
xy <- xy[i,]
v <- v[i]

tps <- Tps(xy, v)
p <- rast(unw_tl)

p <- interpolate(p, tps, na.rm = FALSE)
p <- mask(p, unw_tl)
plot(p)

r <- raster(matrix(1:16, nrow=8, ncol=8))
r[r==12] <- NA
plot(r)
r
unw_tl

fill.na <- function(x) {
  center = 0.5 + (width*width/2) 
  if( is.na(x)[center] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[center],0) )
  }
}  

unw_tl

width = 9
r2 <- raster::focal(r, w = matrix(1,width,width), fun = fill.na,  na.rm = FALSE)

summary(getValues(r2))

r2 <- focal(unw_rl, w = matrix(1,3,3), fun = fill.na, 
            pad = TRUE, na.rm = FALSE )

as.matrix(r)
as.matrix(r2)