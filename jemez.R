# jemez
# 12/29

#geolocating is right, talk to HP about it

library(raster)
library(data.table)
library(rgdal)
library(gdalUtils)
library(sp)
library(caTools)
library(rgdal)
library(rgeos)


jemez_cor_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/HH/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.cor.grd")
jemez_amp1_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/HH/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.amp1.grd")
jemez_amp2_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/HH/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.amp2.grd")
jemez_int_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/HH/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.int.grd") 
jemez_unw_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/HH/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.unw.grd") 
jemez_DEM_HH_1 <-raster("/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/DEM/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.hgt.grd.tiff")

hist(jemez_cor_HH_1, breaks = 100)
yaht <-freq(jemez_cor_HH_1, digits = 3)
values(jemez_cor_HH_1)[values(jemez_cor_HH_1) == 0] = NA
values(jemez_cor_HH_1)[values(jemez_cor_HH_1) > 1] = NA
plot(jemez_cor_HH_1)


hist(jemez_amp1_HH_1, breaks = 100)
values(jemez_amp1_HH_1)[values(jemez_amp1_HH_1) == 0] = NA
values(jemez_amp1_HH_1)[values(jemez_amp1_HH_1) > 1] = NA
plot(jemez_amp1_HH_1)
yaht <-freq(jemez_cor_HH_1, digits = 3)

mask_test <- mask(jemez_unw_HH_1, jemez_cor_HH_1, maskvalue = NA)
values(mask_test)[values(mask_test) == 0] = NA
plot(mask_test)
writeRaster(mask_test, "/Volumes/JT/projects/uavsar/jemez/polygons/mask_test.tif")
writeRaster(jemez_amp1_HH_1, "/Volumes/JT/projects/uavsar/jemez/polygons/amp_test.tif")
plot(jemez_unw_HH_1)

plot(jemez_DEM_HH_1)
values(jemez_DEM_HH_1)[values(jemez_DEM_HH_1) == -10000] = NA
hist(jemez_DEM_HH_1, "/Volumes/JT/projects/uavsar/jemez/polygons/dem.tif")

writeRaster(jemez_cor_HH_1, "/Volumes/JT/projects/uavsar/jemez/polygons/test_dem.tif")

#get extent of DEM
dem_exnt <- extent(jemez_DEM_HH_1)
#create a spatial polygonn of it
dem_poly <- as(dem_exnt, 'SpatialPolygons')
plot(dem_poly)

# make all values the same. Either do
same_vals <- jemez_DEM_HH_1 > -Inf
plot(same_vals)

Sys.which('gdal_polygonize.py')

## Define the function
polygonizer <- function(x, outshape=NULL, pypath=NULL, readpoly=TRUE, 
                        fillholes=FALSE, aggregate=FALSE, 
                        quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL 
  # outshape: the path to the output shapefile (if NULL, a temporary file will 
  #           be created) 
  # pypath: the path to gdal_polygonize.py or OSGeo4W.bat (if NULL, the function 
  #         will attempt to determine the location)
  # readpoly: should the polygon shapefile be read back into R, and returned by
  #           this function? (logical) 
  # fillholes: should holes be deleted (i.e., their area added to the containing
  #            polygon)
  # aggregate: should polygons be aggregated by their associated raster value?
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly) || isTRUE(fillholes)) require(rgdal)
  if (isTRUE(aggregate)) require(rgeos)
  if (is.null(pypath)) {
    cmd <- Sys.which('OSGeo4W.bat')
    pypath <- 'gdal_polygonize'
    if(cmd=='') {
      cmd <- 'python'
      pypath <- Sys.which('gdal_polygonize.py')
      if (!file.exists(pypath)) 
        stop("Could not find gdal_polygonize.py or OSGeo4W on your system.") 
    }
  }
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
system2(cmd, args=(
sprintf('"%s" "%s" %s -f "ESRI Shapefile" "%s.shp"', 
  pypath, rastpath, ifelse(quietish, '-q ', ''), outshape)))
  
  if(isTRUE(aggregate)||isTRUE(readpoly)||isTRUE(fillholes)) {
    shp <- readOGR(dirname(outshape), layer=basename(outshape), 
                   verbose=!quietish)    
  } else return(NULL)
  
  if (isTRUE(fillholes)) {
    poly_noholes <- lapply(shp@polygons, function(x) {
      Filter(function(p) p@ringDir==1, x@Polygons)[[1]]
    })
    pp <- SpatialPolygons(mapply(function(x, id) {
      list(Polygons(list(x), ID=id))
    }, poly_noholes, row.names(shp)), proj4string=CRS(proj4string(shp)))
    shp <- SpatialPolygonsDataFrame(pp, shp@data)
    if(isTRUE(aggregate)) shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  if(isTRUE(aggregate) & !isTRUE(fillholes)) {
    shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  ifelse(isTRUE(readpoly), return(shp), return(NULL))
}


dem_shp <-polygonizer(jemez_DEM_HH_1,
                      pypath = "/Users/jacktarricone/opt/anaconda3/lib/python3.8/site-packages/osgeo/utils/gdal_polygonize.py",
                      outshape = "/Volumes/JT/projects/uavsar/jemez/test.shp", 
                      readpoly = FALSE)


# convert to polygons (you need to have package 'rgeos' installed for this to work)
dem_shp <- rasterToPolygons(same_vals, dissolve=TRUE)
plot(dem_shp)

# look at the results
plot(x)
plot(p, lwd=5, border='red', add=TRUE)
plot(pp, lwd=3, border='blue', add=TRUE)






dem_exnt <-extent(jemez_DEM_HH_1)
plot(dem_exnt)
jemez_DEM_HH_1 <-trim(jemez_DEM_HH_1)

test <- crop(jemez_unw_HH_1, jemez_DEM_HH_1, maskvalue = NA)
plot(test)

#; ------------------------------------------------------------------------------


# ; Ground Range Multi-Looked Product Information 
#; ------------------------------------------------------------------------------
#  ; Slant range multi-looked products projected to the ground
#; Applicable Files: int.grd, unw.grd, cor.grd, amp1.grd, amp2.grd, hgt.grd

#Ground Range Data Latitude Lines               (-)             = 6315
#Ground Range Data Longitude Samples            (-)             = 4631
#Ground Range Data Starting Latitude            (deg)           = 36.064996080000000     ; center of upper left ground range pixel
#Ground Range Data Starting Longitude           (deg)           = -106.565357880000008   ; center of upper left ground range pixel
#Ground Range Data Latitude Spacing             (deg)           = -0.0000555600000000
#Ground Range Data Longitude Spacing            (deg)           = 0.0000555600000000
#Approximate Upper Left Latitude                (deg)           = 36.02454445            ; approximate corners of non-zero image footprint
#Approximate Upper Left Longitude               (deg)           = -106.56254541          ; approximate corners of non-zero image footprint
#Approximate Upper Right Latitude               (deg)           = 36.04360921            ; approximate corners of non-zero image footprint
#Approximate Upper Right Longitude              (deg)           = -106.33324373          ; approximate corners of non-zero image footprint
#Approximate Lower Left Latitude                (deg)           = 35.73394238            ; approximate corners of non-zero image footprint
#Approximate Lower Left Longitude               (deg)           = -106.55381725          ; approximate corners of non-zero image footprint
#Approximate Lower Right Latitude               (deg)           = 35.75907871            ; approximate corners of non-zero image footprint
#Approximate Lower Right Longitude              (deg)           = -106.30749763          ; approximate corners of non-zero image footprint

# lat/lon or 4 corners
# UL (36.02454445, -106.56254541)
# UR (36.04360921, -106.33324373)
# LL (35.73394238, -106.55381725)
# LR (35.75907871, -106.30749763)

# this method gets closer but still not correct
# need to pin the corners, but still it's not straight data...
appt_ymx <-(36.02454445+36.04360921)/2
appt_ymn <-(35.73394238+35.75907871)/2
appt_xmn <-(-106.56254541+(-106.55381725))/2
appt_xmx <-(-106.33324373+(-106.30749763))/2

system.time(cor_HH <-as.matrix(fread("/Users/jacktarricone/Desktop/jemez/cor_mats/cor_HH_mat.txt")))

cor_HH_raw <-raster(cor_HH, xmn=-106.5582, xmx=-106.3204,ymn=35.74651, ymx=36.03408,
                  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

flip1 <-flip(cor_HH_raw,1) #flip on vert axix
cor_HH_rast <-flip(flip1,2) #flip on horz axix
cor_HH_rast #insepct
plot(cor_HH_rast)
writeRaster(cor_HH_rast, "/Users/jacktarricone/Desktop/jemez/cor_mats/cor_HH_rast.tif")

jemez_HH_DEM <- raster("/Users/jacktarricone/Desktop/jemez/raw_data/DEM/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_01.hgt.grd.tiff") 
jemez_HH_DEM[jemez_HH_DEM == -10000] <- NA
plot(jemez_HH_DEM)
hist(jemez_HH_DEM)
jemez_HH_DEM
