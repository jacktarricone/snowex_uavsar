library(BiocManager) # huge repo of packages used in the bio/genetics, we need it for our hdf5 readera
library(rhdf5) # hdf5 reader from bioc, which is the format the margulis data is in
library(rgeos) # spatial data driver
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal) # geospatial data abstraction library aka GDAL, very important to learn
library(tidyverse) # for the pipe operator
library(sf) # vector data aka shapefiles
library(data.table) # very useful package for working with dataframes and .csv data, much faster than base r
library(gtools) # for mixed sort
library(raster) # working with raster data
library(gdalUtils)

sds <-get_subdatasets("/Users/jacktarricone/Desktop/modis/MOD10A1.A2020064.h09v05.061.2020329193539.hdf")


# set path and file name for hdf5 SWE file
hdf_path <- "/Users/jacktarricone/Downloads" #create path
hdf_name <- "MOD10A1.A2020064.h09v05.061.2020329193539.hdf" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
# h5readAttributes(hdf_file, name = "SWE") #$units = mm

# read in 1 days SWE data which is an array, or stack of matrixes
modis <- h5read(hdf_file, "/SWE", index = list(1:6601,1:5701,185)) #read in SWE group
class(swe86_185_raw) #inspect 
dim(swe86_185_raw) #dimensions