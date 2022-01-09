# starting analysis with direct download for zenodo
# jack tarricone
library(rgdal)
library(gdalUtils)
library(reticulate)

"/Users/jacktarricone/opt/miniconda3/bin/python"

# Set the path to the Python executable file
use_python("/Users/jacktarricone/opt/miniconda3/bin/python", required = T)

# Check the version of Python.
py_config()
use_virtualenv("test_env")

# define file path and name to download
temp <-"/Users/jacktarricone/Desktop/sagehen/temp_test/test.zip"

# url link to interferometric zip from ASF
asf_url <-"https://datapool.asf.alaska.edu/INTERFEROMETRY_GRD/UA/donner_03904_20014-003_20016-007_0014d_s01_L090_01_int_grd.zip"

# download files to temp dir
download.file(url = asf_url, destfile = temp, method = "wget")

# unzip
setwd("/Users/jacktarricone/Desktop/sagehen/temp_test/")
int_files <-unzip(temp)
int_files # check

# define 
int_files[1]
