#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 13:30:23 2021

@author: Jack Tarricone
# this script creates an ENVI header (.hdr) with information from a UAVSAR InSAR pair's annotation file
# the header file must have the same name as the .grd file, only changing the extention to .hdr from .grd
# once the header is created, the .grd files can be uploaded into programming and GIS software (R, QGIS, Python, ArcGIS)
# adapted from Nathan Thomas
# https://spectraldifferences.wordpress.com/2014/09/29/envi-header-uavsar/
"""

headerParameters = {}
headerParameters['fileName'] = '#insert file path here, ending with /filename.hdf#'
headerParameters['samples'] = '#get from annoation file, changes for each pair even at the same location#'
headerParameters['lines'] = '#get from annoation file, changes for each pair even at the same location#'
headerParameters['ULlon'] = '#from annoation#'
headerParameters['ULlat'] = '#from annoation#'
headerParameters['pixelSize'] = '#from annoation#'
 
headerText = '''ENVI
description = {{{fileName}}}
samples = {samples}
lines = {lines}
bands = 1
header offset = 0
data type = # insert 4 (floating point) for .amp1, .am2, .cor, .unw, # insert 6 (complex) for .int #
interleave = bsq
sensor type = Unknown
byte order = 0
map info = {{Geographic Lat/Lon, 1.000, 1.000, {ULlon}, {ULlat}, {pixelSize}, {pixelSize}, WGS-84, units=Degrees}}
wavelength units = Unknown'''.format(**headerParameters)
 
headerFile = open('#insert file path here, ending with /filename.hdf#','w')
headerFile.write(headerText)
headerFile.close()