#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 13:30:23 2021

@author: jacktarricone
"""

headerParameters = {}
headerParameters['fileName'] = '/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/headers/alamos_1_int.hdr'
headerParameters['samples'] = '4713'
headerParameters['lines'] = '7795'
headerParameters['ULlon'] = '-106.566746880000011'
headerParameters['ULlat'] = '36.121778400000004'
headerParameters['pixelSize'] = '0.0000555600000000'
 
headerText = '''ENVI
description = {{{fileName}}}
samples = {samples}
lines = {lines}
bands = 1
header offset = 0
data type = 6
interleave = bsq
sensor type = Unknown
byte order = 0
map info = {{Geographic Lat/Lon, 1.000, 1.000, {ULlon}, {ULlat}, {pixelSize}, {pixelSize}, WGS-84, units=Degrees}}
wavelength units = Unknown'''.format(**headerParameters)
 
headerFile = open('/Volumes/JT/projects/uavsar/jemez/raw_data/02122020_02192020/headers/alamos_1_int.hdr','w')
headerFile.write(headerText)
headerFile.close()