#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 13:30:23 2021

@author: jacktarricone
"""

headerParameters = {}
headerParameters['fileName'] = '/Volumes/JT/projects/uavsar/jemez/raw_data/02192020_02262020/headers/alamos_2.hdr'
headerParameters['samples'] = '4631'
headerParameters['lines'] = '6315'
headerParameters['ULlon'] = '-106.565357880000008'
headerParameters['ULlat'] = '36.064996080000000'
headerParameters['pixelSize'] = '0.0000555600000000'
 
headerText = '''ENVI
description = {{{fileName}}}
samples = {samples}
lines = {lines}
bands = 1
header offset = 0
data type = 4
interleave = bsq
sensor type = Unknown
byte order = 0
map info = {{Geographic Lat/Lon, 1.000, 1.000, {ULlon}, {ULlat}, {pixelSize}, {pixelSize}, WGS-84, units=Degrees}}
wavelength units = Unknown'''.format(**headerParameters)
 
headerFile = open('/Volumes/JT/projects/uavsar/jemez/raw_data/02192020_02262020/headers/alamos_2.hdr','w')
headerFile.write(headerText)
headerFile.close()