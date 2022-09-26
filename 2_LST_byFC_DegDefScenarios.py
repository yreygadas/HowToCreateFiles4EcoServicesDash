#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      yreygada
#
# Created:     12/04/2022
# Copyright:   (c) yreygada 2022
# Licence:     <your licence>
#-------------------------------------------------------------------------------

# Set environments
import arcpy
import os
from arcpy.sa import *
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension('Spatial')

################### Set before running the script ######################
pathDBF = r'D:/Yunuen/6PostDoc/Dashboards/LST/DBFiles'
pathLST= r'D:/Yunuen/6PostDoc/Dashboards/Layers'
polygons= 'D:/Yunuen/6PostDoc/Dashboards/Layers/Tis_TerritoriosIndigenas_sa_greater9sq.shp'
areaPrefix= "IndTerr"
zoneField= "objectid"
########################################################################
# Set the workspace 
arcpy.env.workspace = pathLST

## Lists all MTDD/variable rasters
listLST = arcpy.ListRasters('*LST_*.tif')
print(listLST)

# Iterate through each MTDD/variable rasters and
# calculate zonal statistics as table based on the input polygons
counter= 0
for raster in listLST:
    print(str(raster)+" Counter: "+str(counter))
    arcpy.env.cellSize = pathLST+'\\'+ raster
    arcpy.env.snapRaster = pathLST+'\\'+ raster
    outZSaT = ZonalStatisticsAsTable(polygons, zoneField,pathLST+'\\'+ raster,
                            pathDBF + '\\' +areaPrefix+"LST_"+str(counter)+".dbf", "DATA", "MEAN")
    counter=counter+1
