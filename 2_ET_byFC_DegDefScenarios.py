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
pathDBF = r'D:/Yunuen/6PostDoc/Dashboards/ET/DBFiles'
pathET=r'D:/Yunuen/6PostDoc/Dashboards/Layers'
polygons= 'D:/Yunuen/6PostDoc/Dashboards/Layers/Tis_TerritoriosIndigenas_sa_greater9sq.shp'
areaPrefix= "IndTerr"
zoneField= "objectid"
########################################################################
# Set the workspace
arcpy.env.workspace = pathET

## Lists all MTDD/variable rasters
listET = arcpy.ListRasters('*ET_*.tif')
print(listET)


# Iterate through each MTDD/variable rasters and
# calculate zonal statistics as table based on the input polygons
counter= 0
for raster in listET:
    print(str(raster)+" Counter: "+str(counter))
    arcpy.env.cellSize = pathET+'\\'+ raster
    arcpy.env.snapRaster = pathET+'\\'+ raster
    outZSaT = ZonalStatisticsAsTable(polygons, zoneField,pathET+'\\'+ raster,
                            pathDBF + '\\' +areaPrefix+"ET_"+str(counter)+".dbf", "DATA", "MEAN")
    counter=counter+1
