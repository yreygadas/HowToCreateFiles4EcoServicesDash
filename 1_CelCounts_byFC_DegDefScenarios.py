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
pathDBF = r'D:/Yunuen/6PostDoc/Dashboards/DBFCellCounts'
pathMTDD = r'D:/Yunuen/6PostDoc/Dashboards/Layers'
polygons= 'D:/Yunuen/6PostDoc/Dashboards/Layers/Tis_TerritoriosIndigenas_sa_greater9sq.shp'
areaPrefix= "IndTerr"
zoneField= "objectid"
########################################################################
# Set workspace
arcpy.env.workspace = pathMTDD

## Lists all MTDD rasters
listMTDD = arcpy.ListRasters('*MTDD_*.tif')
print(listMTDD)

# Iterate through each MTDD/variable rasters and
# calculate zonal statistics as table based on the input polygons
counter= 0
for raster in listMTDD:
    print(str(raster)+" Counter: "+str(counter))
    arcpy.env.cellSize = pathMTDD+'\\'+ raster
    arcpy.env.snapRaster = pathMTDD+'\\'+ raster
    outZSaT = ZonalStatisticsAsTable(polygons, zoneField,pathMTDD+'\\'+ raster,
                            pathDBF + '\\' +areaPrefix+"CellCounts_"+str(counter)+".dbf", "DATA", "MEAN")
    counter=counter+1

