library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
# library(scales)
# library(units)
# library(viridis)
library(sf)

#----------------------------------------------------------------------------
########################################
## load data
########################################


# CL gdb path
cl_gdb <- "GIS/CL_v4_July31_2018.gdb"


# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
cl_list <- ogrListLayers(cl_gdb)
print(cl_list)


# Read the feature class
cl <- readOGR(dsn=cl_gdb,layer="anc_cl_ws")


# Determine the FC extent, projection, and attribute information
summary(cl)
object.size(cl)
proj4string(cl)

# Monongaehla National Forest 
mon_sh <- readOGR("GIS/mon_nf")
mon_sh <- spTransform(mon_sh, CRS = proj4string(cl))


# State Shaepfile
states <- readOGR("GIS/states")
states <- spTransform(states, CRS = proj4string(cl))



#----------------------------------------------------------------------------

########################################
## Clip to Mon NF
########################################

mon_cl <- intersect(mon_sh, cl) 
saveRDS(mon_cl, "data/mon_cl.RDS")


#----------------------------------------------------------------------------











