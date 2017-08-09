#6/29/17


rm(list = ls())

library(raster)
library(rgdal)
library(tmap)
library(dplyr)
library(ncdf4)
library(stringr)
library(broom)
library(fasterize)
library(sf)

boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')


# Read in TPC results and name layers

prod<-stack(paste(boxdir,"TPC/Carib_cobia_suitable.nc",sep=""),varname="Prod")

growth<-stack(paste(boxdir,"TPC/Carib_cobia_suitable.nc",sep=""),varname="Grow")

st <- as.Date("2007-01-01")
en <- as.Date("2016-12-01")

layernames <- as.character(seq(st, en, by = "1 month")) %>%
  str_sub(1,7)

names(prod)<-layernames
#prod_values <- getValues(prod) # get cell values as a matric (columns are layers (e.g. months), rows are cells)

names(growth)<-layernames
#growth_values <- getValues(growth)

## Add economic data

eez_shape<-readOGR(dsn = paste(boxdir,"Suitability/tmp/",sep=""),layer = "carib_eez_shape")

econ_params<-read.csv(paste(boxdir,"economic/data/eez_parameters.csv",sep=""))

econ_params$Territory1 <- gsub('Curacao','Curaçao', econ_params$Territory1)

econ_params$Territory1 <- gsub('Saint-Barth\x8elemy', 'Saint-Barthélemy', econ_params$Territory1)

eez_shape_full<-merge(eez_shape,econ_params,by="Territory1",all=TRUE)

# Rasterize econ parameter layer

depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep=""))

eez_shape_full<-st_as_sf(eez_shape_full) #Convert to sf object for fasterize

fuel_price<-fasterize(eez_shape_full,depth, field = 'fuel_price')

min_wage<-fasterize(eez_shape_full,depth,field = "min_wage")

permit_fee<-fasterize(eez_shape_full,depth, field="permit_fee")

risk_score<-fasterize(eez_shape_full,depth, field="risk")

# read in distance to shore raster

shore_distance<-raster(paste(boxdir,"economic/data/shore_distanc.tif",sep=""))

# Create layer that to increase installation and maintenance cost as a function of depth. Farms >50 m increases total by 10%

depth_charge<-depth

depth_charge[depth_charge > 50]<- 0.10

depth_charge[depth_charge < 51 & depth_charge > 0.10]<- 0


## Wave exposure layer

waves<-raster(paste(boxdir,"economic/data/PECS/PECS_exposure.tif",sep=""))

carib_waves<-crop(waves,depth)
carib_waves<-resample(waves,depth)
carib_waves<-mask(carib_waves,shore_distance)


normal_waves<-10^log_waves

# Test by multiplying permit values times production quantities (meaningless, can delete)
test2 <- prod_values * permit_values






