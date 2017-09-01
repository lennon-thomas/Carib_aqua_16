rm(list = ls())


library(rgdal)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)


#setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture") #work computer directory
boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# EEZs data from: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn=paste(boxdir,"Suitability/raw/World_EEZ_v9_20161021/",sep=""),layer="eez",stringsAsFactors=FALSE)

Tonga_eez<- EEZ[EEZ$GeoName == "Tongan Exclusive Economic Zone",]

ext<-extent(-176, -173, -19.5, -18)

depth = raster(paste(boxdir,"Suitability/raw/topo30.tif",sep=""))
depth <- rotate(depth,progress='text')

tonga_depth<-crop(depth,ext)
tonga_depth<-mask(tonga_depth,Tonga_eez)
tonga_depth = calc(tonga_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text')#,filename=paste(boxdir,'Suitability/tmp/carib_depth.tif',sep=""),overwrite=TRUE)


color.scale(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
            extremes=NA,na.color=NA)

tonga_depth[tonga_depth<25]<-0

tonga_depth[tonga_depth>100]<-0

tonga_depth[tonga_depth > 0]<-500

tonga_depth[is.na(tonga_depth)]<-1000

plot(tonga_depth,col=c("lightblue","darkblue","bisque4"))


ext<-extent(-176, -172, -19, -14.15417)
tonga_depth<-crop(tonga_depth,ext)
