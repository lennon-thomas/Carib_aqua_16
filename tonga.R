rm(list = ls())


library(rgdal)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)
library(tmap)


#setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture") #work computer directory
boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# EEZs data from: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn=paste(boxdir,"Suitability/raw/World_EEZ_v9_20161021/",sep=""),layer="eez",stringsAsFactors=FALSE)

Tonga_eez<- EEZ[EEZ$GeoName == "Tongan Exclusive Economic Zone",]

ext<-extent(-174.2, -173.85, -18.9, -18.55)

Tonga_eez<-crop(Tonga_eez,ext)

#depth

depth = raster(paste(boxdir,"Suitability/raw/topo30.tif",sep=""))
depth <- rotate(depth,progress='text')


tonga_depth<-crop(depth,ext)
tonga_depth<-mask(tonga_depth,Tonga_eez)
tonga_depth = calc(tonga_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text')#,filename=paste(boxdir,'Suitability/tmp/carib_depth.tif',sep=""),overwrite=TRUE)




tonga_depth[tonga_depth<25]<-0

tonga_depth[tonga_depth>100]<-0

#tonga_depth[tonga_depth > 0]<-500





#Shipping
ships<-raster(paste(boxdir,"Tonga/tmp/tonga_ship.tif",sep=""))

ships<-crop(ships,ext)

top<-unique(ships) #5949970

top<-sort(top,decreasing=TRUE)

num<-(length(top)*.10) #top 10% of shipping activity from Becca's paper

top_num<-top[1:num]

ship_threshold<-min(top_num)

ships[ships>ship_threshold]<-0 

ships[ships==ship_threshold]<-0

#coral
coral<-raster(paste(boxdir,"Tonga/tmp/tonga_coral.tif",sep=""))

coral<-crop(coral,ext)

coral[coral==0]<-1

coral[coral==2]<-0

#oil
oil<-raster(paste(boxdir,"Tonga/tmp/tonga_oil.tif",sep=""))

oil<-crop(oil,ext)

oil[oil>0]<-0

#
seamount<-raster(paste(boxdir,"Tonga/tmp/tonga_seamount.tif",sep=""))

seamount<-crop(seamount,ext)

plot(seamount)

#suitabiliby
s = stack(tonga_depth,ships,coral)

s<-stackApply(s,indices=c(1,1,1),fun=prod,na.rm=TRUE)

all_suitable<-mask(s,Tonga_eez,maskvalues=NA,inverse=FALSE)

all_suitable[all_suitable>1]<-1

all_suitable[all_suitable==0]<-NA


all_suitable<-mask(all_suitable,Tonga_eez,maskvalues=NA,inverse=FALSE)


tm_shape(all_suitable)+
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red"),labels = c("Suitable Areas"),title = "")+
  tm_shape(Tonga_eez,is.master = TRUE) +
  tm_fill(col=c("lightblue"),alpha = c(0.6),legend.show=FALSE) +
  tm_borders(lwd = 1.2) +
  tm_scale_bar(breaks = NULL, width = NA, size =.6, text.color = NA,
               color.dark = "black", color.light = "white", lwd = 1, position =c("left","bottom")
               ) 
