#Calculating distance from each cell in study area to nearest port
# 6/17/17
# Port data from: https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_62&pubCode=0015


rm(list = ls())
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(spdep)
library(igraph)
library(dplyr)
library(colorRamps)

boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')


##Calculate distance to port

study_area1<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

study_area<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

study_area[is.na(study_area)]<-200

land<-raster(paste(boxdir,"Suitability/tmp/land_mask.tif",sep=""))

study_area<-mask(study_area,land)

ext<-extent(study_area)

ports<-readOGR(dsn=paste(boxdir,"economic/data/port_index",sep=""),layer="WPI")

carib_ports<-crop(ports,ext,progress = "text")

overlap<-extract(study_area,carib_ports,cellnumbers=TRUE,buffer=5000)

overlap2<-lapply(overlap,as.data.frame)

overlap3<-bind_rows(overlap2)

cells<-overlap3[,1]

study_area[cells]<-300

study_area[is.na(study_area)]<-0

study_area[study_area==200]<-NA

port_distance<-gridDistance(study_area,origin = 300, omit=c(NA,0))

port_list<-Which(port_distance==0,cell=TRUE,na.rm=TRUE)


writeRaster(port_distance,paste(boxdir,"economic/data/port_distance.tif",sep=""))


study_area[cells]<-1000

study_area[study_area<1000]<-NA

land[land>0]<-1

study_area<-mask(study_area,study_area1)



png(filename=paste(boxdir,"economic/data/port_distance.png",sep=""))

plot(port_distance, main="Distance to port (m)",col=rev(terrain.colors(500)))

plot(study_area,add=TRUE,col="black",legend=FALSE)

dev.off()

## Calculate distance from shore

study_area<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

study_area[is.na(study_area)]<-200

land<-raster(paste(boxdir,"Suitability/tmp/land_mask.tif",sep=""))

land[is.na(land)]<-0.01

land[land>0.01]<-NA

shore_distance<-distance(land,progress='text')


study_area<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

final_shore<-mask(shore_distance,study_area)

writeRaster(final_shore,filename=paste(boxdir,"economic/data/shore_distance",sep=""))

final_shore<-raster(paste(boxdir,"economic/data/shore_distance",sep=""))

png(filename=paste(boxdir,"economic/data/shore_distance.png",sep=""))

plot(final_shore,main="Distance to shore (m)",col=rev(terrain.colors(55)))

dev.off()


