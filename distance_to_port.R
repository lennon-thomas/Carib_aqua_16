#Calculating distance from each cell in study area to nearest port
# 6/17/17

rm(list = ls())
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(spdep)
library(igraph)
boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

study_area<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

ext<-extent(study_area)

ports<-readOGR(dsn=paste(boxdir,"economic/data/port_index",sep=""),layer="WPI")
                  
carib_ports<-crop(ports,ext,progress = "text") 
carib_ports@coords

lon<-carib_ports@coords[,1]
lat<-carib_ports@coords[,2]
port<-as.data.frame(carib_ports@data$INDEX_NO)

sp=SpatialPoints(cbind(lon,lat))
coords= cbind(lon,lat)
spdf= SpatialPointsDataFrame(coords,port)
spdf= SpatialPointsDataFrame(sp,port)
coordinates(port)=cbind(lon,lat)

spdf[!is.na(spdf)]<-500



carib_ports_raster<-rasterFromXYZ(spdf,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
carib_ports_raster_resamp<-resample(carib_ports_raster,study_area)
#carib_ports_raster_resamp[is.na(carib_ports_raster_resamp)]<-10000
#carib_ports_raster_resamp[carib_ports_raster_resamp<10000]<-500
ports<-mask(study_area,carib_ports_raster_resamp,maskvalue=500,updatevalue=500)
plot(ports)

port_list<-Which(ports==500,cell=TRUE,na.rm=TRUE)

dgrid<-gridDistance(ports,origin = 500, omit=NA)

dgrid[dgrid==0]<--500


port_point<-rasterToPoints(carib_ports_raster_resamp,fun=function(x){x==500},spatial=TRUE)
plot(port_point,col='red',size=5)


overlap<-extract(study_area,port_point,cellnumbers=TRUE,buffer=10000)
length(overlap)
study_area[overlap$cell]<-500

plot(dgrid)
r <- raster(ncol=10,nrow=10)
r[] <- 1
r[48] <- 2
r[66:68] <- 3
d <- gridDistance(r,origin=2,omit=3) 
plot(d)

