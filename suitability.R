# Date: 5/24/17
# Author: Lennon Thomas

########################################################################
## This creates a spatial layer of 1km2 cells that may be suitable 
# for cobia aquaculture, and estimates the number of suitable cells
# in each EEZ and accounted for by each factor
#######################################################################

rm(list = ls())
library(raster)
library(rgdal)

boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assign a zero to international waters and 1 to waters inside EEZ

raster_eez<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

png(file=paste(boxdir,"Suitability/plots/suitable_eez.png",sep=""))

plottitle<-("suitable_eez")

plot(raster_eez,col="lightblue")

dev.off()

raster_eez[raster_eez > 0]<- 1

writeRaster(raster_eez,paste(boxdir,"Suitability/final/suitable_eez.tif",sep=""),overwrite = TRUE)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns depth values to either a 1 (between 25-100 m depth) or 0

carib_depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep=""))

carib_depth<-mask(carib_depth,raster_eez)

carib_depth[carib_depth<25]<-0

carib_depth[carib_depth>100]<-0

carib_depth<-mask(carib_depth,land,inverse=FALSE,maskvalues=NA)

png(file=paste(boxdir,"Suitability/plots/suitable_depth.png",sep=""))

plottitle<-("Depth from 25-100 m")

plot(carib_depth,main=plottitle,legend.args=list(text="Depth (m)",side=3))

dev.off()

carib_depth[carib_depth!=0]<-1

writeRaster(filename=paste(boxdir,"Suitability/final/suitable_depth.tif",sep=""),overwrite=TRUE)
#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns cells with MPAs present a zero

MPA<-raster(paste(boxdir,'Suitability/tmp/final_mpa_layer.tif',sep=""))

MPA<-mask(MPA, raster_eez, maskvalue = 1)

png(file=paste(boxdir,"Suitability/plots/suitable_mpa.png",sep=""))

plottitle<-("MPA areas")

plot(MPA,main=plottitle,col="red")

dev.off()

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns cells either a 1 (< 1 m/s )  or 0 based on current velocity

u<-raster(paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/allyears_v.tif",sep=""))

v<-raster(paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/allyears_v.tif",sep=""))

s<-stack(u,v)

current<-calc(s,fun=function(x){max(abs(x),na.rm=FALSE)})

current[current>1]<-0

current[current==1]<-0

current<-mask(current,raster_eez)

png(file=paste(boxdir,"Suitability/plots/suitable_currents.png",sep=""))

plottitle<-("Current Velocities")

plot(current,main=plottitle,legend.args=list(text="Current Velocity (m/s)",side=3))

dev.off()

current[current!=0]<-1

writeRaster(current,paste(boxdir,"Suitability/final/final_carib_current.tif",sep=""),overwrite=TRUE)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns a 0 to cells with the top 10% of highest shipping activity

ships<-raster(paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""))

top<-unique(ships) #5949970

top<-sort(top,decreasing=TRUE)

num<-(length(top)*.10) #top 10% of shipping activity from Becca's paper

top_num<-top[1:num]

ship_threshold<-min(top_num)

ships[ships>ship_threshold]<-0 

ships[ships==ship_threshold]<-0


png(file=paste(boxdir,"Suitability/plots/suitable_shipping.png",sep=""))

plottitle<-("Shipping")

plot(ships,main=plottitle,legend.args=list(text="Shipping Activity",side=3))

dev.off()

ships[ships>0]<-1

writeRaster(ships,paste(boxdir,"Suitability/final/suitable_shipping.tif",sep=""),overwrite=TRUE)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns a 0 to cells with the benthic oil structures

oil<-raster(paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""))

oil[oil>0]<-0



png(file=paste(boxdir,"Suitability/plots/suitable_oil.png",sep=""))

plottitle<-("Oil Structure")

plot(oil,main=plottitle,legend.args=list(text="Oil Structure",side=3))

dev.off()

writeRaster(oil,paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""),overwrite=TRUE)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns a 0 to cells with coral reefs

coral<-raster(paste(boxdir,"Suitability/tmp/carib_coral_raster.tif",sep=""))

coral[coral==0]<-1

coral[coral==2]<-0

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Identify suitable cells based on all layers

s = stack(carib_depth,MPA,current,ships,raster_eez,oil,coral)

s<-stackApply(s,indices=c(1,1,1,1,1,1,1),fun=prod,na.rm=TRUE)

all[all==0]<-NA

all<-mask(s,raster_eez,maskvalues=NA,inverse=FALSE,filename=paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep=""),overwrite=TRUE)

saveRDS(all,paste(boxdir,"Suitability/final/suitable_areas.rds",sep=""))

suitable_area<-as.data.frame(area(all,na.rm=TRUE))

final_suitable_area<-sum(suitable_area,na.rm=TRUE)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Determine total area (km2) of area that was identified as suitable

study_area<-as.data.frame(area(raster_eez,na.rm = TRUE))

study_area<-sum(study_area,na.rm = TRUE)

suitable_areas<-as.data.frame(area(all,na.rm = TRUE))

total_suitable_area<-sum(suitable_areas,na.rm = TRUE)

percent_suitable<-total_suitable_area/study_area*100

#-----------------------------------------------------------------
#----------------------------------------------------------------
## Calculate suitable area by country

EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

country<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep = ""))

names<-data.frame(matrix(NA,nrow=33,ncol=2))

EEZ<-as.data.frame(EEZ)

c<-EEZ[,2:3]

cell_area<-area(all,na.rm=TRUE)

cells<-zonal(all,EEZ,fun='sum')

cells<-cells[-1,]

for (i in 1:nrow(cells)){
m<-as.integer(cells[i,1])
names[i,1]<-m
n<-c$rgn_name[c$rgn_id==m]
n<-as.character(n[1])
names[i,2]<-n
}

cells<-cbind(cells,names)
cells<-cells[,-3]
cells[order(cells$sum,decreasing=TRUE),]
write.csv(cells,"final/suitable area by country.csv")
# 

# ####tmaps######
# test<-select(country)
# library(tmap)
# data(World)
# 
# land = readOGR(dsn="C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/tmp/EEZ",layer="carib_eez_shape")
# land$la<-ifelse(land@data$Pol_type=="200NM","blue","orange")
# 
# suitable<-tm_shape(land)+
#   tm_fill("la")+
# tm_shape(all)+
#   tm_raster(showNA=FALSE,legend.show=FALSE)+
# tm_shape(land)+
#   tm_borders()
# 
# save_tmap(suitable,filename="datalayer plots/suitability.png")
# tm.layout(legend.position=c("right","top"))
# 
# 
