#Oct 18,2016
#This file calculates the total area in each EEZ that is < 100 m depth and then matches this with 
#aquaculture production per EEZ to determine the average production per unit area of suitable aquaculture space.


library(raster)
library(rgdal)
library(sp)
library(ncdf4)
library(dplyr)


##download STRM30 topo30.grd file at http://topex.ucsd.edu/WWW_html/srtm30_plus.html. This file has already been rotated

depth<-raster('tmp/world_depth.tif')


#Longdill, Healy and Black (2008) determined up to 100 m
depth=calc(depth,fun=function(x){ifelse(x<=100,x,NA)},progress='text',filename='tmp/world_suitable_depth.tif')


#EEZs downloaded at: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")

EEZ_raster<-rasterize(EEZ,depth,field=EEZ$EEZ_ID,filename='tmp/world_eez_raster.tif',progress='text')  

eez_data<-as.data.frame(EEZ)

countries<-eez_data[,c(4,8)]
countries<-countries[order(countries$EEZ_ID),]

names(countries)
a<-area(depth,na.rm=TRUE)
length(EEZ_raster)
areas<-zonal(a,EEZ_raster,fun='sum',na.rm=TRUE)
colnames(areas)<-c("Country","EEZ suitable area (km^2)")
final_area<-areas[order(areas$Country),]
unique(EEZ_raster)
nrow(final_area)

final_area<-cbind(countries,final_area)
final_area<-final_area[,-c(1,3)]
View(final_area)

write.csv(final_area,"final_area.csv")
#read in suitable cell layer
s<-raster("Suitability/final/Suitable areas.tif")
is.na(s) <- !s
plot(s)
b<-area(s,na.rm=TRUE)
extent(b)<-c(-87.29167, -57.04167, 7.375, 30.16667)
s_areas<-as.data.frame(zonal(b,carib_eez_raster_mask,fun='sum')) 
colnames(s_areas)<- c("ID","Suitable_area")
s_areas<-subset(s_areas,!(s_areas$ID %in% noncountries))
total_areas<-cbind(final_area,s_areas)
gom_area<-35643 #km^2
gom_prod<-29000 #metric tons
gom_prod_per_area<-gom_prod/gom_area #mt/km
#gom_prod_per_area<-813.36
gom_eez<-98629
gom_prod2<-gom_prod/gom_eez
colnames(total_areas)<-c("Country","EEZ_area","ID","Suitable_area")

library(dplyr)
total_area_info<-total_areas %>%
  mutate(Prop_suitable_EEZ = Suitable_area/EEZ_area*100)%>%
  mutate(Prod_Eco_capacity = gom_prod_per_area*Suitable_area)%>%
  mutate(prod_capacity2= gom_prod2*EEZ_area)
write.csv("final/capacity by area.csv")
View(total_area_info)


###EEZ area by country
EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")
EEZ_data<-as.data.frame(EEZ)
View(EEZ_data)
EEZ_data<-EEZ_data[,c(3:5,12)]
View(EEZ_data)
write.csv(EEZ_data, "world_eez2.csv")
aqua<-read.csv("aqua_prod.csv",stringsAsFactors = FALSE)
EEZ_aqua<-merge(EEZ_data,aqua,by="Country",all.y=TRUE,all.x=TRUE)
EEZ_aqua<- as.data.frame(sapply(EEZ_aqua,gsub,pattern="F",replacement=""))
EEZ_aqua$X2010<-as.numeric(as.character(EEZ_aqua$X2010))
EEZ_aqua$X2011<-as.numeric(as.character(EEZ_aqua$X2011))
EEZ_aqua$X2012<-as.numeric(as.character(EEZ_aqua$X2012))
EEZ_aqua$X2013<-as.numeric(as.character(EEZ_aqua$X2013))
EEZ_aqua$X2014<-as.numeric(as.character(EEZ_aqua$X2014))

EEZ_aqua<-EEZ_aqua%>%
  mutate(avg_prod=rowMeans(cbind(X2010,X2011,X2012,X2013,X2014),na.rm=TRUE))

write.csv(EEZ_aqua,"EEZ_aqua.csv")

#Total suitaible area by country
##############
# filename<-system.file("external/test.grd",package="raster")
#  f<-raster(filename)
#  depth<-raster("Suitability/raw/topo30.grd")
depth<-raster("Suitability/raw/topo30.tif")
depth<-rotate(depth) #rotates to correct projection
plot(depth)

depth = calc(depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename='tmp/world_depth.tif')
depth<-raster('tmp/world_depth.tif')
depth<-depth[depth>100]<-NA



# memory.limit
 memory.limit(size=18000)
# depth<-raster('tmp/world_depth.tif')
writeRaster(depth[depth>100]<-NA,"world_suitable.tif")

#plot(depth)

#Longdill, Healy and Black (2008) determined up to 100 m
#writeRaster(depth[depth>100]<-NA,"suitable_world_depth.tif")
rm(list = ls())

gc()
d_area<-area(depth,na.rm=TRUE)
EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")
