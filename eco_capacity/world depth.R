#Oct 18,2016
#This file calculates the total area in each EEZ that is < 100 m depth and then matches this with 
#aquaculture production per EEZ to determine the average production per unit area of suitable aquaculture space.

library(raster)
library(rgdal)
library(sp)
library(ncdf4)
library(dplyr)


##download STRM30 topo30.grd file at http://topex.ucsd.edu/WWW_html/srtm30_plus.html. This file has already been rotated

depth<-raster('Suitability/tmp/world_depth.tif')


#Longdill, Healy and Black (2008) determined up to 100 m
#depth=calc(depth,fun=function(x){ifelse(x<=100,x,NA)},progress='text',filename='Suitability/tmp/world_suitable_depth.tif')
depth<-raster('Suitability/tmp/world_suitable_depth.tif')

#EEZs downloaded at: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="eez")

EEZ_raster<-rasterize(EEZ,depth,field=EEZ$PolygonID,filename='Suitability/tmp/world_eez_raster.tif',progress='text',overwrite=TRUE,background=NA)  
EEZ_raster<-raster('Suitability/tmp/world_eez_raster.tif')
eez_data<-as.data.frame(EEZ)
eez_data<-eez_data[order(eez_data$Territory1),]

a<-area(depth,na.rm=TRUE)
areas<-zonal(a,EEZ_raster,fun='sum',na.rm=TRUE)
areas<-as.data.frame(areas)

countries<-eez_data[,c(1,8,22)]
countries<-countries[order(countries$PolygonID),]
remv<-countries[!countries$PolygonID%in%areas$zone,]
countries<-countries[!countries$PolygonID %in% remv$PolygonID,]

final_area<-cbind(countries,areas)
View(final_area) #Check to make sure PolygonIDs match

final_area<-final_area[,-c(1,4)]

final_area<-final_area%>%
  group_by(Sovereign1)%>%
  summarise(suitable_area=sum(sum),total_area=sum(Area_km2))
 #Write file that contains total area and total area <100 m by EEZ                      
write.csv(final_area,"final_area.csv")

prod_data<-read.csv("eco_capacity/aqua_prod.csv",stringsAsFactors=FALSE)
prod_data$X2010<-as.numeric(prod_data$X2010)
prod_data$X2011<-as.numeric(prod_data$X2011)
prod_data$X2012<-as.numeric(prod_data$X2012)
prod_data$X2013<-as.numeric(prod_data$X2013)
prod_data$X2014<-as.numeric(prod_data$X2014)
  
prod_data<-prod_data%>%
  mutate(avg_prod=mean(c(X2010,X2011,X2012,X2013,X2014),na.rm=TRUE))



all_data<-merge(prod_data,final_area,by.x="Country",by.y="Sovereign1",all.x=TRUE,all.y=TRUE)

View(all_data)

write.csv(all_data,'eco_capacity/prod_suit_depth.csv')




# all<-test%>%
#   group_by(Sovereign)%>%
#   summarise(EEZ=sum(Area_m2),av_prod=mean(avg_prod),suitable=sum(sum))
# 
# View(all)
# 
# 
# nrow(test)
# View(test)
# #read in suitable cell layer
# s<-raster("Suitability/final/Suitable areas.tif")
# is.na(s) <- !s
# plot(s)
# b<-area(s,na.rm=TRUE)
# extent(b)<-c(-87.29167, -57.04167, 7.375, 30.16667)
# s_areas<-as.data.frame(zonal(b,carib_eez_raster_mask,fun='sum')) 
# colnames(s_areas)<- c("ID","Suitable_area")
# s_areas<-subset(s_areas,!(s_areas$ID %in% noncountries))
# total_areas<-cbind(final_area,s_areas)
# gom_area<-35643 #km^2
# gom_prod<-29000 #metric tons
# gom_prod_per_area<-gom_prod/gom_area #mt/km
# #gom_prod_per_area<-813.36
# gom_eez<-98629
# gom_prod2<-gom_prod/gom_eez
# colnames(total_areas)<-c("Country","EEZ_area","ID","Suitable_area")
# 
# library(dplyr)
# total_area_info<-total_areas %>%
#   mutate(Prop_suitable_EEZ = Suitable_area/EEZ_area*100)%>%
#   mutate(Prod_Eco_capacity = gom_prod_per_area*Suitable_area)%>%
#   mutate(prod_capacity2= gom_prod2*EEZ_area)
# write.csv("final/capacity by area.csv")
# View(total_area_info)
# 
# 
# ###EEZ area by country
# g
# 
# write.csv(EEZ_aqua,"EEZ_aqua.csv")
# 
# #Total suitaible area by country
# ##############
# # filename<-system.file("external/test.grd",package="raster")
# #  f<-raster(filename)
# #  depth<-raster("Suitability/raw/topo30.grd")
# depth<-raster("Suitability/raw/topo30.tif")
# depth<-rotate(depth) #rotates to correct projection
# plot(depth)
# 
# depth = calc(depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename='tmp/world_depth.tif')
# depth<-raster('tmp/world_depth.tif')
# depth<-depth[depth>100]<-NA
# 
# '/
# 
# # memory.limit
#  memory.limit(size=18000)
# # depth<-raster('tmp/world_depth.tif')
# writeRaster(depth[depth>100]<-NA,"world_suitable.tif")
# 
# #plot(depth)
# 
# #Longdill, Healy and Black (2008) determined up to 100 m
# #writeRaster(depth[depth>100]<-NA,"suitable_world_depth.tif")
# rm(list = ls())
# 
# gc()
# d_area<-area(depth,na.rm=TRUE)
# EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")
