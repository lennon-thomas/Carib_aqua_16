library(raster)
library(rgdal)
library(sp)
library(ncdf4)

#Define study region
ext<-c(-87.29167,-57.04167,7.375,30.16667)

##download STRM30 topo30.grd file at http://topex.ucsd.edu/WWW_html/srtm30_plus.html to rasterize EEZ shapefile
filename<-system.file("external/test.grd",package="raster")
f<-raster(filename)
depth<-raster("raw/topo30.grd")
depth<-rotate(depth) #rotates to correct projection
carib_depth<-crop(depth,ext,progress='text')
carib_depth <- calc(carib_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename='tmp/carib_depth.tif')
carib_depth<-raster("Suitability/tmp/carib_depth.tif")

#EEZs downloaded at: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")
carib_eez<-crop(EEZ,ext,progress='text')
writeOGR(carib_eez, dsn="tmp",driver="ESRI Shapefile", layer="carib_eez_shape")
carib_eez_raster<-rasterize(carib_eez,carib_depth,field=carib_eez$ID)  

#countries<-countries[!duplicated(countries), ]
noncountries<-c(108,164,129,130,131,132,133,135,139,163,167,255,161,260,165) 
countries<-subset(countries,!(ID %in% noncountries))
countries<-countries[order(countries$ID),]

countries<-countries[countries$Country!="Martinique",]
countries<-countries[countries$Country!="Virgin Islands of the United States",]
carib_eez_raster_mask <- mask(carib_eez_raster,carib_depth,progress='text',filename='tmp/carib_eez_ocean.tif',overwrite=T)
carib_eez_raster_mask<-raster('Suitability/tmp/carib_eez_ocean.tif')
eez_data<-as.data.frame(carib_eez_raster_mask)
countries<-eez_data[,c(1,4)]

#calculate area per cell
a<-area(carib_eez_raster_mask,na.rm=TRUE)

#Group areas by EEZ and sum
areas<-as.data.frame(zonal(a,carib_eez_raster_mask,fun='sum'))
colnames(areas)<-c("Country","EEZ area (km^2)")
final_area<-areas[order(areas$Country),]
#noncountries<-c(108,164,129,130,131,132,133,135,139,163,167,255,165)           
final_area<-subset(areas,!(Country %in% noncountries))
View(final_area)

##Add country names to dataframe
final_area<-cbind(countries,final_area)
final_area<-final_area[,-c(1,3)]

#read in suitable cell layer
s<-raster("Suitability/final/Suitable areas.tif")
is.na(s) <- !s
plot(s)

#find areas of suitable cells
b<-area(s,na.rm=TRUE)
extent(b)<-c(-87.29167, -57.04167, 7.375, 30.16667)

#sum areas of suitable cells by EEZ
s_areas<-as.data.frame(zonal(b,carib_eez_raster_mask,fun='sum')) 
colnames(s_areas)<- c("ID","Suitable_area")

#remove countries that aren't included in analysis
s_areas<-subset(s_areas,!(s_areas$ID %in% noncountries))

#column of total area and area that is suitable by EEZ
total_areas<-cbind(final_area,s_areas)

#Gulf of Mexico Suitable Area
gom_area<-35643 #km^2

#Gulf of Mexico production capacity
gom_prod<-29000 #metric tons

#Gulf of Mexico production per unit area
gom_prod_per_area<-gom_prod/gom_area #mt/km
#gom_prod_per_area<-813.36
gom_eez<-98629
gom_prod2<-gom_prod/gom_eez
colnames(total_areas)<-c("Country","EEZ_area","ID","Suitable_area")


##Make data frame calculating eco capacity 
library(dplyr)
total_area_info<-total_areas %>%
  mutate(Prop_suitable_EEZ = Suitable_area/EEZ_area*100)%>%
  mutate(Prod_Eco_capacity = gom_prod_per_area*Suitable_area)%>%
  mutate(prod_capacity2= gom_prod2*EEZ_area)
write.csv("final/capacity by area.csv")
View(total_area_info)


###EEZ area by country
EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="World_EEZ_v8_2014_HR")
#EEZ_data<-as.data.frame(EEZ)

filename<-system.file("external/test.grd",package="raster")
f<-raster(filename)
depth<-raster("Suitability/raw/topo30.grd")
depth<-rotate(depth) 



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
