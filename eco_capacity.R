library(raster)
library(rgdal)
library(sp)
library(ncdf4)



final_area<-read.csv("Suitability/final/suitable area by country.csv")
final_area<-final_area[,c(2,3,4)]
#Gulf of Mexico Suitable Area
gom_area<-35643 #km^2

#Gulf of Mexico production capacity
gom_prod<-29000 #metric tons

#Gulf of Mexico production per unit area
gom_prod_per_area<-gom_prod/gom_area #mt/km
#gom_prod_per_area<-813.36 kg/km
gom_eez<-98629
gom_prod2<-gom_prod/gom_eez
colnames(final_area)<-c("Country","EEZ_area","Suitable_area")


##Make data frame calculating eco capacity 
library(dplyr)
total_area_info<-final_area %>%
  mutate(Prop_suitable_EEZ = Suitable_area/EEZ_area*100)%>%
  mutate(Gom__capacity_mt = gom_prod_per_area*Suitable_area)
 # mutate(prod_capacity2= gom_prod2*EEZ_area)
#write.csv("eco_capacity/gom_capacity_by_area.csv")
View(total_area_info)


###EEZ area by country
EEZ = readOGR(dsn="Suitability/raw/EEZ",layer="eez")
EEZ_data<-as.data.frame(EEZ[,c(1,3,8,22)])


depth<-raster("Suitability/raw/topo30.tif")
depth<-rotate(depth,filename="Suitability/tmp/rotated_topo30.tif",progress="text") 

write.csv(EEZ_data, "world_eez.csv")
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
