rm(list = ls())

library(raster)
library(rgdal)
library(ncdf4)
library(maptools)
library(utils)
library(RNetCDF)
library(rgeos)





dir<-"Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability/raw/ICUN MPA"
poly_layer<-"caribmpa_polygons"
mpa_data_poly<-readOGR(dsn=dir,layer=poly_layer)
plot(mpa_data_poly)

#Remove MPAs where MARINE = 0 bc they are designated terrestrial protected areas
mpa_poly<-mpa_data_poly[mpa_data_poly@data$MARINE!="0",]
#mpa_poly<-mpa_poly[mpa_poly@data$MARINE!="1",]

#Remove MPAs where Status is proposed
mpa_poly<-mpa_poly[mpa_poly@data$STATUS!="Proposed",]

##rasterize shapefile
depth<-raster("carib_depth.tif")
carib_poly_raster<-rasterize(mpa_poly,depth,field="MARINE",progress='text',filename="carib_poly_raster.tif",overwrite=TRUE)

##assign 0 to cells with MPA and 1 to cells with no MPAs
carib_poly_raster[carib_poly_raster>0]<-0
carib_poly_raster[is.na(carib_poly_raster)]<-1

##Add a land mask
carib_poly_raster<-mask(carib_poly_raster,depth,maskvalues=NA,filename="MPA poly raster final.tif",overwrite=TRUE)
poly<-raster("MPA poly raster final.tif")
plot(carib_poly_raster,col=c("red","lightblue"))

#####Now rasterize point file#############
###
#############################################################################

points<-raster("mpapoints2.tif")
plot(points)
##Convert terrestrial protected areas to NA
points[points==1]<-NA


##resample
resample(points,depth,file="MPA point_1k.tif",method="bilinear",overwrite=TRUE,progress='text')
points<-raster("MPA point_1k.tif")
plot(points)
##change points that are 1 and 2 (MPAs) to 0 and NA to 1
points[points==2]<-0
points[points==3]<-0
points[is.na(points)]<-1

#add land mask
mask(points,depth,maskvalue=NA,inverse=FALSE, file="MPA point_final.tif",overwrite=TRUE)
points<-raster("MPA point_final.tif")
plot(points,col=c("red","lightblue"))

## now try adding points to polygon layer

poly<-raster("MPA poly raster final.tif")
plot(points)
points[points==0]<-NA
UNEP_MPA<-mask(poly,points,maskvalue=NA,inverse=FALSE)
UNEP_MPA[is.na(UNEP_MPA)]<-0
UNEP_MPA<-mask(UNEP_MPA,depth,inverse=FALSE,maskvalue=NA)
plot(UNEP_MPA,col=c("purple","lightblue"),add=TRUE)

## Bring in WRI MPA
WRI<-raster("MPA_1km.tif")
plot(WRI)
WRI[is.na(WRI)]<-0
WRI[WRI>0]<-3
WRI[9000:12000]
###Create final w both files combined
final_mpa<-mask(UNEP_MPA,WRI,maskvalue=NA,inverse=FALSE)
plot(final_mpa)
final_mpa[is.na(final_mpa)]<-2
final_mpa<-mask(final_mpa,depth,maskvalue=NA,inverse=FALSE)
plot(final_mpa,col=c("red","lightblue","purple"))
writeRaster(final_mpa,"final_mpa_layer.tif")

###view overlap between layers
UNEP_MPA[UNEP_MPA==1]<-NA
UNEP_MPA[UNEP_MPA==0]<-2
UNEP_MPA[is.na(UNEP_MPA)]<-0
s<-stack(UNEP_MPA,WRI)
t<-calc(s,fun=sum)
t<-mask(t,depth)
plot(t,col=c("lightblue","orange","purple","darkred"),legend=FALSE)
legend("topright",title="MPA data", c("none","WDPA","WRI","WDPA and WRI"),fill=c(c("lightblue","orange","purple","darkred")))
