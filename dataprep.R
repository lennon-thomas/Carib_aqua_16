# Caribbean Aquaculture Dataprep
# all raw data files are located here:https://ucsb.app.box.com/files/0/f/12037510712/raw
rm(list = ls())


library(rgdal)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)


setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture") #work computer directory
boxdir<-('/Users/Lennon/Documents/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')
#-----------------------------------------------------------------
#----------------------------------------------------------------
# Depth data from: http://topex.ucsd.edu/WWW_html/srtm30_plus.html

depth = raster("Suitability/raw/topo30.tif")
depth <- rotate(depth,progress='text')
r_depth<-raster("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability/tmp/topo30.tif")
carib_depth<-crop(depth,ext)
carib_depth = calc(carib_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename='Suitability/tmp/carib_depth.tif')
carib_depth<-raster('Suitability/tmp/carib_depth.tif')

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# EEZs data from: http://www.marineregions.org/downloads.php

EEZ = readOGR(dsn=paste(boxdir,"Suitability/raw/EEZ",sep=""),layer="eez",stringsAsFactors=FALSE)

ext<-c(-87.29167,-57.04167,7.375,30.16667)

carib_eez<-crop(EEZ,ext,progress='text')

writeOGR(carib_eez, dsn=paste(boxdir,"Suitability/tmp",sep=""),driver="ESRI Shapefile", layer="carib_eez_shape",overwrite=TRUE)

carib_eez<-readOGR(dsn=paste(boxdir,"Suitability/tmp",sep=""), layer="carib_eez_shape")
carib_eez@data$PolygonID<-carib_eez@data$PolygonID[carib_eez@data$PolygonID==123]<-1000land<-readOGR( dsn="C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/tmp/EEZ",layer="carib_eez_shape2")
carib_eez$PolygonID<-as.numeric(as.character(carib_eez$PolygonID))
carib_eez_raster<-rasterize(carib_eez,carib_depth,field=carib_eez$PolygonID,progress='text')
carib_eez_raster_mask <- mask(carib_eez_raster,carib_depth,progress='text',filename='Suitability/tmp/carib_eez_ocean.tif',overwrite=T)
country<-as.vector(c(48,49,50,52,53,56,57,60,64,65,66,71,72,75,79,83,84,85,86,87,88,89,91,92,93,94,95,99,100,101,110,111,122,123))

carib_suitable_eez<-carib_eez_raster_mask[carib_eez_raster_mask %in% country==1]
carib_eez_raster_mask[carib_eez_raster_mask>1]<-0
plot(carib_suitable_eez)
writeRaster(carib_suitable_eez,paste(boxdir,"Suitability/tmp/suitable_eez.tif",sep="")
#-----------------------------------------------------------------
#----------------------------------------------------------------

# MPAs data from two sources:WDPA data and WRI 
#First step is WRI data prep ((http://www.wri.org/resource/marine-protected-areas-world)
MPA<-readOGR(dsn="Suitability/raw/WRI_MPA",layer="mpa_poly")
MPA<-crop(MPA,ext,progress='text')
MPA_raster<-rasterize(MPA,carib_depth,field=1,progress='text')
extent(MPA_raster)<-ext
plot(MPA_raster)
head(MPA_raster)
MPA_raster<-resample(MPA_raster,carib_depth,progress='text',filename='Suitability/tmp/WRI MPA_1km.tif')

#---------------------------------------------------------------------
#WDPA data prep (http://www.protectedplanet.net/) download both polygon and point shapefiles!
#First step is to open the WDPA shapefile polygon in arcmap and set the extent to the Caribbean 
# and then export as shapefile called "raw/ICUN MPA/caribmpa_polygons" (otherwise R will crash bc file is too big)
#This is where wri gets their data!
#dir<-"Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability/raw/ICUN MPA"
dir<-"Suitability/raw/ICUN MPA"
poly_layer<-"caribmpa_polygons"
mpa_data_poly<-readOGR(dsn=dir,layer=poly_layer)
plot(mpa_data_poly)

#Remove MPAs where MARINE = 0 bc they are designated terrestrial protected areas
mpa_poly<-mpa_data_poly[mpa_data_poly@data$MARINE!="0",]

#Remove MPAs where Status is proposed
#mpa_poly<-mpa_poly[mpa_poly@data$STATUS!="Proposed",]

#Rasterize shapefile
depth<-raster("Suitability/tmp/carib_depth.tif")
carib_poly_raster<-rasterize(mpa_poly,depth,field="MARINE",progress='text',filename="Suitability/raw/ICUN MPA/carib_poly_raster.tif",overwrite=TRUE)

#Assign 0 to cells with MPA and 1 to cells with no MPAs
carib_poly_raster[carib_poly_raster>0]<-0
carib_poly_raster[is.na(carib_poly_raster)]<-1

#Add a land mask
carib_poly_raster<-mask(carib_poly_raster,depth,maskvalues=NA,filename="Suitability/raw/ICUN MPA/MPA poly raster final.tif",overwrite=TRUE)
poly<-raster("raw/ICUN MPA/MPA poly raster final.tif")
plot(carib_poly_raster,col=c("red","lightblue"))

#-----------------------------------------------------------------------------------------------------------------
#Next step requires arcmap (still looking for an R solution!)
#First crop file to Caribbean extent, 
#Next use toolbox to change file from multipart to single part and save asSave as "raw/ICUN MPA/caribmpa_points2"### 
##Next use the point to raster to tool to rasterize the shape file (error when tried to rasterize in R). Output is "raw/ICUN/mpapoints2.tif"
points<-raster("Suitability/raw/ICUN MPA/mpapoints2.tif")
#plot(points)
##Convert terrestrial protected areas to NA
points[points==1]<-NA


##resample
resample(points,depth,file="raw/ICUN/MPA point_1k.tif",method="bilinear",overwrite=TRUE,progress='text')
points<-raster("MPA point_1k.tif")
#plot(points)

##change points that are 1 and 2 (MPAs) to 0 and NA to 1
points[points==2]<-0
points[points==3]<-0
points[is.na(points)]<-1

#add land mask
mask(points,depth,maskvalue=NA,inverse=FALSE, file="raw/ICUN MPA/MPA point_final.tif",overwrite=TRUE)
points<-raster("Suitability/raw/ICUN MPA/MPA point_final.tif")
plot(points,col=c("red","lightblue"))

##Now combine points and polygon files
## now try adding points to polygon layer

poly<-raster("Suitability/raw/ICUN MPA/MPA poly raster final.tif")
plot(points)
points[points==0]<-NA
UNEP_MPA<-mask(poly,points,maskvalue=NA,inverse=FALSE)
UNEP_MPA[is.na(UNEP_MPA)]<-0
UNEP_MPA<-mask(UNEP_MPA,depth,inverse=FALSE,maskvalue=NA,filename="Suitability/tmp/WDPA MPA")
plot(UNEP_MPA,col=c("purple","lightblue"),add=TRUE)

##Combine WRI and WDPA MPA layers
WRI<-raster("Suitability/tmp/MPA_1km.tif")
plot(WRI)
WRI[is.na(WRI)]<-0
WRI[WRI>0]<-3

###Create final w both files combined
final_mpa<-mask(UNEP_MPA,WRI,maskvalue=NA,inverse=FALSE)
plot(final_mpa)
final_mpa[is.na(final_mpa)]<-2
final_mpa<-mask(final_mpa,depth,maskvalue=NA,inverse=FALSE)
plot(final_mpa,col=c("red","lightblue","purple"))
writeRaster(final_mpa,"Suitability/tmp/final_mpa_layer.tif")

#------------------------------------------------------------------
#Thermocline layer data from: http://www.ifremer.fr/cerweb/deboyer/mld/Surface_Warm_Layer_Depth.php

thermo<-brick("Suitability/raw/topofthermocline.nc",varname="ttd",stopIfNotEqualSpaced=FALSE)
r_thermo<-rotate(thermo)
s_thermo<-crop(r_thermo,c(-125,0,0,50))
s_thermo[s_thermo>8e+08]<-NA
resample(s_thermo,depth,method="ngb",filename="raw/carib_thermo_1k.nc",varname="ttd",overwrite=TRUE)
carib_thermo<-brick("Suitabiliyt/raw/carib_thermo_1k.nc")
mask(carib_thermo,depth,maskvalue=NA,filename="Suitability/raw/carib_thermo_final.nc",overwrite=TRUE)
carib_thermo<-brick("Suitability/raw/carib_thermo_final.nc")

calc(carib_thermo,fun=function(x){min(abs(x),na.rm=FALSE)},file="tmp/min_thermo.tif",overwrite=TRUE)

final_min<-raster("tmp/min_thermo.tif")

cols<-(terrain.colors(250))
png(file="min thermocline depth.png")
plottitle<-("Minimum thermocline depth in 2015 (m)")
plot(final_min,col=cols,main=plottitle,legend.args=list(text="meters",side=3))
dev.off()

#-----------------------------------------------------------------------
#Current velocity data from: https://podaac.jpl.nasa.gov/dataset/OSCAR_L4_OC_third-deg
currentfiles<-list.files('Suitability/raw/currents/data/raw data',full.names=T, pattern="nc")
varname="v"


currentlist<-lapply(currentfiles,brick,varname=varname)
year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
output1<-paste("Suitability/raw/currents/data/tmp/",year,"_1c.nc")
for(i in seq_along(currentlist)){crop(currentlist[[i]],c(19.83333, 380.16667, -80.16667, 80.16667),file=output1[i],overwrite=TRUE)}



currentfiles<-list.files('Suitability/raw/currents/data/tmp/',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output2<-paste("Suitability/raw/currents/data/tmp/2c/",year,"_2c.nc")

for (i in 1:length(currentlist)){
  plot1<-crop(currentlist[[i]],c(19.33333,199.5000,-80.16767,80.16667))
  plot2<-crop(currentlist[[i]],c(199.5001,380.1667,-80.16667,80.16667))
  plot2<-shift(plot2,x=-360)
  whole<-merge(plot1,plot2)
  crop_whole<-crop(whole,c(-90,-55,0,40),filename=output2[i],overwrite=TRUE)
  print(i)
}

rm(list = ls())

year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
currentfiles<-list.files('Suitability/raw/currents/data/tmp/2c/',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output3<-paste("Suitability/raw/currents/data/tmp/1k/",year,"_1km.nc")
depth<-raster("Suitability/tmp/carib_depth.tif")

for(i in seq_along(currentlist)){resample(currentlist[[i]],depth,method="ngb",file=output3[i],
                                          overwrite=TRUE)}

currentfiles<-list.files('Suitability/raw/currents/data/tmp/1k',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)
output5<-paste("Suitability/raw/currents/data/final_v/",year,"_max.tif")


for(i in seq_along(currentlist)){calc(currentlist[[i]],fun=function(x){max(abs(x),na.rm=TRUE)},file=output5[i],overwrite=TRUE)}



currentfiles<-list.files('Suitability/raw/currents/data/final_v/',full.names=T,pattern='tif')
currentlist<-lapply(currentfiles,raster)
all<-brick(currentlist)
max_u<-calc(all,fun=function(x){max(abs(x),na.rm=TRUE)})

writeRaster(max_u,"Suitability/raw/currents/data/final_v/final/allyears_v.tif")

###run interpolation here using OA_interpolation.py file.

###Read in interpolated file and add land mask
max_u<-raster("Suitability/raw/currents/data/final_v/final/allyears_v_int.tif")
plot(max_u)
depth<-raster("Suitability/tmp/carib_depth.tif")
final_max_v<-mask(max_u,depth,maskvalue=NA,inverse=FALSE)
plot(final_max_v,col=cols)


##Write final raster and plot
writeRaster(final_max_v,"Suitability/raw/currents/data/final_v/final/final_max_int_v.tif")

cols<-(terrain.colors(250))
png(file="Suitability/raw/currents/data/final_v/final/max_v_final_plot.png")
plottitle<-("Absolute Maximum Meridional Velocity\n 2006-2015")
plot(final_max_v,col=cols,main=plottitle,legend.args=list(text="m/s",side=3))
dev.off()

currentfiles<-list.files('Suitability/raw/currents/data/raw data/',full.names=T, pattern="nc")
varname="u"
#varname="v"

currentlist<-lapply(currentfiles,brick,varname=varname)
year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
output1<-paste("Suitability/raw/currents/data/tmp/",year,"_1c.nc")
for(i in seq_along(currentlist)){crop(currentlist[[i]],c(19.83333, 380.16667, -80.16667, 80.16667),file=output1[i],overwrite=TRUE)}



currentfiles<-list.files('Suitability/raw/currents/data/tmp/',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output2<-paste("Suitability/raw/currents/data/tmp/2c/",year,"_2c.nc")

for (i in 1:length(currentlist)){
  plot1<-crop(currentlist[[i]],c(19.33333,199.5000,-80.16767,80.16667))
  plot2<-crop(currentlist[[i]],c(199.5001,380.1667,-80.16667,80.16667))
  plot2<-shift(plot2,x=-360)
  whole<-merge(plot1,plot2)
  crop_whole<-crop(whole,c(-90,-55,0,40),filename=output2[i])
  print(i)
}

rm(list = ls())

year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
currentfiles<-list.files('Suitability/raw/currents/data/tmp/2c/',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output3<-paste("Suitability/raw/currents.data/tmp/1k/",year,"_1km.nc")
depth<-raster("Suitability/tmp/carib_depth.tif")

for(i in seq_along(currentlist)){resample(currentlist[[i]],depth,method="ngb",file=output3[i],
                                          overwrite=TRUE)}

currentfiles<-list.files('Suitability/raw/currents/data/tmp/1k',full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)
plot(currentlist[[1]])
#####Run interpolation here######
#currentfiles<-list.files('data2/tmp/interpolated')
#currentlist<-lapply(currentfiles,brick)
# 
# depth<-raster("carib_depth.tif")
# output4<-paste("data2/tmp/mask/",year,"masked.nc")
# for(i in seq_along(currentlist)){mask(currentlist[[i]],depth,maskvalue=NA, inverse=FALSE, file=output4[i],overwrite=TRUE)}
# 
# currentfiles<-list.files('data2/tmp/mask')
# currentlist<-lapply(currentfiles,brick)

output5<-paste("Suitability/raw/currents/data/final_u/",year,"_max.tif")
#output5<-paste("data2/final_v",year,"_max.tif")

for(i in seq_along(currentlist)){calc(currentlist[[i]],fun=function(x){max(abs(x),na.rm=TRUE)},file=output5[i],overwrite=TRUE)}



currentfiles<-list.files('Suitability/raw/currents/data/final_u/',full.names=T,pattern='tif')
currentlist<-lapply(currentfiles,raster)
all<-brick(currentlist)
max_u<-calc(all,fun=function(x){max(abs(x),na.rm=TRUE)})


writeRaster(max_u,"Suitability/raw/currents/data/final_u/final/2005_u.tif")

###run interpolation here using OA_interpolation.py file.

###Read in interpolated file and add land mask
max_u<-raster("Suitability/raw/currents/data/final_u/final/2005_u_int.tif")
plot(max_u)
depth<-raster("Suitability/tmp/carib_depth.tif")
final_max_u<-mask(max_u,depth,maskvalue=NA,inverse=FALSE)

plot(final_max_u)
writeRaster(final_max_u,"Suitability/raw/currents/data/final_u/final/final_max_int_u.tif")
cols<-(terrain.colors(250))
png(file="Suitability/raw/currents/data/final_u/final/max_u_final_plot.png")
plottitle<-("Absolute Maximum Zonal Velocity\n 2006-2015")
plot(final_max_u,col=cols,main=plottitle,legend.args=list(text="m/s",side=3))
dev.off()

#----------------------------------------------------------------------------
##Shipping data from: https://www.nceas.ucsb.edu/globalmarine2008/impacts

ships<-raster("Suitability/raw/shipping_lzw.tif")
crs(ships)<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

projectRaster(ships,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",filename="Suitability/tmp/ships_proj.tif")
ships2<-raster("Suitability/tmp/ships_proj.tif")

resample(ships2,world,method="ngb",filename="Suitability/tmp/ship_1k.tif")
ship1k<-raster("ship_1k.tif")

crop(ship1k,depth,filename="carib_ship.tif")
caribship<-raster("carib_ship.tif")
plot(caribship)

#-----------------------------------------------------------------------------
## Oil rig data from:https://www.nceas.ucsb.edu/globalmarine2008/impacts
carib_depth<-raster("C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/tmp/carib_depth.tif")
oil2<-raster("C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/2013stablelights.tif")
oil2<-crop(oil2,carib_depth,filename="C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/tmp/carib_oil2.tif",overwrite=TRUE)
oil2<-mask(oil2,carib_depth,maskvalue=NA,inverse=FALSE)
plot(oil2)

oil<-raster("Suitability/raw/oil_lzw.tif")
crs(oil)<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +no_defs"
projectRaster(oil,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",filename="Suitability/raw/oil_proj.tif")
oil2<-raster("Suitability/raw/oil_proj.tif")
plot(oil,col="black")
crop(oil, carib_depth,filename="Suitability/tmp/carib_oil.tif",overwrite=TRUE)
oil<-raster("tmp/carib_oil.tif")

resample(oil,carib_depth,method="ngb",filename="tmp/carib_oil_1k.tif",overwrite=TRUE)
oil<-raster("Suitability/tmp/carib_oil_1k.tif")
oil[is.na(oil)]<-0
mask(oil,carib_depth,maskvalue=NA,filename="Suitability/final/suitable_oil.tif",overwrite=TRUE)
oil<-raster("Suitability/final/suitable_oil.tif")
plot(oil)


## Coral reefs data from: http://data.unep-wcmc.org/datasets/1
coral<-readOGR(dsn="Suitability/raw/coral/01_Data", layer="14_001_WCMC008_CoralReef2010_v1_3")
carib_depth<-raster("Suitability/tmp/carib_depth.tif")
carib_coral<-crop(coral,ext,progress='text',filename='Suitability/tmp/carib_coral.tif')
carib_coral@data$COV_TYPE<-as.character(carib_coral@data$COV_TYPE)
coral_raster<-rasterize(carib_coral,carib_depth,field=2,progress='text')
coral_raster[is.na(coral_raster)]<-0
coral_raster<-mask(coral_raster,carib_depth,maskvalue=NA,inverse=FALSE,filename="Suitability/tmp/carib_coral_raster.tif",overwrite=TRUE)
