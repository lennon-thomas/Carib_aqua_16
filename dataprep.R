# Caribbean Aquaculture Dataprep
# all raw data files are located here:https://ucsb.app.box.com/files/0/f/12037510712/raw
rm(list = ls())


library(rgdal)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)


#setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture") #work computer directory
boxdir<- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# EEZs data from: http://www.marineregions.org/downloads.php
#EEZ<-readOGR('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/World_EEZ_v9_20161021/',layer="eez")


EEZ <- readOGR(dsn=paste0(boxdir,'Suitability/raw/World_EEZ_v9_20161021'),layer="eez",stringsAsFactors=FALSE)

EEZ_bounds <- readOGR(dsn=paste0(boxdir,'Suitability/raw/World_EEZ_v9_20161021'),layer="eez_boundaries",stringsAsFactors=FALSE)

carib_countries<-c("Anguilla Exclusive Economic Zone",
                   "Antigua and Barbuda Exclusive Economic Zone",
                   "Aruban Exclusive Economic Zone",
                   "Bahamas Exclusive Economic Zone",
                   "Barbados Exclusive Economic Zone",
                   "Bonaire Exclusive Economic Zone",
                   "British Virgin Islands Exclusive Economic Zone",
                   "Cayman Islands Exclusive Economic Zone",
                   "Curaçaoan Exclusive Economic Zone",
                   "Cuban Exclusive Economic Zone",
                   "Dominican Exclusive Economic Zone",
                   "Dominican Republic Exclusive Economic Zone",
                   "Grenadian Exclusive Economic Zone",
                   "Guadeloupean Exclusive Economic Zone",
                   "Haitian Exclusive Economic Zone",
                   "Jamaican Exclusive Economic Zone",
                   "Martinican Exclusive Economic Zone",
                   "Montserrat Exclusive Economic Zone",
                   "Puerto Rican Exclusive Economic Zone",
                   "Saba Exclusive Economic Zone",
                   "Saint-Barthélemy Exclusive Economic Zone",
                   "Sint-Eustatius Exclusive Economic Zone",
                   "Saint Kitts and Nevis Exclusive Economic Zone",
                   "Saint Lucia Exclusive Economic Zone",
                   "Saint-Martin Exclusive Economic Zone",
                   "Saint Vincent and the Grenadines Exclusive Economic Zone",
                   "Sint-Maarten Exclusive Economic Zone",
                   "Trinidad and Tobago Exclusive Economic Zone",
                   "Turks and Caicos Exclusive Economic Zone",
                   "Virgin Islander Exclusive Economic Zone")

carib_eez<- EEZ[EEZ$GeoName %in% carib_countries,]

carib_bounds<-EEZ_bounds[EEZ_bounds$EEZ1 %in% carib_countries,]

writeOGR(carib_eez, dsn=paste(boxdir,"Suitability/tmp",sep=""),driver="ESRI Shapefile", layer="carib_eez_shape",overwrite=TRUE)

carib_eez<-crop(EEZ,carib_depth, progress='text')

#carib_eez<-readOGR(dsn=paste(boxdir,"Suitability/tmp",sep=""), layer="carib_eez_shape")

carib_eez_raster<-rasterize(carib_eez,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""),overwrite = TRUE)

carib_eez_raster<-mask(carib_eez_raster,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""),overwrite = TRUE)

carib_eez_raster<-rasterize(carib_eez,carib_depth,field=carib_eez$PolygonID)
#-----------------------------------------------------------------
#----------------------------------------------------------------
# Depth data from: http://topex.ucsd.edu/WWW_html/srtm30_plus.html

depth = raster(paste(boxdir,"Suitability/raw/topo30.tif",sep=""))
depth <- rotate(depth,progress='text')
#r_depth<-raster("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability/tmp/topo30.tif")
ext<-c(-86.93966 ,-56.00158, 9.831944 ,30.37239)
carib_depth<-crop(depth,ext)
carib_depth<-mask(carib_depth,carib_eez)
carib_depth = calc(carib_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename=paste(boxdir,'Suitability/tmp/carib_depth.tif',sep=""),overwrite=TRUE)
carib_depth<-raster(paste(boxdir,'Suitability/tmp/carib_depth.tif',sep=""))

#-----------------------------------------------------------------
#----------------------------------------------------------------

# MPAs data from two sources:WDPA data and WRI 
#First step is WRI data prep ((http://www.wri.org/resource/marine-protected-areas-world)
MPA<-readOGR(dsn=paste(boxdir,"Suitability/raw/WRI_MPA",sep=""),layer="mpa_poly")
MPA<-crop(MPA,ext,progress='text')
MPA_raster<-rasterize(MPA,carib_depth,field=1,progress='text')
extent(MPA_raster)<-ext
MPA_raster<-mask(MPA_raster,carib_eez)
head(MPA_raster)
MPA_raster<-resample(MPA_raster,carib_depth,progress='text',filename=paste(boxdir,'Suitability/tmp/WRI MPA_1km.tif',sep=""),overwrite=TRUE)

#---------------------------------------------------------------------
#WDPA data prep (http://www.protectedplanet.net/) download both polygon and point shapefiles!
#First step is to open the WDPA shapefile polygon in arcmap and set the extent to the Caribbean 
# and then export as shapefile called "raw/ICUN MPA/caribmpa_polygons" (otherwise R will crash bc file is too big)
#This is where wri gets their data!
#dir<-"Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability/raw/ICUN MPA"
dir<-paste(boxdir,"Suitability/raw/ICUN MPA",sep="")
poly_layer<-"caribmpa_polygons"
mpa_data_poly<-readOGR(dsn=dir,layer=poly_layer)


#Remove MPAs where MARINE = 0 bc they are designated terrestrial protected areas
mpa_poly<-mpa_data_poly[mpa_data_poly@data$MARINE!="0",]

#Remove MPAs where Status is proposed
mpa_poly<-mpa_poly[mpa_poly@data$STATUS!="Proposed",]

#Rasterize shapefile
#depth<-raster("Suitability/tmp/carib_depth.tif")
carib_poly_raster<-rasterize(mpa_poly,carib_depth,field="MARINE",progress='text',filename=paste(boxdir,"Suitability/raw/ICUN MPA/carib_poly_raster.tif",sep=""),overwrite=TRUE)

#Assign 0 to cells with MPA and 1 to cells with no MPAs
carib_poly_raster[carib_poly_raster>0]<-0
carib_poly_raster[is.na(carib_poly_raster)]<-1

#Add a land mask
carib_poly_raster<-mask(carib_poly_raster,carib_depth,maskvalues=NA,filename=paste(boxdir,"Suitability/raw/ICUN MPA/MPA poly raster final.tif",sep=""),overwrite=TRUE)
poly<-raster(paste(boxdir,"Suitability/raw/ICUN MPA/MPA poly raster final.tif",sep=""))
plot(carib_poly_raster,col=c("red","lightblue"))

#-----------------------------------------------------------------------------------------------------------------
#Next step requires arcmap (still looking for an R solution!)
#First crop file to Caribbean extent, 
#Next use toolbox to change file from multipart to single part and save asSave as "raw/ICUN MPA/caribmpa_points2"### 
##Next use the point to raster to tool to rasterize the shape file (error when tried to rasterize in R). Output is "raw/ICUN/mpapoints2.tif"
points<-raster(paste(boxdir,"Suitability/raw/ICUN MPA/mpapoints2.tif",sep=""))
#plot(points)
##Convert terrestrial protected areas to NA
points[points==1]<-NA


##resample
resample(points,carib_depth,file=paste(boxdir,"Suitability/raw/ICUN MPA/MPA point_1k.tif",sep=""),method="bilinear",overwrite=TRUE,progress='text')
points<-raster(paste(boxdir,"Suitability/raw/ICUN MPA/MPA point_1k.tif",sep=""))
#plot(points)

##change points that are 1 and 2 (MPAs) to 0 and NA to 1
points[points==2]<-0
points[points==3]<-0
points[is.na(points)]<-1

#add land mask
mask(points,carib_depth,maskvalue=NA,inverse=FALSE, file=paste(boxdir,"Suitability/raw/ICUN MPA/MPA point_final.tif",sep=""),overwrite=TRUE)
points<-raster(paste(boxdir,"Suitability/raw/ICUN MPA/MPA point_final.tif",sep=""))
plot(points,col=c("red","lightblue"))

##Now combine points and polygon files
## now try adding points to polygon layer

poly<-raster(paste(boxdir,"Suitability/raw/ICUN MPA/MPA poly raster final.tif",sep=""))
plot(points)
points[points==0]<-NA
UNEP_MPA<-mask(poly,points,maskvalue=NA,inverse=FALSE)
UNEP_MPA[is.na(UNEP_MPA)]<-0
UNEP_MPA<-mask(UNEP_MPA,carib_depth,inverse=FALSE,maskvalue=NA,filename=paste(boxdir,"Suitability/tmp/WDPA MPA",sep=""))
plot(UNEP_MPA,col=c("purple","lightblue"),add=TRUE)

##Combine WRI and WDPA MPA layers
WRI<-raster(paste(boxdir,"Suitability/tmp/WRI MPA_1km.tif",sep=""))
plot(WRI)
WRI[is.na(WRI)]<-0
WRI[WRI>0]<-3

###Create final w both files combined
final_mpa<-mask(UNEP_MPA,WRI,maskvalue=NA,inverse=FALSE)
plot(final_mpa)
final_mpa[is.na(final_mpa)]<-2
final_mpa<-mask(final_mpa,carib_depth,maskvalue=NA,inverse=FALSE)
plot(final_mpa,col=c("red","lightblue","purple"))
writeRaster(final_mpa,paste(boxdir,"Suitability/tmp/final_mpa_layer.tif",sep=""),overwrite=TRUE)

#------------------------------------------------------------------
#Thermocline layer data from: http://www.ifremer.fr/cerweb/deboyer/mld/Surface_Warm_Layer_Depth.php

thermo<-brick(paste(boxdir,"Suitability/raw/topofthermocline.nc",sep=""),varname="ttd",stopIfNotEqualSpaced=FALSE)
r_thermo<-rotate(thermo)
s_thermo<-crop(r_thermo,c(-125,0,0,50))
s_thermo[s_thermo>8e+08]<-NA
resample(s_thermo,carib_depth,method="ngb",filename=paste(boxdir,"Suitability/raw/carib_thermo_1k.nc",sep=""),varname="ttd",overwrite=TRUE)
carib_thermo<-brick(paste(boxdir,"Suitability/raw/carib_thermo_1k.nc",sep=""))
mask(carib_thermo,carib_depth,maskvalue=NA,filename=paste(boxdir,"Suitability/raw/carib_thermo_final.nc",sep=""),overwrite=TRUE)
carib_thermo<-brick(paste(boxdir,"Suitability/raw/carib_thermo_final.nc",sep=""))

calc(carib_thermo,fun=function(x){min(abs(x),na.rm=FALSE)},file=paste(boxdir,"Suitability/tmp/min_thermo.tif",sep=""),overwrite=TRUE)

final_min<-raster(paste(boxdir,"Suitability/tmp/min_thermo.tif",sep=""))

cols<-(terrain.colors(250))
png(file="min thermocline depth.png")
plottitle<-("Minimum thermocline depth in 2015 (m)")
plot(final_min,col=cols,main=plottitle,legend.args=list(text="meters",side=3))
dev.off()

#-----------------------------------------------------------------------
#Current velocity data from: https://podaac.jpl.nasa.gov/dataset/OSCAR_L4_OC_third-deg
currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/data/raw data',sep=""),full.names=T, pattern="nc")
varname="v"


currentlist<-lapply(currentfiles,brick,varname=varname)
year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
output1<-paste(boxdir,"Suitability/raw/currents/data/tmp/",year,"_1c.nc",sep="")
for(i in seq_along(currentlist)){crop(currentlist[[i]],c(19.83333, 380.16667, -80.16667, 80.16667),file=output1[i],overwrite=TRUE)}



currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/',sep=""),full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output2<-paste(boxdir,"Suitability/raw/currents/currents/data/tmp/2c/",year,"_2c.nc",sep="")
i=12
for (i in 1:length(currentlist)){
  plot1<-crop(currentlist[[i]],c(19.33333,199.5000,-80.16767,80.16667))
  plot2<-crop(currentlist[[i]],c(199.5001,380.1667,-80.16667,80.16667))
  plot2<-shift(plot2,x=-360)
  whole<-merge(plot1,plot2)
  crop_whole<-crop(whole,c(-90,-55,0,40),filename=output2[i],overwrite=TRUE)
  print(i)
}



year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/2c/',sep=""),full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output3<-paste(boxdir,"Suitability/raw/currents/currents/data/tmp/1k/",year,"_1km.nc",sep="")


for(i in seq_along(currentlist)){resample(currentlist[[i]],carib_depth,method="ngb",file=output3[i],
                                          overwrite=TRUE)}

currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/1k',sep=""),full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)
output5<-paste(boxdir,"Suitability/raw/currents/currents/data/final_v/",year,"_max.tif",sep="")
war
for(i in seq_along(currentlist)){calc(currentlist[[i]],fun=function(x){max(abs(x),na.rm=TRUE)},file=output5[i],overwrite=TRUE)}

currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/final_v/',sep=""),full.names=T,pattern='tif')
currentlist<-lapply(currentfiles,raster)
all<-brick(currentlist)
max_v<-calc(all,fun=function(x){max(abs(x),na.rm=TRUE)})

writeRaster(max_v,paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/allyears_v.tif",sep=""),overwrite=TRUE)

###run interpolation here using OA_interpolation.py file.

###Read in interpolated file and add land mask
max_v<-raster(paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/allyears_v_int.tif",sep=""))


final_max_v<-mask(max_v,carib_depth,maskvalue=NA,inverse=FALSE)
plot(final_max_v,col=cols)


##Write final raster and plot
writeRaster(final_max_v,paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/final_max_int_v.tif",sep=""),overwrite=TRUE)

cols<-(terrain.colors(250))
png(file=paste(boxdir,"Suitability/raw/currents/currents/data/final_v/final/max_v_final_plot.png",sep=""))
plottitle<-("Absolute Maximum Meridional Velocity\n 2006-2015")
plot(final_max_v,col=cols,main=plottitle,legend.args=list(text="m/s",side=3))
dev.off()

currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/raw data/',sep=""),full.names=T, pattern="nc")
varname="u"
#varname="v"

currentlist<-lapply(currentfiles,brick,varname=varname)
year<-as.vector(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
output1<-paste(boxdir,"Suitability/raw/currents/currents/data/tmp/",year,"_1c.nc",sep="")
for(i in seq_along(currentlist)){crop(currentlist[[i]],c(19.83333, 380.16667, -80.16667, 80.16667),file=output1[i],overwrite=TRUE)}



currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/',sep=""),full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output2<-paste(boxdir,"Suitability/raw/currents/currents/data/tmp/2c/",year,"_2c.nc",sep="")

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
currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/2c/',sep=""),full.names=T, pattern="nc")
currentlist<-lapply(currentfiles,brick)

output3<-paste(boxdir,"Suitability/raw/currents/currents/data/tmp/1k/",year,"_1km.nc",sep="")


for(i in seq_along(currentlist)){resample(currentlist[[i]],carib_depth,method="ngb",file=output3[i],
                                          overwrite=TRUE)}

currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/tmp/1k',sep=""),full.names=T, pattern="nc")
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

output5<-paste(boxdir,"Suitability/raw/currents/currents/data/final_u/",year,"_max.tif",sep="")
#output5<-paste("data2/final_v",year,"_max.tif")

for(i in seq_along(currentlist)){calc(currentlist[[i]],fun=function(x){max(abs(x),na.rm=TRUE)},file=output5[i],overwrite=TRUE)}



currentfiles<-list.files(paste(boxdir,'Suitability/raw/currents/currents/data/final_u/',sep=""),full.names=T,pattern='tif')
currentlist<-lapply(currentfiles,raster)
all<-brick(currentlist)
max_u<-calc(all,fun=function(x){max(abs(x),na.rm=TRUE)})


writeRaster(max_u,paste(boxdir,"Suitability/raw/currents/currents/data/final_u/final/2005_u.tif",sep=""),overwrite=TRUE)

###run interpolation here using OA_interpolation.py file.

###Read in interpolated file and add land mask
max_u<-raster(paste(boxdir,"Suitability/raw/currents/currents/data/final_u/final/2005_u_int.tif",sep=""))
plot(max_u)
depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif"))
final_max_u<-mask(max_u,carib_depth,maskvalue=NA,inverse=FALSE)

plot(final_max_u)
writeRaster(final_max_u,paste(boxdir,"Suitability/raw/currents/currents/data/final_u/final/final_max_int_u.tif",sep=""),overwrite=TRUE)
cols<-(terrain.colors(250))
png(file=paste(boxdir,"Suitability/raw/currents/currents/data/final_u/final/max_u_final_plot.png",sep=""))
plottitle<-("Absolute Maximum Zonal Velocity\n 2006-2015")
plot(final_max_u,col=cols,main=plottitle,legend.args=list(text="m/s",side=3))
dev.off()

#----------------------------------------------------------------------------
##Shipping data from: https://www.nceas.ucsb.edu/globalmarine2008/impacts
world<-raster(paste(boxdir,"Suitability/raw/world_depth.tif",sep=""))

ships<-raster(paste(boxdir,"Suitability/raw/shipping_lzw.tif",sep=""))
crs(ships)<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

projectRaster(ships,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",filename=paste(boxdir,"Suitability/tmp/ships_proj.tif",sep=""),overwrite=TRUE)
ships2<-raster(paste(boxdir,"Suitability/tmp/ships_proj.tif",sep=""))



crop(ships2,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""))
caribship<-raster(paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""))
resample(caribship,carib_depth,method="ngb",filename=paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""),overwrite=TRUE)

crop(caribship,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""),overwrite=TRUE)
mask(caribship,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_ship.tif",sep=""),overwrite=TRUE)
plot(caribship)

#-----------------------------------------------------------------------------
## Oil rig data from:https://www.nceas.ucsb.edu/globalmarine2008/impacts
carib_depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep=""))
oil2<-raster(paste(boxdir,"Suitability/raw/2013stablelights.tif",sep=""))
oil2<-crop(oil2,carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_oil2.tif",sep=""),overwrite=TRUE)
oil2<-mask(oil2,carib_depth,maskvalue=NA,inverse=FALSE)
plot(oil2)

oil<-raster(paste(boxdir,"Suitability/raw/oil_lzw.tif",sep=""))
crs(oil)<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +no_defs"
projectRaster(oil,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",filename=paste(boxdir,"Suitability/raw/oil_proj.tif",sep=""))
oil2<-raster(paste(boxdir,"Suitability/raw/oil_proj.tif",sep=""))
plot(oil,col="black")
crop(oil2, carib_depth,filename=paste(boxdir,"Suitability/tmp/carib_oil.tif",sep=""),overwrite=TRUE)
oil<-raster(paste(boxdir,"Suitability/tmp/carib_oil.tif",sep=""))


resample(oil,carib_depth,method="ngb",filename=paste(boxdir,"Suitability/tmp/carib_oil_1k.tif",sep=""),overwrite=TRUE)
oil<-raster(paste(boxdir,"Suitability/tmp/carib_oil_1k.tif",sep=""))
oil[is.na(oil)]<-0
mask(oil,carib_depth,maskvalue=NA,filename=paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""),overwrite=TRUE)
oil<-raster(paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""))
plot(oil)


## Coral reefs data from: http://data.unep-wcmc.org/datasets/1
coral<-readOGR(dsn=paste(boxdir,"Suitability/raw/WCMC coral 2010",sep=""), layer="clippedcorals")
carib_depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep=""))
carib_coral<-crop(coral,carib_depth,progress='text',filename=paste(boxdir,'Suitability/tmp/carib_coral.tif',sep=""))
carib_coral@data$COV_TYPE<-as.character(carib_coral@data$COV_TYPE)
coral_raster<-rasterize(carib_coral,carib_depth,field=2,progress='text')
coral_raster[is.na(coral_raster)]<-0
coral_raster<-mask(coral_raster,carib_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"Suitability/tmp/carib_coral_raster.tif",sep=""),overwrite=TRUE)
plot(coral_raster)
