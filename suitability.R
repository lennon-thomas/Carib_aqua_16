rm(list = ls())

library(raster)
library(rgdal)
# create spatial layer of 1km2 cells that may be suitable for cobia aquaculture


setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture/Suitability")

#---------------------------------------------------------------------------------------------
#reassigns cell values to 1 that are between 25-100 m depth. All other cells are assigned a zero

carib_depth<-raster("tmp/carib_depth.tif")
carib_depth[is.na(carib_depth)]<-0
carib_depth[carib_depth<25]<-0
carib_depth[carib_depth>90]<-0

land<-raster("tmp/carib_depth.tif")
carib_depth<-mask(carib_depth,land,inverse=FALSE,maskvalues=NA,filename="final/suitable_depth.tif",overwrite=TRUE)

png(file="datalayer plots/suitable_depth.png")
plottitle<-("Depth from 25-100 m")
plot(carib_depth,main=plottitle,legend.args=list(text="Depth (m)",side=3))
dev.off()

carib_depth[carib_depth!=0]<-1

#----------------------------------------
##MPAs####
#Assigns cells with MPAs present a zero#
#-------------------------------------------
MPA<-raster('tmp/final_mpa_layer.tif')
land<-raster("tmp/carib_depth.tif")
suitableMPA<-mask(MPA,land,maskvalues=NA,inverse=FALSE,filename="final/suitable_MPA.tif",overwrite=TRUE)

 png(file="datalayer plots/suitable_MPA.png")
 plottitle<-("WRI and WDPA MPAs")
 plot(suitableMPA,col=c("purple","lightblue"),legend=FALSE,main=plottitle)
 dev.off()

####Currents#########
#Assigns cells with a current velocity over 1 m/s a 0
#----------------------------------------------------
u<-raster("tmp/final_max_int_u.tif")
v<-raster("tmp/final_max_int_v.tif")
s<-stack(u,v)

current<-calc(s,fun=function(x){max(abs(x),na.rm=FALSE)})
current[current>1]<-0
current[current==1]<-0



png(file="datalayer plots/suitable currents.png")
cols<-terrain.colors(250)
plottitle<-("Suitable max v and u current speed from 2006-2015")
plot(current,main=plottitle,legend.args=list(text="m/s",side=3))
dev.off()

writeRaster(current,filename="final/suitable_currents.tif",overwrite=TRUE)
current[current!=0]<-1


####Shipping######
####Assigns a 0 to cells where shipping traffic is high###
#----------------------------------------------------
ships<-raster("tmp/carib_ship.tif")

ships[ships>0.35]<-0 ## 0.4 was chosen as an arbitary threshold for now
ships[ships==0.35]<-0
ships[is.na(ships)]<-0.1
ships<-mask(ships,land,maskvalues=NA,inverse=FALSE,filename="final/suitable_shipping.tif",overwrite=TRUE)

 png("datalayer plots/Suitable shipping lanes.png")
 plottitle="Major shipping areas (>0.35)"
 plot(ships,main=plottitle)## still need to figure out what units/scale on this dataset represnet and add legend title and units
 dev.off()

ships[ships>0]<-1

##EEZ####
##Assigns a zero to international waters (not inside an EEZ)
#--------------------------------------------------------
eez<-raster("tmp/carib_eez_ocean.tif")
plot(eez)
codes<-read.csv("tmp/country_codes.csv")

##Remove countries (Latin America) that aren't included in the analysis
noncountries<-c(108,164,129,130,131,132,133,135,139,163,167,255)
for (i in 1:length(noncountries)){
eez[eez==noncountries[i]]<-0
}

writeRaster(eez,filename="final/suitable_eez.tif",overwrite=TRUE)

png("datalayer plots/EEZ Suitability.png")
plottitle="Caribbean EEZs"
plot(eez,main=plottitle,legend=FALSE)
dev.off()

eez[eez>0]<-1
###Thermocline
## Assings a 0 to cells with thermocline <20 m depth
#--------------------------------------------

thermo<-raster("tmp/min_thermo.tif")
thermo[thermo<20]<-0
writeRaster(thermo,filename="final/suitable_min thermocline.tif",overwrite=TRUE)

png("datalayer plots/Min thermocline Suitability.png")
plottitle="Minimum Thermocline Depth"
plot(thermo,main=plottitle,legend.args=list(text="depth (m)",side=3))
dev.off()

thermo[thermo>0]<-1

##Oil
##  0 are cells with no oil platforms, 1 means they are present- we need to reverse this
#--------------------------------------------
oil<-raster("final/suitable_oil.tif")
oil[oil==1]<-100
oil[oil==0]<-1
oil[oil==100]<-0
png("datalayer plots/Oil Structure Suitability.png")
plottitle="Benthic Oil Structure presence"
plot(oil,main=plottitle,legend=FALSE,col=c("red","lightblue"))
dev.off()

#Quick visual check to make sure values in each layer are 0 (not suitable) or 1 (suitable)
plot(carib_depth)
plot(MPA)
plot(current)
plot(ships)
plot(eez)
plot(thermo)
plot(oil)

#s = stack(carib_depth,MPA,current,ships,eez,thermo,oil)
s = stack(carib_depth,MPA,current,ships,eez,oil)

# multiplies layers so that any cells with a zero will be given a zero value
all<-stackApply(s,indices=c(1,1,1,1,1,1),fun=prod,na.rm=FALSE)
all<-mask(all,land,maskvalues=NA,inverse=FALSE,filename="final/Suitable areas.tif",overwrite=TRUE)

png("datalayer plots/Suitable areas-no thermo.png")
plottitle="Suitable areas for offshore cobia aquaculture (without thermocline)"
plot(all,main=plottitle,legend=FALSE,col=c("lightblue","red"))
dev.off()

#Determine total area (km2) of area that was identified as suitable
suitable_cells<-all[all==1]
suitable_area<-length(suitable_cells)


## Calculate suitable area by country
EEZ = readOGR(dsn="tmp",layer="carib_eez_shape")
#countries<-rasterize(EEZ,carib_depth,field="rgn_name")
country<-raster("final/suitable_eez.tif")
all[all==0]<-NA
plot(country)
names<-data.frame(matrix(NA,nrow=27,ncol=2))
EEZ<-as.data.frame(EEZ)
c<-EEZ[,2:3]
cells<-zonal(all,country,fun='sum')
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

####tmaps######
test<-select(country)
library(tmap)
data(World)

land = readOGR(dsn="C:/Users/Lennon Thomas/Desktop/Carib_aqua_16/Suitability/tmp/EEZ",layer="carib_eez_shape")
land$la<-ifelse(land@data$Pol_type=="200NM","blue","orange")

suitable<-tm_shape(land)+
  tm_fill("la")+
tm_shape(all)+
  tm_raster(showNA=FALSE,legend.show=FALSE)+
tm_shape(land)+
  tm_borders()

save_tmap(suitable,filename="datalayer plots/suitability.png")
tm.layout(legend.position=c("right","top"))


