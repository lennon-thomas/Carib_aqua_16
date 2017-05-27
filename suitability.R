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
library(tmap)
library(dplyr)

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

png(file=paste(boxdir,"Suitability/plots/suitable_depth.png",sep=""))

plottitle<-("Depth from 25-100 m")

plot(carib_depth,main=plottitle,legend.args=list(text="Depth (m)",side=3))

dev.off()

carib_depth[carib_depth!=0]<-1

writeRaster(carib_depth,filename=paste(boxdir,"Suitability/final/suitable_depth.tif",sep=""),overwrite=TRUE)
#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns cells with MPAs present a zero

MPA<-raster(paste(boxdir,'Suitability/tmp/final_mpa_layer.tif',sep=""))


png(file=paste(boxdir,"Suitability/plots/suitable_mpa.png",sep=""))

plottitle<-("MPA areas")

plot(MPA,main=plottitle)

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

all_suitable<-mask(s,raster_eez,maskvalues=NA,inverse=FALSE,filename=paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep=""),overwrite=TRUE)

saveRDS(all,paste(boxdir,"Suitability/final/suitable_areas.rds",sep=""))

all_suitable[all_suitable == 0]<-NA

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Determine total area (km2) of area that was identified as suitable

study_area<-as.data.frame(area(raster_eez,na.rm = TRUE))

study_area<-sum(study_area,na.rm = TRUE)

suitable_areas<-as.data.frame(area(all_suitable,na.rm = TRUE))

total_suitable_area<-sum(suitable_areas,na.rm = TRUE)

percent_suitable<-total_suitable_area/study_area*100

#-----------------------------------------------------------------
#----------------------------------------------------------------
## Calculate suitable area by country

EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

country<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep = ""))

EEZ<-as.data.frame(EEZ)

names<-as.character(EEZ$GeoName)

c_area<-EEZ$Area_km2

country_area<-area(all_suitable,na.rm=TRUE)

country_suitable<-zonal(country_area,country,fun='sum')

country_suitable<-as.data.frame(country_suitable)

percent_eez_suitable = country_suitable$sum/c_area*100

country_suitable<-cbind(names,c_area,country_suitable,percent_eez_suitable) 



country_suitable<-country_suitable[,-3]

country_suitable<- country_suitable[order(-percent_eez_suitable),]

colnames(country_suitable)<-c("EEZ","Total Area","Suitable Area","Percent Suitable")

country_suitable$EEZ<-as.character(country_suitable$EEZ)

write.csv(country_suitable,paste(boxdir,filename = "Suitability/results/suitable area by eez.csv",sep = ""))


#-----------------------------------------------------------------
#----------------------------------------------------------------
# Determine total area (km2) of area that was identified as suitable

data(World)

EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

suitable<-raster(paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep = ""))

great_ant = c("Joint regime area Honduras / Cayman Islands",
                    "Cuban Exclusive Economic Zone","Cayman Islands Exclusive Economic Zone",
                    "Jamaican Exclusive Economic Zone",
                    "Haitian Exclusive Economic Zone",
                    "Joint regime area Colombia / Dominican Republic",
                    "Joint regime area Colombia / Jamaica",
                     "Dominican Republic Exclusive Economic Zone")

luc_arc<-c("Turks and Caicos Exclusive Economic Zone", "Bahamas Exclusive Economic Zone")

lee_ant <-c("Aruban Exclusive Economic Zone","Bonaire Exclusive Economic Zone","Curaçaoan Exclusive Economic Zone")

EEZ@data$region<-as.factor(ifelse(EEZ@data$GeoName %in% great_ant, "Greater Antilles",
                                  ifelse(EEZ@data$GeoName %in% luc_arc, "Lucayan Archipelago", 
                                         ifelse(EEZ@data$GeoName %in% lee_ant, "Leeward Antilles","Lesser Antilles"))))

GA<- EEZ[EEZ$GeoName %in% great_ant,]

GA_suit<-mask(suitable,GA)

GA_suit<-crop(GA_suit,GA)



suitable<-raster(paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep = ""))

LUC<-EEZ[EEZ@data$region == "Lucayan Archipelago",]

LUC_suit<-mask(suitable,LUC)

LUC_suit<-crop(LUC_suit,LUC)



suitable<-raster(paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep = ""))

LEE<- EEZ[EEZ$GeoName %in% lee_ant,]

LEE_suit<-mask(suitable,LEE)

LEE_suit<-crop(LEE_suit,LEE)


suitable<-raster(paste(boxdir,"Suitability/results/final_suitable_areas.tif",sep = ""))

LS<- EEZ[EEZ@data$region == "Lesser Antilles",]

LS_suit<-mask(suitable,LS)

LS_suit<-crop(LS_suit,LS)





g_antilles_fig<-  tm_shape(GA_suit) +
                    tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas"), title = "Greater Antilles") +
                  tm_shape(GA,is.master = TRUE) +
                    tm_fill(col="lightblue",alpha = 0.4,title = "Greater Antilles") +
                    tm_borders(lwd = 1.2) +
                    tm_legend(main.title.size = 2, main.title="Greater Antilles", position = c("right","top"))
                   
                 
                  

luc_arc_suitable.fig<- tm_shape(LUC_suit) +
                          tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
                       tm_shape(LUC,is.master = TRUE) +
                          tm_fill(col="lightblue",alpha = 0.4,title ="TESt") +
                          tm_borders(lwd = 1.2) +
                          tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"),main.title = "Lucayan Archipelago")



lee_ant_suitable.fig<- tm_shape(LEE_suit) +
                          tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
                       tm_shape(LEE,is.master = TRUE) +
                          tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
                          tm_borders(lwd = 1.2) +
                          tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"), main.title = "Leeward Antilles")


less_ant_suitable.fig<- tm_shape(LS_suit) +
                          tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
                       tm_shape(LS,is.master = TRUE) +
                          tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
                          tm_borders(lwd = 1.2) +
                          tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"), main.title = "Lesser Antilles")


all_plots<- tmap_arrange(g_antilles_fig, luc_arc_suitable.fig,lee_ant_suitable.fig,less_ant_suitable.fig)

save_tmap(all_plots,filename = paste(boxdir,"Suitability/results/suitability_figure_zoom.png",sep = ""))


save.image(file = paste(boxdir,'Suitability/results/suitability_results.Rdata',sep=""))


