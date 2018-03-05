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
library(tmaptools)
library(tidyverse)

## Set User (lennon/tyler) for rendering
user <- 'lennon'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <- '../../Box Sync/Carib_aqua_16/'}

# Chose whether raster layers should be saved and plotted

write_rasters<- FALSE

plot_rasters<-FALSE

# Set results directory

results_dir<-paste0(boxdir,"results/suitability/")

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assign a zero to international waters and 1 to waters inside EEZ

raster_eez<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))

raster_eez[raster_eez > 0]<- 1

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns depth values to either a 1 (between 25-100 m depth) or 0

carib_depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep=""))

carib_depth<-mask(carib_depth,raster_eez)

carib_depth[carib_depth<25]<-0

carib_depth[carib_depth>100]<-0

#carib_depth[carib_depth!=0]<-1


#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns cells with MPAs present a zero

MPA<-raster(paste(boxdir,'Suitability/tmp/final_mpa_layer.tif',sep=""))
MPA<-mask(MPA, raster_eez)

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

#current[current!=0]<-1

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

#ships[ships>0]<-1

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns a 0 to cells with the benthic oil structures

oil<-raster(paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""))

oil[oil>0]<-0

oil[is.na(oil)]<-1

oil<-mask(oil,raster_eez)

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Assigns a 0 to cells with coral reefs

coral<-raster(paste(boxdir,"Suitability/tmp/carib_coral_raster.tif",sep=""))

coral[coral==0]<-1

coral[coral==2]<-0

coral<-mask(coral,raster_eez)

# Plot rasters ------------------------------------------------------------

if (plot_rasters == TRUE) {

# eez
png(file=paste(boxdir,"Suitability/plots/suitable_eez.png",sep=""))

plottitle<-("suitable_eez")

plot(raster_eez,col="lightblue")

dev.off()

# depth

png(file=paste(boxdir,"Suitability/plots/suitable_depth.png",sep=""))

plottitle<-("Depth from 25-100 m")

plot(carib_depth,main=plottitle,legend.args=list(text="Depth (m)",side=3))

dev.off()

# MPA
png(file=paste(boxdir,"Suitability/plots/suitable_mpa.png",sep=""))

plottitle<-("MPA areas")

plot(MPA,main=plottitle)

dev.off()

# Currents
png(file=paste(boxdir,"Suitability/plots/suitable_currents.png",sep=""))

plottitle<-("Current Velocities")

plot(current,main=plottitle,legend.args=list(text="Current Velocity (m/s)",side=3))

dev.off()

png(file=paste(boxdir,"Suitability/plots/suitable_shipping.png",sep=""))

plottitle<-("Shipping")

plot(ships,main=plottitle,legend.args=list(text="Shipping Activity",side=3))

dev.off()

# Ships
png(file=paste(boxdir,"Suitability/plots/suitable_ships.png",sep=""))

plottitle<-("Shipping Activity")

plot(ships,main=plottitle,legend.args=list(text="Shipping Activity",side=3))

dev.off()


# Oil
png(file=paste(boxdir,"Suitability/plots/suitable_oil.png",sep=""))

plottitle<-("Oil Structure")

plot(oil,main=plottitle,legend.args=list(text="Oil Structure",side=3))

dev.off()

# Coral 

png(file=paste(boxdir,"Suitability/plots/suitable_coral.png",sep=""))

plottitle<-("Coral Habitat")

plot(coral,main=plottitle,legend.args=list(text="Oil Structure",side=3))

dev.off()

}



# Write Raster ------------------------------------------------------------

if (write_rasters == TRUE){
  
  writeRaster(raster_eez,paste(boxdir,"Suitability/final/suitable_eez.tif",sep=""),overwrite = TRUE) 
  writeRaster(carib_depth,filename=paste(boxdir,"Suitability/final/suitable_depth.tif",sep=""),overwrite=TRUE)
  writeRaster(carib_depth,filename=paste(boxdir,"Suitability/final/MPA.tif",sep=""),overwrite=TRUE)
  writeRaster(current,paste(boxdir,"Suitability/final/final_carib_current.tif",sep=""),overwrite=TRUE)
  writeRaster(ships,paste(boxdir,"Suitability/final/suitable_shipping.tif",sep=""),overwrite=TRUE)
  writeRaster(oil,paste(boxdir,"Suitability/final/suitable_oil.tif",sep=""),overwrite=TRUE)
  writeRaster(oil,paste(boxdir,"Suitability/final/coral.tif",sep=""),overwrite=TRUE)
}

#-----------------------------------------------------------------
#----------------------------------------------------------------
# Identify suitable cells based on all layers

s = stack(carib_depth,MPA,current,ships,raster_eez,oil,coral)

s_final<-stackApply(s,indices=c(1,1,1,1,1,1,1),fun=prod,na.rm=TRUE)

all_suitable<-mask(s_final,raster_eez,maskvalues=NA,inverse=FALSE,filename=paste0(boxdir,"Suitability/results/suitable_areas.tif"),overwrite=TRUE)

all_suitable[all_suitable == 0]<-NA

all_suitable[all_suitable > 1]<-1

saveRDS(all_suitable,paste0(results_dir,"suitable_areas.rds"))


# create raster with suitable cells as cell numbers

EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

EEZ$MRGID<-{as.numeric(levels(EEZ$MRGID))[EEZ$MRGID]}

eez_raster<-rasterize(EEZ,all_suitable,field="MRGID")

suit_no<-rasterFromCells(all_suitable, 1:length(all_suitable), values=TRUE) 
 
cell_no<-as.data.frame(suit_no)

suit_index<-as.data.frame(all_suitable)
suit_index$suitable_areas<-is.na(suit_index$suitable_areas)<-0

eez<-as.data.frame(eez_raster)

study_area<-as.data.frame(area(raster_eez,na.rm = TRUE))

suit_df<-cbind(cell_no,study_area,suit_index,eez)

names(suit_df)<-c("cell_no","study_area_km","suit_index","eez")

country_id<-as.data.frame(EEZ) %>%
  select(MRGID,Territory1) %>%
  setNames(c("eez","country"))


suit_df<-left_join(suit_df,country_id,by="eez") 

suit_df<-suit_df[!is.na(suit_df$country),]

write.csv(suit_df,paste0(boxdir,"data/cell_area_df.csv"))

## Calculate suitable area by country

country<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep = ""))

EEZ.df<-as.data.frame(EEZ)

names<-as.character(EEZ$GeoName)

c_area<-EEZ.df$Area_km2

country_area<-area(all_suitable,na.rm=TRUE)

country_suitable<-zonal(country_area,country,fun='sum')

country_suitable.df<-as_data_frame(country_suitable) %>%
  mutate( names =names,
          c_area = c_area) %>%
  mutate(percent_eez_suitable = sum/c_area*100) %>%
  arrange(desc(sum)) %>%
  select(names,c_area,sum,percent_eez_suitable)


write.csv(country_suitable.df, paste0( boxdir,"results/suitability/suit_df_summary.csv"))


#-----------------------------------------------------------------
#----------------------------------------------------------------
# Determine total area (km2) of area that was identified as suitable

# Create data frame of total suitable areasby cell


layer<-c("Study area","Depth","MPA presence","Current speed","Coral presence","High shipping activity","Oil platform presence","Final suitable area")

suit_layer_df <- tibble(Layers = layer, 
  suit_area = as.numeric(NA))



suit_layer_df$suit_area[suit_layer_df$Layers== "Study area"]<-sum(study_area, na.rm = TRUE)


all_suitable[all_suitable==0]<-NA
suitable_areas<-area(all_suitable,na.rm = TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "Final suitable area"]<-cellStats(suitable_areas, stat = sum, na.rm = TRUE)

carib_depth[carib_depth==0]<-NA
depth<-area(carib_depth,na.rm=TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "Depth"]<-cellStats(depth, stat = sum, na.rm = TRUE)

MPA[MPA==0]<-NA
MPA<-area(MPA,na.rm =TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "MPA presence"]<-cellStats(MPA, stat = sum, na.rm = TRUE)


current[current==0]<-NA)

current<-area(current,na.rm = TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "Current speed"]<-cellStats(current, stat = sum, na.rm = TRUE)

coral[coral==0]<-NA
coral<-area(coral,na.rm = TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "Coral presence"]<-cellStats(coral, stat = sum, na.rm = TRUE)

ships[ships==0]<-NA
ship<-area(ships,na.rm = TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "High shipping activity"]<-cellStats(ship, stat = sum, na.rm = TRUE)

oil[oil==0]<-NA
oil<-area(oil,na.rm = TRUE)
suit_layer_df$suit_area[suit_layer_df$Layers== "Oil platform presence"]<-cellStats(oil, stat = sum, na.rm = TRUE)

final_study_area<-suit_layer_df$suit_area[1]

suit_layer_df<-suit_layer_df %>%
  mutate(perc_area = suit_area[]/final_study_area*100) %>%
  arrange(desc(perc_area)) 
 
   colnames(suit_layer_df)<-c("Layer","Suitable Area (km^2)","% of Study Area")

write.csv(suit_layer_df,paste0(boxdir, "results/suitability/suit_by_variable.csv")

##Script should end here and the rest will be proceesed in results
#-----------------------------------------------------------------
#----------------------------------------------------------------
carib_depth[carib_depth == 0]<-NA

pal1<- get_brewer_pal("YlOrRd", n = 8, contrast = c(0.375,1))


depth_plot<-
  tm_shape(carib_depth) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="Depth (m)", style ="cont", auto.palette.mapping = FALSE, palette = pal1, n=6) +
  tm_shape(EEZ, is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.25) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 0.8,title.size = 1.5,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)


MPA_plot<-
  tm_shape(MPA) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="", style ="cat", auto.palette.mapping = FALSE, palette = c("red","white"), labels = c("MPAs","")) +
  tm_shape(EEZ, is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.3) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 1.5,title.size = 1.5,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)

current_plot<-
  tm_shape(current) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="Max current \n velocity (m/s)", style ="cont", auto.palette.mapping = FALSE)+
  tm_shape(EEZ, is.master = TRUE) +
  tm_fill(col="navy",alpha = 0.3) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 0.8,title.size = 1.1,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)


coral_plot<-
  tm_shape(coral) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="", style ="cat", auto.palette.mapping = FALSE, palette = c("green","white"), labels = c("Coral reefs","")) +
  tm_shape(EEZ, is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.3) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 1.5,title.size = 1.5,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)

ships[ships==0]<-10

shipping_plot<-
  tm_shape(ships) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="Relative shipping activity", style ="cont", auto.palette.mapping = FALSE, palette = "Reds", n = 6,
            labels = c("","low activity","","","high activity","")) +
  tm_shape(EEZ) +
  tm_fill(col="lightblue",alpha = 0.2) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 0.8,title.size = 1.5,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)

oil_plot<-
  tm_shape(oil) +
  tm_raster(showNA = FALSE, legend.show = TRUE,title="", style ="cat", auto.palette.mapping = FALSE, palette = c("forestgreen","white"), labels = c("Oil platforms","")) +
  tm_shape(EEZ, is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.25) +
  tm_borders(lwd = 0.8) +
  tm_legend(position = c(0.7,0.7),text.size = 1.5,title.size = 1.2,scale=1.1,legend.width = 0.9)  +
  tm_scale_bar(position=c("LEFT","bottom"),width = 0.25)

tmap_arrange(depth_plot, MPA_plot, current_plot, coral_plot, shipping_plot, oil_plot)
   
# Plot suitable raster layer


EEZ$MRGID<-{as.numeric(levels(EEZ$MRGID))[EEZ$MRGID]}

eez_raster<-rasterize(EEZ,all_suitable,field="MRGID")

all<-tm_shape(all_suitable) +
  tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas"), title = "all_suitable") +
  tm_shape(EEZ,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.3,title = "Greater Antilles") +
  tm_borders(lwd = 1.2) +
  tm_legend(main.title.size = 2, main.title="all_suitable", position = c("right","top"))

save_tmap(all,filename = paste0(results_dir,"all_suitable.png"))

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


