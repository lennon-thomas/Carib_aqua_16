# Determine total area (km2) of area that was identified as suitable

plot_map_zoom<-function(EEZ,data_raster,legend_label){

data(World)

#EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

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

GA_data<-mask(data_raster,GA)

GA_data<-crop(GA_data,GA)



LUC<-EEZ[EEZ@data$region == "Lucayan Archipelago",]

LUC_data<-mask(data_raster,LUC)

LUC_data<-crop(LUC_data,LUC)


LEE<- EEZ[EEZ$GeoName %in% lee_ant,]

LEE_data<-mask(data_raster,LEE)

LEE_data<-crop(LEE_data,LEE)


LS<- EEZ[EEZ@data$region == "Lesser Antilles",]

LS_data<-mask(data_raster,LS)

LS_data<-crop(LS_data,LS)



g_antilles_fig<-  tm_shape(GA_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"), title = "Greater Antilles") +
  tm_shape(GA,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title = "Greater Antilles") +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, main.title="Greater Antilles", position = c("right","top"))




luc_arc_suitable.fig<- tm_shape(LUC_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"),labels = c(legend_label)) +
  tm_shape(LUC,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title ="TESt") +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"),main.title = "Lucayan Archipelago")



lee_ant_suitable.fig<- tm_shape(LEE_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"),labels = legend_label) +
  tm_shape(LEE,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"), main.title = "Leeward Antilles")


less_ant_suitable.fig<- tm_shape(LS_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"),labels = legend_label) +
  tm_shape(LS,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"), main.title = "Lesser Antilles")


all_plots<- tmap_arrange(g_antilles_fig, luc_arc_suitable.fig,lee_ant_suitable.fig,less_ant_suitable.fig)

save_tmap(all_plots,filename = paste0(fig_folder,legend_label,"_map_zoom.png"))

return(all_plots)
}


reg_map_plot<-function(EEZ,map_layer,legend){
  tm_shape(EEZ)+
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(lwd = 1.2) +
  tm_shape(map_layer,is.master = TRUE)+
  tm_raster(showNA = FALSE, legend.show = TRUE,palette=c("red","blue")) +
  tm_legend(position = c("right","top"),scale=1) 

save_tmap(reg_map_plot,filename = paste0(fig_folder,map_layer,"_map_zoom.png,"))

return(reg_map_plot)
}




