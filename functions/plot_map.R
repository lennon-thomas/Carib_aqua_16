# Determine total area (km2) of area that was identified as suitable

plot_map_zoom<-function(EEZ,data_raster){

data(World)

# EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")

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

### Subset different main Caribbean regions for plotting

# Greater Antilles
GA<- EEZ[EEZ$GeoName %in% great_ant,]
GA_data<-mask(data_raster,GA)
GA_data<-crop(GA_data,GA)

# Lucayan Archipelago
LUC<-EEZ[EEZ@data$region == "Lucayan Archipelago",]
LUC_data<-mask(data_raster,LUC)
LUC_data<-crop(LUC_data,LUC)

# Leeward Antilles
LEE<- EEZ[EEZ$GeoName %in% lee_ant,]
LEE_data<-mask(data_raster,LEE)
LEE_data<-crop(LEE_data,LEE)

# Lesser Antilles
LS<- EEZ[EEZ@data$region == "Lesser Antilles",]
LS_data<-mask(data_raster,LS)
LS_data<-crop(LS_data,LS)

# Make individual plots by region 

g_antilles_fig <-  tm_shape(GA_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"), title = "10 yr NPV ($US)") +
  tm_shape(GA,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, main.title="Greater Antilles", position = c("right","top"))


luc_arc_suitable_fig <- tm_shape(LUC_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"), title = '10 yr NPV ($US)') +
  tm_shape(LUC,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"), main.title = "Lucayan Archipelago")



lee_ant_suitable_fig<- tm_shape(LEE_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"), title = '10 yr NPV ($US)') +
  tm_shape(LEE,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"), main.title = "Leeward Antilles")


less_ant_suitable_fig<- tm_shape(LS_data) +
  tm_raster(showNA = FALSE, legend.show = TRUE, palette = c("red","blue"), title = '10 yr NPV ($US)') +
  tm_shape(LS, is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(col="black") +
  tm_legend(main.title.size = 1.4, text.size = 1, position = c("right","top"), main.title = "Lesser Antilles")


png(filename = paste0(fig_folder,"10_yr_NPV_map_zoom.png"), width = 6, height = 6, units = 'in', res = 300)
tmap_arrange(g_antilles_fig, luc_arc_suitable_fig, lee_ant_suitable_fig, less_ant_suitable_fig)
dev.off()


# save_tmap(tmap_arrange(g_antilles_fig, luc_arc_suitable_fig, lee_ant_suitable_fig, less_ant_suitable_fig), 
#           filename = paste0(fig_folder,"10_yr_NPV_map_zoom.png"))

return(all_plots)
}

### Regional map plotting function
reg_map_plot <- function(EEZ,map_layer){
  
  # Create map
  region_map <- tm_shape(EEZ) +
    tm_fill(col="lightblue",alpha = 0.4) +
    tm_borders(lwd = 1.2) +
    tm_shape(map_layer, is.master = TRUE)+
    tm_raster(showNA = FALSE, legend.show = TRUE, palette=c("red","blue"), title = '10 yr NPV ($US)') +
    tm_legend(position = c("right","top"), scale=1, main.title = "The Caribbean") 
  
  # Save map
  save_tmap(region_map, filename = paste0(fig_folder,"carib_npv_map_zoom.png"), width = 5, asp = 0, units = 'in')
  # Return map object
  return(region_map)
}




