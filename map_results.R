  # Load packages and functions-----------------------------------------------------------
  rm(list = ls())
  
  library(raster)
  library(rgdal)
  library(tmap)
  library(ncdf4)
  library(stringr)
  library(broom)
  library(fasterize)
  library(sf)
  library(tidyverse)
  library(parallel)
  library(R.utils)
  library(readr)
  library(pander)
  library(scales)
  library(viridis)
  library(ggforce)
  library(broom)
  library(forcats)
  library(rasterVis)
  #library(plotly)
  # library(plyr)
  
  # Run settings -------------------------------------------------------------
  
  ## Set User (lennon/tyler)
  user <- 'lennon'
  
  if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
  if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}
  
  run_name = 'fixed_Feb_26'

# Create figure folder
  fig_folder <- paste0(boxdir,'results/',run_name, "/Figures/")

  if (dir.exists(fig_folder) == F) {
  dir.create(fig_folder, recursive = T)
} else {
  
  print('Folder already exists')
}
# Load EEZ shapefile for plotting

  EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")
  
  # Load suitablity results
  s_areas<-gzfile(paste0(boxdir,"results/Suitability/suitable_areas.rds"))
  
  suit_areas<-readRDS(s_areas)
  
  suit_df<-read_csv(paste0(boxdir,"/results/Suitability/suitable_area_df.csv"),
                    col_types = "iiddiic") %>%
    dplyr::select(c("cell_no","study_area_km","suit_area_km","suit_index","eez","country"))
  
  suit_df_summary<- suit_df %>%
    group_by(country) %>%
    summarise(suitable_area_km = round(sum(suit_area_km,na.rm = TRUE),2),
              total_area_km = round(sum(study_area_km,na.rm =TRUE),2)) %>%
    mutate(suitable_perc = round(suitable_area_km / total_area_km * 100,2)) %>%
    arrange(desc(suitable_perc)) %>%
    set_names(c("Country","Suitable area (km^2)","Total EEZ area (km^2)","Suitable area (% of EEZ)"))
    
  write.csv(suit_df_summary,paste0(boxdir,"results/Suitability/suit_df_summary.csv"))       
  
# Load sim data
  
  data_folder <- paste0(boxdir,'results/',run_name,"/Data/")
  
  data.names <- list.files(path = data_folder, pattern = ".nc")
  
  data_files <- lapply(paste0(data_folder,"/",data.names),brick)
  
  avg_growth<-data_files[[1]]
  
  harv_cycle_length<-data_files[[2]]
  
  no_cycles<-data_files[[3]]
  
  stocking_n<-data_files[[4]]
  
  rm(data_files)

# Load run results 


  result_folder <- paste0(boxdir,'results/',run_name,"/Results")
  
  file.names <- list.files(path = result_folder, pattern = ".csv")
  
  result_files <- lapply(paste0(result_folder,"/",file.names),read_csv)
  
  carib_supply<-result_files[[1]]
  
  eez_supply_df<-result_files[[2]]
  
  npv_df<-result_files[[4]]
  
  supply_summary<-result_files[[6]]
  
  rm(result_files)

# Prep data for plotting --------------------------------------------------

#This joins npv_df dataframe with spatial coordinates and EEZ of each cell number
  
  tidy_eez<-tidy(EEZ)
  
  temp_df <- data.frame(EEZ@data)
  
  temp_df$id <- seq(0,nrow(temp_df)-1)
  
  EEZ_df <- merge(tidy_eez, temp_df, by="id")
  
  cells<-as.vector(Which(suit_areas>0, cells =TRUE))
  
  raster_coords<-as_data_frame(rasterToPoints(suit_areas))
  
  suit_coords<-cbind(cells,raster_coords) %>%
    set_names(c("cell","long","lat","suitable"))
  
  all_df<- npv_df %>%
    filter(prices == 8.62) %>%
    left_join(suit_coords) 
  
  
  all_df<-dplyr:: rename(all_df,Territory1 = country) 

# Create base map of land and water in Carib ------------------------------

eez.land<-EEZ_df %>%
  filter(hole == TRUE)

eez.water<-EEZ_df %>%
  filter(hole == FALSE)

base<- ggplot() + 
        geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
        geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.07) +
        theme(legend.position="none") +
        theme_minimal() +
        xlab("Longitude") +
        ylab("Latitude") +
        coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))

  base_facet<-  ggplot() + 
                  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
                  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.07) +
                  theme(legend.position="none") +
                  theme_minimal() +
               #   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                  xlab("Longitude") +
                  ylab("Latitude") +
                  facet_wrap(~Territory1,scales = "free")
                  #facet_wrap_paginate(~Territory1,scales="free",ncol = 1, nrow = 2, page = i) 

# Suitable area maps -----------------------------------------------------

                  
  suitable_plot<-base +
                    geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="orange") +
                    theme(legend.position = "none") +
                    ggtitle("Suitable Areas for Offshore Mariculture")         
    
  ggsave(filename = paste0(fig_folder,'carib_suit_area.png'), width = 6, height = 5)
                                    
  suitable_plot_facet<-base_facet +
                          geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="orange") +
                          theme(legend.position = "none") +
                          ggtitle("Suitable Areas for Offshore Mariculture")

  ggsave(filename = paste0(fig_folder,'eez_suitable_area.png'), width = 12, height = 12)

# Production maps ---------------------------------------------------------

# Production if don't consider economics
  all_df$eez<-as.factor(all_df$eez)
  
  total_prod<- all_df %>%
    filter(feed_price_index == 1 & disc_scenario == 'cntry') %>%
    group_by(eez) %>%
    summarise(eez_harvest_mt = sum(total_harvest)* 0.001) %>%
    mutate(annual_eez_harvest = eez_harvest_mt/10) %>%
    ungroup() %>%
    left_join(eez.water, by = c('eez' = 'MRGID'))

  
  
   # with no economic consideration 
    total_prod_map<-  ggplot() + 
      geom_polygon(data = total_prod,aes(x = long,y = lat, group = group, fill= eez_harvest_mt), colour = "black", size = 0.1 , alpha = 0.8) +
      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
      scale_fill_viridis("Average Annual Production (mt)") +
      ggtitle("(all suitable cells)") +
      theme_minimal() + 
      xlab("Longitude") +
      ylab("Latitude") +
      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
    
    ggsave(paste0(fig_folder,'total_prod_map.png'), width = 6, height = 5)
    
    #Bar stacked bar plot of total production by EEZ for profitable cells
   
    all_df$disc_scenario<-as.factor(all_df$disc_scenario) 
    all_df$feed_price_index<-as.factor(all_df$feed_price_index)
    
    all_df<-all_df %>%
      mutate(scenario_names = ifelse(disc_scenario == "all_suitable" & is.na(feed_price_index),"all_suitable",
                                     ifelse(disc_scenario == "0" & feed_price_index == 1, "No discount, high feed",
                                            ifelse (disc_scenario =="0" & feed_price_index == 0.9, "No discount, low feed",
                                                    ifelse(disc_scenario =="cntry" & feed_price_index == 1, "Risk discount, high feed",
                                                           "Risk discount, low feed"))))) 
 
    # Can now change these to group by scenarios names instead 
    
   econ_prod<- all_df %>%
      filter(npv > 0) %>%
      dplyr::group_by(eez,disc_scenario,feed_price_index) %>%
      summarise(total_harvest_mt = sum(total_harvest * 0.001)) 
  
  
  t_prod<-all_df%>%
    filter(feed_price_index == 1 & disc_scenario ==0) %>%
    group_by(eez) %>%
    summarise(total_harvest_mt = sum(total_harvest * 0.001)) %>%
    mutate(disc_scenario = "all_suitable",
           feed_price_index = NA) %>%
    select(eez,disc_scenario,feed_price_index,total_harvest_mt,scenario_names)

  c_names<-as.data.frame(EEZ@data)%>%
    select(MRGID,Territory1)

  all_eez_prod<-bind_rows(econ_prod,t_prod) %>%
    mutate(scenario_names = ifelse(disc_scenario == "all_suitable" & is.na(feed_price_index),"all_suitable",
                                   ifelse(disc_scenario == "0" & feed_price_index == 1, "No discount, high feed",
                                          ifelse (disc_scenario =="0" & feed_price_index == 0.9, "No discount, low feed",
                                                  ifelse(disc_scenario =="cntry" & feed_price_index == 1, "Risk discount, high feed",
                                                         "Risk discount, low feed"))))) 
  
   
  final_prod<-left_join(all_eez_prod,c_names,by=c("eez"="MRGID")) %>%
    group_by(scenario_names) %>%
    summarize(total_production = sum(total_harvest_mt)/10)
  
  ggplot(final_prod,aes(x=fct_relevel(scenario_names,c("all_suitable","No discount, low feed","Risk discount, low feed","No discount, high feed","Risk discount, high feed")),y=total_production))+
    geom_bar(stat="identity") +
    theme_minimal() +
    ylab("Average Annual Harvest (mt)") +
    xlab("Economic Scenario") 
  
 
  ggsave(paste0(fig_folder,"production.png"),width=5, height=6)
  
   # scale_fill_brewer()
# NPV maps/histograms for current price -----------------------------------------------------

  # Current price and only profitable farms by EEZ
  npv_eez<-all_df %>%
    filter( npv>0) %>%
    group_by(Territory1,disc_scenario,feed_price_index) %>%
    summarise(total_npv = sum(npv),
              total_production = sum(total_harvest * 0.001)) %>%
    left_join(eez.water)  %>%
    rename(Discount_scenario = disc_scenario)

  npv_eez$feed_price_index<-ifelse(npv_eez$feed_price_index==0.9,"Low feed cost","High feed cost")
  npv_eez$Discount_scenario<-ifelse(npv_eez$Discount_scenario==0, "0 %", "Country risk score")
  
  # Map of NPV by discount rate (need to plot on separate pages)
  
  npv_discount_map<-  ggplot() + 
    geom_polygon(data = npv_eez,aes(x = long,y = lat,group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("10 year NPV ($)") +
    theme_minimal() + 
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
    facet_wrap(Discount_scenario~feed_price_index,labeller = "label_both") +
    ggtitle("EEZ level 10 yr NPV")
  #  facet_grid_paginate(~Discount_rate,labeller = "label_both", page=3, ncol = 3)
  ggsave(paste0(fig_folder,'npv_scenarios_map.png'), width = 6, height = 5)
  
 #Zoom in on just one of the above plots
  
  risk_high_feed<-npv_eez %>%
    filter(Discount_scenario =="Country risk score" & feed_price_index =="High feed cost")
  
 risk_high_feed_map<-  ggplot() + 
    geom_polygon(data = risk_high_feed,aes(x = long,y = lat,group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("10 year NPV ($)") +
    theme_minimal() + 
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
   ggtitle("EEZ level 10 yr NPV \n Discount scenario: Country risk score \n feed_price_index: High feed cost")
     
  ggsave(paste0(fig_folder,'risk_high_feed_map.png'), width = 6, height = 5)
  
### Playing around with raster plotting for farm levels maps (not done)
  
  farm_npv<-all_df %>%
    filter( npv>0) %>%
    left_join(eez.water)  %>%
    rename(Discount_scenario = disc_scenario)
  
  farm_npv$feed_price_index<-ifelse(farm_npv$feed_price_index==0.9,"Low feed cost","High feed cost")
  farm_npv$Discount_scenario<-ifelse(farm_npv$Discount_scenario==0, "0 %", "Country risk score")
  
  base_raster<-raster(paste0(boxdir,"Suitability/tmp/carib_eez_raster.tif"))
  
  farm_data<-farm_npv %>%
    filter(Discount_scenario == "0 %" & feed_price_index =="High feed cost")
    
  points<-farm_data%>%
    select(long,lat)
  
   npv_sp<-SpatialPointsDataFrame(coords = points, data = farm_data)
   
   npv_raster<-rasterize(npv_sp,base_raster, "npv")
   
   gplot(npv_raster)+geom_tile(aes(fill=value))
  
 farm_example_map<-  gplot(npv_raster) + 
    geom_tile(aes(fill = value)) +
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("10 year NPV ($)") +
    theme_minimal() + 
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
  

   


  # Histogram of NPV by cell
 
   npv_cell<-all_df %>%
     filter(npv>0)
   
  ggplot(data = npv_cell, aes(x=npv, fill=scenario_names))+
    geom_histogram(position = "identity", bins=50) +
    theme_minimal() +
    xlab("NPV ($)") +
    ylab("Frequency") +
    facet_wrap(~scenario_names) +
    ggtitle("Farm Level 10 yr NPV ($)") +
    theme(strip.text.x = element_blank()) +
    guides(fill=guide_legend(title="Scenario"))
  ggsave(paste0(fig_folder,'Caribbean_npv_cell.png'), width = 6, height = 5)

   # # Histogram of NPV by cell for each Territory
  ggplot(data = npv_cell, aes(x=npv, fill=scenario_names))+
    geom_histogram(position = "identity") +
    theme_minimal() +
    xlab("NPV ($)") +
    ylab("Frequency") +
    facet_wrap(~scenario_names) +
    ggtitle("Farm Level 10 yr NPV ($)") +
    facet_wrap(~Territory1, scales = "free_y") +
    guides(fill=guide_legend(title="Scenario"))
ggsave(paste0(fig_folder,'eez_npv_cell.png'), width = 12, height = 12)


# Histograms of  avg harvest cycle length ---------------------------------

jpeg(paste0(fig_folder,"harv_length_hist.png"))

hist(harv_cycle_length[[1]], maxpixels = 1000000,
                                    main = "Distribution of harvest cycle lengths",xlab= "Harvest cycle length (months)")

dev.off()

# Boxplot of average growth by month

cells<-as.vector(Which(avg_growth[[1]]>0, cells =TRUE))

countries<-all_df %>%
  select(cell,Territory1)

growth_df<-as.data.frame(avg_growth) %>%
  filter(!is.na(X1)) %>%
  cbind(cells) %>%
  set_names(c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec","cell")) %>%
  gather("Month","avg_growth",1:12)

growth_df<-left_join(growth_df,countries)

#Box plot of avergage growth per month for whole Carib

ggplot(growth_df, aes(x=fct_relevel(Month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")) ,y=avg_growth)) +
  geom_boxplot()+
  theme_minimal() +
  xlab("Month") +
  ylab("Average Growth")

ggsave(paste0(fig_folder,'Caribbean_avg_growth.png'), width = 6, height = 5)

ggplot(growth_df, aes(x=fct_relevel(Month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")),y=avg_growth)) +
  geom_boxplot()+
  theme_minimal() +
  xlab("Month") +
  ylab("Average Growth") +
  facet_wrap(~Territory1)

ggsave(paste0(fig_folder,'EEZ_avg_growth.png'), width = 12, height = 12)


