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
  library(ggpubr)

#  library(rasterVis)
  #library(plotly)
  # library(plyr)
  
  # Run settings -------------------------------------------------------------
  
  ## Set User (lennon/tyler)
  user <- 'lennon'
  
  if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
  if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}
  
  run_name = '2018-02-27_est'

# Create figure folder
  fig_folder <- paste0(boxdir,'results/',run_name, "/Figures/")

  if (dir.exists(fig_folder) == F) {
  dir.create(fig_folder, recursive = T)
} else {
  
  print('Folder already exists')
}
  
 # Set figure theme 
  carib_theme <- function() {
    theme_minimal() +
      theme(text         = element_text(size = 6),
            title        = element_text(size = 10),
            axis.text    = element_text(size = 8),
            legend.text  = element_text(size = 8))
  }  
  
# Load EEZ shapefile for plotting

  EEZ = readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape")
  
  # Load suitablity results
  s_areas<-gzfile(paste0(boxdir,"results/Suitability/suitable_areas.rds"))
  
   suit_areas<-readRDS(s_areas)
  # 
   suit_df<-read_csv(paste0(boxdir,"/results/Suitability/suitable_area_df.csv"),
                     col_types = "iiddiic") %>%
     dplyr::select(c("cell_no","study_area_km","suit_area_km","suit_index","eez","country"))
  # 
   suit_df_summary<- suit_df %>%
     group_by(country) %>%
     summarise(suitable_area_km = round(sum(suit_area_km,na.rm = TRUE),2),
               total_area_km = round(sum(study_area_km,na.rm =TRUE),2)) %>%
     mutate(suitable_perc = round(suitable_area_km / total_area_km * 100,2)) %>%
     arrange(desc(suitable_perc)) %>%
     set_names(c("Country","Suitable area (km^2)","Total EEZ area (km^2)","Suitable area (% of EEZ)"))
    
  suit_df_summary<-read_csv(paste0(boxdir,"results/Suitability/suit_df_summary.csv"))       
  
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

ggsave( paste0(fig_folder,"study_area.png"), width = 6, height = 5)

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
                    carib_theme() +
                    theme(legend.position = "none") +
                       
    
  ggsave(filename = paste0(fig_folder,'carib_suit_area.png'), width = 6, height = 5)
                                    
  suitable_plot_facet<-base_facet +
                          geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="orange") +
                          theme(legend.position = "none") +
                          ggtitle("Suitable Areas for Offshore Mariculture")

  ggsave(filename = paste0(fig_folder,'eez_suitable_area.png'), width = 12, height = 12)

# Production and NPV by EEZ maps  ---------------------------------------------------------

# Production if don't consider economics
  
  all_df$eez<-as.factor(all_df$eez)
  all_df$disc_scenario<-as.factor(all_df$disc_scenario)
  all_df$feed_price_index<-as.factor(all_df$feed_price_index)
  
  
  total_prod<- all_df %>%
    filter(feed_price_index == '1' & disc_scenario == '0.1' ) %>%
    group_by(eez) %>%
    summarise(eez_harvest_mt = sum(total_harvest)* 0.001) %>%
    mutate(annual_eez_harvest = eez_harvest_mt/10, 
            scenario_names = "All suitable") %>%
    ungroup() %>%
    dplyr::select(eez,scenario_names,eez_harvest_mt,annual_eez_harvest) 
 
  total_prod_sp<-left_join(total_prod,eez.water,by=c("eez"="MRGID"))
   
  ggplot() + 
    geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
    geom_polygon(data = total_prod_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest), colour = "black", size = 0.1 , alpha = 0.8) +
 
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("Average Annual \n Production (mt)") +
    # ggtitle("(all suitable cells)") +
    carib_theme() + 
    theme(legend.title.align =0.5,legend.position=c(0.87,0.87),legend.background = element_rect( fill = "white", color = "white")) +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
  
  ggsave(paste0(fig_folder,'total_prod_map.png'), width = 6, height = 5)
  
  
  
  ## Production and npv - for .10 disount rate- only consider profitable cells
  
  
  econ_prod<- all_df %>%
    filter(disc_scenario == '0.1' & npv > 0 ) %>%
    dplyr::group_by(eez,feed_price_index) %>%
    summarise(eez_harvest_mt = sum(total_harvest) * 0.001,
              annual_eez_harvest = eez_harvest_mt/10,
              total_npv = sum(npv)) %>%
    ungroup() %>%
       mutate(scenario_names =  ifelse( feed_price_index == "1", "Current feed cost", "Reduced feed cost"))
 
   econ_prod_sp<-left_join(econ_prod,eez.water, by=c("eez"="MRGID"))         
   
   
  # plot 
 
 econ_prod_map<- ggplot() + 
                      geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
                      geom_polygon(data = econ_prod_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest), colour = "black", size = 0.1 , alpha = 0.8) +
                      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
                      scale_fill_viridis("Average Annual \n Production (mt)") +
                      carib_theme() + 
                      xlab("Longitude") +
                      ylab("Latitude") +
                      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                      facet_wrap(~scenario_names) +
                      theme(strip.text.x = element_text(size = 12))
                    
 econ_npv_map<- ggplot() + 
                      geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
                     geom_polygon(data = econ_prod_sp,aes(x = long,y = lat, group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
                     geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
                     scale_fill_viridis("10 Year NPV ($)") +
                     carib_theme() + 
                     xlab("Longitude") +
                     ylab("Latitude") +
                     coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                     facet_wrap(~scenario_names) +
                     theme(strip.background = element_blank(), strip.text = element_blank())

 
all_econ<-  ggarrange(econ_prod_map,econ_npv_map, nrow = 2)
  
ggsave(paste0(fig_folder,'econ_npv_prod_map.png'), width = 12, height = 10)  
  
  

# Box plots of total production --------------------------------------------


invest_scenario<-all_df %>%
  filter(npv>0) %>%
  mutate(scenario_names =  ifelse(disc_scenario == "0.1" & feed_price_index == "1", "10 % discount \n Current feed cost",
                                         ifelse (disc_scenario =="0.1" & feed_price_index == "0.9", "10 % discount \n Low feed cost",
                                                 ifelse(disc_scenario =="cntry" & feed_price_index == "1", "Country specific discount /n High feed",
                                                        "Country specific discount /nLow feed")))) 

ggplot(invest_scenario) +
  geom_boxplot(aes(x = scenario_names, y= npv))
    
    
  all_eez_prod<-bind_rows(econ_prod_feed,total_prod)  
  
  final_prod<-all_eez_prod %>%
    group_by(scenario_names) %>%
    summarize(total_annual_eez_harvest = sum(annual_eez_harvest))
  
  ggplot(final_prod,aes(x=fct_relevel(scenario_names,c("All suitable", "Low feed","High feed")),y=total_annual_eez_harvest))+
    geom_bar(stat="identity") +
    theme_minimal() +
    ylab("Average Annual Harvest (mt)") +
    xlab("Scenario") 
  
  
  ggsave(paste0(fig_folder,"production.png"),width=5, height=6)
  

# Production maps and bar plots -------------------------------------------



  econ_prod_cntry_disc_sp<- left_join(econ_prod_cntry_disc,eez.water, by=c("eez"="MRGID"))    
  
  
  
 
      ggplot() +
      geom_polygon(data = econ_prod_cntry_disc_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest), colour = "black", size = 0.1 , alpha = 0.8) +
      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
      scale_fill_viridis("Average Annual Production (mt)") +

      theme_minimal() + 
      xlab("Longitude") +
      ylab("Latitude") +
      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
    
    ggsave(paste0(fig_folder,'econ_prod_map.png'), width = 6, height = 5)
    
    
  
    
  ggplot() +
      geom_polygon(data = econ_prod_feed_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest), colour = "black", size = 0.1 , alpha = 0.8) +
      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
      scale_fill_viridis("Average Annual Production (mt)") +

      theme_minimal() + 
      xlab("Longitude") +
      ylab("Latitude") +
      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
      facet_wrap(~feed_price_index,nrow =2, ncol =1, labeller = label_both)
    
    ggsave(paste0(fig_folder,'feed_prod_map.png'), width = 6, height = 5 )
    
   

# NPV maps ----------------------------------------------------------------

    ggplot() +
      geom_polygon(data = econ_prod_cntry_disc_sp,aes(x = long,y = lat, group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
      scale_fill_viridis("10 yr NPV ($)") +
    #  ggtitle("(Country specific discount, High Feed Cost)") +
      theme_minimal() + 
      xlab("Longitude") +
      ylab("Latitude") +
      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
    
    ggsave(paste0(fig_folder,'econ_npv_map.png'), width = 6, height = 5)
    
    ggplot() +
      geom_polygon(data = econ_prod_feed_sp,aes(x = long,y = lat, group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
      scale_fill_viridis("10 yr NPV ($)") +
     # ggtitle("(Country specific discount)") +
      theme_minimal() + 
      xlab("Longitude") +
      ylab("Latitude") +
      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
      facet_wrap(~feed_price_index,nrow =2, ncol =1, labeller = label_both)
    
    ggsave(paste0(fig_folder,'feed_npv_map.png'), width = 6, height = 5)
    

# NPV maps/histograms for cntry specific discouont -----------------------------------------------------

  # Histogram of NPV by cell
 
#    npv_cell<-all_df %>%
#      filter(npv>0)
#    
#   ggplot(data = npv_cell, aes(x=npv, fill=scenario_names))+
#     geom_histogram(position = "identity", bins=50) +
#     theme_minimal() +
#     xlab("NPV ($)") +
#     ylab("Frequency") +
#     facet_wrap(~scenario_names) +
#     ggtitle("Farm Level 10 yr NPV ($)") +
#     theme(strip.text.x = element_blank()) +
#     guides(fill=guide_legend(title="Scenario"))
#   ggsave(paste0(fig_folder,'Caribbean_npv_cell.png'), width = 6, height = 5)
# 
#    # # Histogram of NPV by cell for each Territory
#   ggplot(data = npv_cell, aes(x=npv, fill=scenario_names))+
#     geom_histogram(position = "identity") +
#     theme_minimal() +
#     xlab("NPV ($)") +
#     ylab("Frequency") +
#     facet_wrap(~scenario_names) +
#     ggtitle("Farm Level 10 yr NPV ($)") +
#     facet_wrap(~Territory1, scales = "free_y") +
#     guides(fill=guide_legend(title="Scenario"))
# ggsave(paste0(fig_folder,'eez_npv_cell.png'), width = 12, height = 12)


# Histograms of  avg harvest cycle length ---------------------------------

jpeg(paste0(fig_folder,"harv_length_hist.jpeg"))

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


