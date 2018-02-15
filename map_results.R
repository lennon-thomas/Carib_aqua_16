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
  #library(plotly)
  # library(plyr)
  
  # Run settings -------------------------------------------------------------
  
  ## Set User (lennon/tyler)
  user <- 'lennon'
  
  if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
  if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}
  
  run_name = 'est_feb_13'

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
  s_areas<-gzfile(paste0(boxdir,"/results/Suitability/suitable_areas.rds"))
  
  suit_areas<-readRDS(s_areas)
  
  suit_df<-read_csv(paste0(boxdir,"/results/Suitability/suitable_area_df.csv"),
                    col_types = "iiddiic") %>%
    select(c("cell_no","study_area_km","suit_area_km","suit_index","eez","country"))
  
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
#"calc_0.02" #run name reflects intital stocking density (calculated or fixed)and feed rates (as % body weight)

  result_folder <- paste0(boxdir,'results/',run_name,"/Results")
  
  file.names <- list.files(path = result_folder, pattern = ".csv")
  
  result_files <- lapply(paste0(result_folder,"/",file.names),read_csv)
  
  carib_supply<-result_files[[1]]
  
  eez_supply_df<-result_files[[2]]
  
  npv_df<-result_files[[4]]
  
  supply_summary<-result_files[[6]]
  
  rm(result_files)

# Prep data for plotting --------------------------------------------------

  tidy_eez<-tidy(EEZ)
  
  temp_df <- data.frame(EEZ@data)
  
  temp_df$id <- seq(0,nrow(temp_df)-1)
  
  EEZ_df <- merge(tidy_eez, temp_df, by="id")
  
  cells<-as.vector(Which(suit_areas>0, cells =TRUE))
  
  raster_coords<-as_data_frame(rasterToPoints(suit_areas))
  
  suit_coords<-cbind(cells,raster_coords) %>%
    set_names(c("cell","long","lat","suitable"))
  
  all_df<-left_join(npv_df,suit_coords)
  
  all_df<-dplyr:: rename(all_df,Territory1 = country)


# Create base map of land and water in Carib ------------------------------


eez.land<-EEZ_df %>%
  filter(hole == TRUE)

eez.water<-EEZ_df %>%
  filter(hole == FALSE)

base<- ggplot() + 
        geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.1 , alpha = 0.5) +
        geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
        theme(legend.position="none") +
        theme_minimal() +
        xlab("Longitude") +
        ylab("Latitude") +
        coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))

  base_facet<-  ggplot() + 
                  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.1 , alpha = 0.5) +
                  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
                  theme(legend.position="none") +
                  theme_minimal() +
               #   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                  xlab("Longitude") +
                  ylab("Latitude") +
                  facet_wrap(~Territory1,scales = "free")
                  #facet_wrap_paginate(~Territory1,scales="free",ncol = 1, nrow = 2, page = i) 
         


# Suitable area maps -----------------------------------------------------

                  
  suitable_plot<-base +
                    geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="darkred") +
                    theme(legend.position = "none") +
                    ggtitle("Suitable Areas for Offshore Mariculture")         
    
  ggsave(filename = paste0(fig_folder,'carib_suit_area.png'), width = 6, height = 5)
                                    
  suitable_plot_facet<-base_facet +
                          geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="darkred") +
                          theme(legend.position = "none") +
                          ggtitle("Suitable Areas for Offshore Mariculture")

  ggsave(filename = paste0(fig_folder,'eez_suitable_area.png'), width = 12, height = 12)

# Production maps ---------------------------------------------------------

# Production if don't consider economics
  all_df$eez<-as.factor(all_df$eez)
  
  total_prod<- all_df %>%
    filter(prices==8.62 & discounts ==0) %>%
    dplyr::group_by(eez) %>%
    summarise(t_harvest = sum(total_harvest)) %>%
    mutate(total_harvest_mt = t_harvest * 0.001) %>%
   left_join(eez.water, by = c('eez' = 'MRGID'))
  
 # with no economic consideration 
  total_prod_map<-  ggplot() + 
    geom_polygon(data = total_prod,aes(x = long,y = lat,group = group, fill= total_harvest_mt), colour = "black", size = 0.1 , alpha = 0.8) +
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("Total Production (mt)") +
    ggtitle("(all suitable cells)") +
    theme_minimal() + 
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
  
  ggsave(paste0(fig_folder,'total_prod_map.png'), width = 6, height = 5)
  
  #Bar stacked bar plot of total production by EEZ for profitable cells
  
  
 econ_prod<- all_df %>%
    filter(prices==8.62 & npv>0) %>%
    dplyr::group_by(eez,discounts) %>%
    summarise(all_harvest = sum(total_harvest*0.001)) %>%
    spread(discounts, all_harvest) %>%
    set_names(c("eez","discount_0","discount_0.5","discount_0.1"))
 
e_prod<-gather(econ_prod,"discount","harvest_mt",c(2:4))%>%
  as_data_frame()

t_prod<-total_prod %>%
  select(eez,total_harvest_mt)%>%
  gather("discount","harvest_mt",2) %>%
  as_data_frame() 

c_names<-as.data.frame(EEZ@data)%>%
  select(MRGID,Territory1)

all_prod<-bind_rows(e_prod,t_prod) %>%
  set_names(c("MRGID","discount","harvest_mt")) 

final_prod<-left_join(all_prod,c_names)
  
  ggplot(final_prod,aes(x=discount,y=harvest_mt,fill = Territory1))+
    geom_bar(stat="identity") +
    theme_minimal() +
    ylab("Total Harvest (mt)") +
    xlab("Economic Scenario")
# NPV maps/histograms for current price -----------------------------------------------------

  # Current price and only profitable farms by EEZ
  npv_eez<-all_df %>%
    filter(prices==8.62 & npv>0) %>%
    rename(Discount_rate = discounts) %>%
    group_by(Territory1,Discount_rate) %>%
    summarise(total_npv = sum(npv),
              total_production = sum(total_harvest * 0.001)) %>%
    left_join(eez.water)
  
  # Map of NPV by discount rate (need to plot on separate pages)
  
  npv_discount_map<-  ggplot() + 
    geom_polygon(data = npv_eez,aes(x = long,y = lat,group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("10 year NPV ($)") +
    theme_minimal() + 
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
    facet_wrap(~Discount_rate,labeller = "label_both", nrow =3, ncol=1)
  #  facet_grid_paginate(~Discount_rate,labeller = "label_both", page=3, ncol = 3)
  ggsave(paste0(fig_folder,'npv_discount_map.png'), width = 6, height = 5)

 
  ggplot(npv_eez, aes(x=total_npv, y=total_production, col=Territory1))+
    geom_point()+
    #geom_label(aes(label=Territory1))+
    facet_wrap(~Discount_rate, labeller = "label_both") +
    theme_minimal()+
    xlab("Total Production (mt)") +
    ylab("10 yr NPV ($)")
   
  ggsave(paste0(fig_folder,'npv_v_production.png'), width = 12, height = 12)  
  # Current price and only profitable farms by cell
  
   
  npv_cell<-all_df %>%
    filter(prices==8.62 & npv>0) %>%
    rename(Discount_rate = discounts) 

npv_cell$Discount_rate<-as.factor(npv_cell$Discount_rate)

  # Histogram of NPV by cell
ggplot(data = npv_cell, aes(x=npv,fill=Discount_rate))+
  geom_histogram( bins=100) +
  theme_minimal() +
  xlab("NPV ($)") +
  ylab("Frequency") +
  ggtitle("Carribbean NPV by cell")
ggsave(paste0(fig_folder,'Caribbean_npv_cell.png'), width = 6, height = 5)

   # # Histogram of NPV by cell for each Territory
ggplot(data = npv_cell, aes(x=npv,fill=Discount_rate))+
  geom_histogram( bins=100) +
  theme_minimal() +
  xlab("NPV ($)") +
  ylab("Frequency") +
  ggtitle("NPV by cell") +
  facet_wrap(~Territory1, scales = "free_y")
ggsave(paste0(fig_folder,'eez_npv_cell.png'), width = 12, height = 12)


# Histograms of  avg harvest cycle length ---------------------------------


carib_harv_length_plot<- hist(harv_cycle_length[[1]], maxpixels = 1000000,
                                    main = "Distribution of harvest cycle lengths",
                                    xlab = "Harvest cycle length (months)", ylab = "Frequency")


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


