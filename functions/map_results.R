  # Load packages and functions-----------------------------------------------------------
  # rm(list = ls())
  
  # library(raster)
  # library(rgdal)
  # library(tmap)
  # library(ncdf4)
  # library(stringr)
  # library(broom)
  # library(fasterize)
  # library(sf)
  # library(tidyverse)
  # library(parallel)
  # library(R.utils)
  # library(readr)
  # library(pander)
  # library(scales)
  # library(viridis)
  # library(ggforce)
  # library(forcats)
  # library(ggpubr)

  map_results <- function(boxdir,
                          fig_folder,
                          avg_growth,
                          harv_cycle_length,
                          harvest_cycles,
                          stocking_n,
                          carib_supply,
                          eez_supply,
                          npv_df,
                          supply_summary,
                          feed_price,
                          price_fingerlings) {
    
  
# Load EEZ shapefile for plotting

  EEZ <- readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape") 
  
  # Load suitablity results
  s_areas <- gzfile(paste0(boxdir,"results/Suitability/suitable_areas.rds"))
  
   suit_areas <- readRDS(s_areas)
   
   # suit_df <- read_csv(paste0(boxdir,"/results/Suitability/suitable_area_df.csv"),
   #                   col_types = "iiddiiccccddddcc") %>%
   #   dplyr::select(c("cell_no","study_area_km","suit_area_km","suit_index","eez","country"))
   # 
   # suit_df_summary <- suit_df %>%
   #   group_by(country) %>%
   #   summarise(suitable_area_km = round(sum(suit_area_km,na.rm = TRUE),2),
   #             total_area_km = round(sum(study_area_km,na.rm =TRUE),2)) %>%
   #   mutate(suitable_perc = round(suitable_area_km / total_area_km * 100,2)) %>%
   #   arrange(desc(suitable_perc)) %>%
   #   set_names(c("Country","Suitable area (km^2)","Total EEZ area (km^2)","Suitable area (% of EEZ)"))
    
  suit_df_summary <- read_csv(paste0(boxdir,"results/Suitability/suit_df_summary.csv"))       
  
# Load sim data
  
  # data_folder<-paste0(boxdir,'results/',run_name,"/Data/")
  # 
  # avg_growth<-brick(paste0(data_folder,"avg_month_growth_stack.nc"))
  # 
  # harv_cycle_length<-raster(paste0(data_folder,"harvest_cycle_length.nc"))
  # 
  # no_cycles<-brick(paste0(data_folder,"harvest_cycles.nc"))
  # 
  # stocking_n<-brick(paste0(data_folder,"initial_stocking_stack.nc"))

# Load run results 

  result_folder <- paste0(boxdir,'results/',run_name,"/Results")

# Prep data for plotting --------------------------------------------------

# This joins npv_df dataframe with spatial coordinates and EEZ of each cell number
  
  tidy_eez <- tidy(EEZ)
  
  temp_df <- data.frame(EEZ@data)
  
  temp_df$id <- seq(0,nrow(temp_df)-1)
  
  EEZ_df <- merge(tidy_eez, temp_df, by="id")
  
  cells <- as.vector(Which(suit_areas > 0, cells =TRUE))
  
  raster_coords <- as_data_frame(rasterToPoints(suit_areas))
  
  suit_coords <- cbind(cells,raster_coords) %>%
    set_names(c("cell","long","lat","suitable"))
  
  all_df <- npv_df %>%
    filter(prices == 8.62) %>%
    left_join(suit_coords) 
  
  
  all_df <- dplyr::rename(all_df, Territory1 = country) 

# Create base map of land and water in Carib ------------------------------

eez.land <- EEZ_df %>%
  filter(hole == TRUE)

eez.water <- EEZ_df %>%
  filter(hole == FALSE)

base <- ggplot() + 
        geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
        geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.07) +
        theme(legend.position="none") +
        carib_theme() +
        xlab("Longitude") +
        ylab("Latitude") +
        coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))

#ggsave( paste0(fig_folder,"study_area.png"), width = 6, height = 5)

  base_facet <- ggplot() + 
                  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
                  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.07) +
                  theme(legend.position="none") +
                  carib_theme() +
               #   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                  xlab("Longitude") +
                  ylab("Latitude") +
                  facet_wrap(~Territory1,scales = "free")
                  #facet_wrap_paginate(~Territory1,scales="free",ncol = 1, nrow = 2, page = i) 

# Suitable area maps -----------------------------------------------------

  # Caribbean suitability map (all Caribbean)                  
  suitable_plot <- base +
                    geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="orange") +
                    carib_theme() +
                    theme(legend.position = "none")
                       
    
  ggsave(filename = paste0(fig_folder,'carib_suit_area.png'), width = 6, height = 5)
                                    
  suitable_plot_facet <- base_facet +
                          geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="orange") +
                          theme(legend.position = "none") +
                          ggtitle("Suitable Areas for Offshore Mariculture")

  ggsave(filename = paste0(fig_folder,'eez_suitable_area.png'), width = 12, height = 12)

## Plot EEZ level suitability for just a couple of countries
  
  suitable_three <- ggplot() + 
    geom_polygon(data = subset(eez.water,Territory1 %in% c("Bahamas","Jamaica","Trinidad and Tobago")),aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
    geom_polygon(data = subset(eez.land,Territory1 %in% c("Bahamas","Jamaica", "Trinidad and Tobago")),aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.07) +
    geom_raster(data = subset(all_df,Territory1 %in% c("Bahamas","Jamaica","Trinidad and Tobago")), aes(x=long,y=lat,fill=suitable),fill="orange") +
     #theme(legend.position="none") +
    carib_theme() +
    theme(title=element_text(size=14), axis.title=element_text(size = 10)) +
    #   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    facet_wrap(~Territory1,scales = "free") +
    theme(strip.text.x = element_text(size = 12)) 
  
  suitable_plot_final <- suitable_plot + 
    suitable_three + 
    plot_layout(nrow = 2,
                heights = c(0.75,0.25))
  
  ggsave(filename = paste0(fig_folder,'carib_suitable_map.png'), width = 6.5, height = 6.5)
  
  ggplot() +
    geom_polygon(data = subset(eez.water,Territory1 %in% c("Haiti")),aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.1 , alpha = 0.5) +
    geom_polygon(data = subset(eez.land,Territory1 %in% c("Haiti")),aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    geom_raster(data = subset(all_df,Territory1 %in% c("Haiti") & npv > 0), aes(x=long,y=lat,fill=npv)) +
    scale_fill_viridis() +
    #theme(legend.position="none") +
    carib_theme() +
    theme(title=element_text(size=14), axis.title=element_text(size = 10)) +
    #   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
    xlab("Longitude") +
    ylab("Latitude")
   
  ggsave(filename = paste0(fig_folder,'Haiti_npv_map.png'), width = 5, height = 6)
  
# Production by EEZ map ---------------------------------------------------------

# Production if don't consider economics
  all_df$eez <- as.factor(all_df$eez)
  all_df$disc_scenario <- as.factor(all_df$disc_scenario)
  all_df$feed_price_index <- as.factor(all_df$feed_price_index)
  
  total_prod <- all_df %>%
    filter(feed_price_index == '1' & disc_scenario == '0.106' ) %>%
    group_by(eez) %>%
    summarise(eez_harvest_mt = sum(total_harvest)* 0.001) %>%
    mutate(annual_eez_harvest = eez_harvest_mt/10, 
            scenario_names = "All suitable") %>%
    ungroup() %>%
    dplyr::select(eez,scenario_names,eez_harvest_mt,annual_eez_harvest) 
 
  total_prod_sp <- left_join(total_prod,eez.water,by=c("eez"="MRGID"))
   
  ggplot() + 
    geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
    geom_polygon(data = total_prod_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest / 1e6), colour = "black", size = 0.1 , alpha = 0.8) +
 
    geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
    scale_fill_viridis("Average Annual Production (MMT)  ") +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    # ggtitle("(all suitable cells)") +
    carib_theme() + 
    theme(legend.position = 'bottom') +
    # theme(legend.title.align =0.5,legend.position=c(0.87,0.87),legend.background = element_rect( fill = "white", color = "white")) +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
  
  # ggsave(paste0(fig_folder,'total_prod_map.png'), width = 6, height = 5)
  
  # Production and NPV - for .10 disount rate- only consider profitable cells
  
  econ_prod <- all_df %>%
    filter(disc_scenario == '0.106' & npv > 0 ) %>%
    dplyr::group_by(eez,feed_price_index) %>%
    summarise(eez_harvest_mt = sum(total_harvest) * 0.001,
              annual_eez_harvest = eez_harvest_mt/10,
              total_npv = sum(npv, na.rm = T))%>%
    ungroup() %>%
       mutate(scenario_names = ifelse(feed_price_index == "1", "Current feed cost", "Reduced feed cost"))
 
   econ_prod_sp <- left_join(econ_prod,eez.water, by=c("eez"="MRGID"))         
   
# plot 
 econ_prod_map <- ggplot() + 
                      geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
                      geom_polygon(data = econ_prod_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest / 1e6), colour = "black", size = 0.1 , alpha = 0.8) +
                      geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
                      scale_fill_viridis("Average Annual Production (MMT) ") +
                      guides(fill = guide_colorbar(title.vjust = 0.75)) +
                      carib_theme() + 
                      xlab("Longitude") +
                      ylab("Latitude") +
                      coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
                      facet_wrap(~scenario_names) +
                      theme(strip.text.x = element_text(size = 12),
                            legend.position = 'bottom')
                    
 econ_npv_map <- ggplot() + 
   geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
   geom_polygon(data = econ_prod_sp,aes(x = long,y = lat, group = group, fill= total_npv / 1e9), colour = "black", size = 0.1 , alpha = 0.8) +
   geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
   scale_fill_viridis("10 yr NPV ($USD, billions) ", labels = comma) +
   guides(fill = guide_colorbar(title.vjust = 0.75)) +
   carib_theme() + 
   xlab("Longitude") +
   ylab("Latitude") +
   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
   facet_wrap(~scenario_names) +
   theme(strip.text.x = element_text(size = 12),
         legend.position = 'bottom')

 
all_econ <- ggarrange(econ_prod_map,econ_npv_map, nrow = 2)
  
ggsave(paste0(fig_folder,'econ_npv_prod_map.png'), width = 12, height = 10)  
  

# Suitable vs profitable production ---------------------------------------

eez_lookup <- all_df %>% 
  ungroup() %>% 
  select(eez, Territory1) %>% 
  distinct() %>% 
  rename(country = Territory1)

prod_compare_A <- supply_summary %>% 
  filter(country != "Caribbean") %>% 
  filter(disc_scenario == 'cntry' & feed_price_index == 1 & supply_scenario == "All farms") %>% 
  ungroup() %>% 
  select(country, total_supply, total_npv, median_supply, median_npv, top95_supply, top95_npv) %>% 
  left_join(eez_lookup) %>% 
  ungroup()

prod_compare_spA <- left_join(prod_compare_A,eez.water, by=c("eez"="MRGID"))

prod_compare_B <- supply_summary %>% 
  filter(country != "Caribbean") %>% 
  filter(disc_scenario == 'cntry' & feed_price_index == 1 & supply_scenario == "Profitable farms") %>% 
  ungroup() %>% 
  select(country, total_supply, total_npv, median_supply, median_npv, top95_supply, top95_npv) %>% 
  left_join(eez_lookup) %>% 
  ungroup()

prod_compare_spB <- left_join(prod_compare_B,eez.water, by=c("eez"="MRGID"))

# plot production and profit and put plots together 
prod_compare_A <- ggplot() + 
  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
  geom_polygon(data = prod_compare_spA, aes(x = long,y = lat, group = group, fill= total_supply / 1e6 / 10 ), colour = "black", size = 0.1 , alpha = 0.8) +
  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
  scale_fill_viridis("MMT") +
  guides(fill = guide_colorbar(title.vjust = 0.75)) +
  carib_theme() + 
  labs(x = "Longitude",
       y = "Latitude",
       title = "a)",
       subtitle = "Total annual production- 'suitable' scenario") +
  coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
  theme(strip.text.x = element_text(size = 16),plot.title=element_text(face = "bold"))

ggsave(paste0(fig_folder,'total_prod_map.png'), width = 6, height = 5)

prod_compare_B <- ggplot() + 
  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
  geom_polygon(data = prod_compare_spB, aes(x = long,y = lat, group = group, fill= total_supply /1e6/ 10), colour = "black", size = 0.1 , alpha = 0.8) +
  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
  scale_fill_viridis("MMT", labels = comma) +
  guides(fill = guide_colorbar(title.vjust = 0.75)) +
  carib_theme() + 
  labs(x = "Longitude",
       y = "Latitude",
       title = "b)",
       subtitle = "Total annual production- 'economic' scenario") +
  coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
  theme(strip.text.x = element_text(size = 12), plot.title=element_text(face = "bold"))

ggsave(paste0(fig_folder,'econ_prod_map.png'), width = 6, height = 5)

prod_compare_A + 
  prod_compare_B + 
  plot_layout(ncol = 1) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

ggsave(paste0(fig_folder,'prod_compare_map.png'), width = 6, height = 10) 

# Box plots of total production --------------------------------------------

# invest_scenario<-all_df %>%
#   filter(npv>0) %>%
#   dplyr::group_by(eez,feed_price_index, disc_scenario) %>%
#   summarise(eez_harvest_mt = sum(total_harvest) * 0.001,
#             annual_eez_harvest = eez_harvest_mt/10,
#             total_npv = sum(npv)) %>%
#   ungroup() %>%
#   mutate(scenario_names =  ifelse(disc_scenario == "0.1" & feed_price_index == "1", "10% discount rate\nCurrent feed price",
#                                          ifelse (disc_scenario =="0.1" & feed_price_index == "0.9", "10% discount rate\nReduced feed price",
#                                                  ifelse(disc_scenario =="cntry" & feed_price_index == "1", "Country specific discount rate\nCurrent feed price",
#                                                         "Country specific discount rate\nReduced feed price")))) 
# 
# invest_scenario_sp<-left_join(invest_scenario,eez.water, by=c("eez"="MRGID")) 
# 
# disc_scen_prod_map<- ggplot() + 
#   geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
#   geom_polygon(data = invest_scenario_sp,aes(x = long,y = lat, group = group, fill= annual_eez_harvest / 1e6), colour = "black", size = 0.1 , alpha = 0.8) +
#   geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
#   scale_fill_viridis("Average Annual Production (MMT) ") +
#   guides(fill = guide_colorbar(title.vjust = 0.75)) +
#   carib_theme() + 
#   xlab("Longitude") +
#   ylab("Latitude") +
#   coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
#   facet_wrap(~scenario_names) +
#   theme(strip.text.x = element_text(size = 12),
#         legend.position = 'bottom')
#  
# ggsave(paste0(fig_folder,'disc_scenario_prod_map.png'), width = 12, height = 10)    

# Discount scenario production map ----------------------------------------

disc_only <- all_df %>%
  filter(npv>0 & feed_price_index =="1") %>%
  group_by(eez,disc_scenario) %>%
  summarise(eez_harvest_mt = sum(total_harvest) * 0.001,
            annual_eez_harvest = eez_harvest_mt/10,
            total_npv = sum(npv)) %>%
  ungroup() %>%
mutate(scenario_name = ifelse( disc_scenario =="0.106", "10.6% discount rate","Country specific discount rate")) 


disc_only_sp <- left_join(disc_only,eez.water, by=c("eez"="MRGID"))

presentation_disc_map <- ggplot() + 
  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.15) +
  geom_polygon(data = disc_only_sp,aes(x = long,y = lat, group = group, fill= total_npv), colour = "black", size = 0.1 , alpha = 0.8) +
  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
  scale_fill_viridis("10 yr NPV ",begin = 0, end = 1) +
  guides(fill = guide_colorbar(title.vjust = 0.75) ) +
  carib_theme() + 
  xlab("Longitude") +
  ylab("Latitude") +
  coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30)) +
  facet_wrap(~scenario_name) +
  theme(strip.text.x = element_text(size = 12),
        legend.position = 'bottom') 
 # ggtitle(paste0("feed price =", feed_price, "fingerling price = ",price_fingerlings))

ggsave(paste0(fig_folder,'disc_scenario_npv_map.png'), width = 12, height = 10)    

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

# jpeg(paste0(fig_folder,"harv_length_hist.jpeg"))
# 
# hist(harv_cycle_length[[1]], maxpixels = 1000000,
#                                     main = "Distribution of harvest cycle lengths",xlab= "Harvest cycle length (months)")
# 
# dev.off()

# Boxplot of average growth by month

cells <- as.vector(Which(avg_growth[[1]]>0, cells =TRUE))

countries <- all_df %>%
  dplyr::select(cell,Territory1)

growth_df <- raster::as.data.frame(avg_growth) %>%
  filter(!is.na(index_1)) %>%
  cbind(cells) %>%
  set_names(c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec","cell")) %>%
  gather("Month","avg_growth",1:12)

growth_df <- left_join(growth_df,countries)
 
growth_plot_df <- growth_df %>% 
  group_by(Territory1, Month) %>% 
  summarize(growth = mean(avg_growth, na.rm = T),
            sd_growth  = sd(avg_growth, na.rm = T)) %>%
  group_by(Territory1) %>% 
  mutate(total_growth = sum(growth, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Month = fct_relevel(Month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")))

# get overall caribbean avg growth to use as midpoint in heatmap
carib_avg <- mean(growth_df$avg_growth, na.rm = T)

ggplot(growth_plot_df, aes(y = fct_reorder(Territory1, total_growth),  x = Month, fill = growth)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = unique(carib_avg), low = muted("blue"), high = muted("red")) +
  labs(y = "Country",
       x = "Month",
       fill = "Average\ngrowth (kg)")

ggsave(paste0(fig_folder,'growth_heatmap.png'), width = 8, height = 6)

# #Box plot of avergage growth per month for whole Carib
# ggplot(growth_df, aes(x=fct_relevel(Month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")) ,y=avg_growth)) +
#   geom_boxplot()+
#   theme_minimal() +
#   xlab("Month") +
#   ylab("Average Growth")
# 
# ggsave(paste0(fig_folder,'Caribbean_avg_growth.png'), width = 6, height = 5)
# 
# ggplot(growth_df, aes(x=fct_relevel(Month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")),y=avg_growth)) +
#   geom_boxplot()+
#   theme_minimal() +
#   xlab("Month") +
#   ylab("Average Growth") +
#   facet_wrap(~Territory1)
# 
# ggsave(paste0(fig_folder,'EEZ_avg_growth.png'), width = 12, height = 12)

return()
}

