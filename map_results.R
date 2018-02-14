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
#library(plotly)
# library(plyr)

# Run settings -------------------------------------------------------------

## Set User (lennon/tyler)
user <- 'lennon'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

run_name = 'est_feb_8'  
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
```



# Prep data for plotting --------------------------------------------------

library(broom)

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


#Create eparate out land from water and plot as separate layers

eez.land<-EEZ_df %>%
  filter(hole==TRUE)

eez.water<-EEZ_df %>%
  filter(hole==FALSE)

base<- ggplot() + 
  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.1 , alpha = 0.5) +
  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
  theme(legend.position="none") +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))

  base_facet<- ggplot() + 
                  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.1 , alpha = 0.5) +
                  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
                  theme(legend.position="none") +
                  theme_minimal() +
                  coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
                  xlab("Longitude") +
                  ylab("Latitude") +
                  facet_wrap(~Territory1,scales="free") +
   


# Suitable area maps -----------------------------------------------------

                  
suitable_plot<-base +
                  geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="darkred") +
                  theme(legend.position = "none") +
                  ggtitle("Suitable Areas for Offshore Mariculture")                                  
                                  
suitable_plot_facet<-base_facet +
                        geom_raster(data = all_df, aes(x=long,y=lat,fill=suitable),fill="darkred") +
                        theme(legend.position = "none") +
                        ggtitle("Suitable Areas for Offshore Mariculture")


# NPV maps for current price -----------------------------------------------------

current_price<-all_df %>%
  filter(prices==8.62 & npv>0) %>%
  rename(Discount_rate = discounts)

npv_plot<-
  for (i in 1:length(unique(current_price$Discount_rate))){
         base +
            geom_raster(data = current_price, aes(x=long,y=lat,fill=npv)) +
            ggtitle("10 year NPV ($)") +
            scale_fill_viridis() +
            facet_wrap_paginate(~Discount_rate,ncol=1,nrow=1,labeller = label_both,page = i)
  }


current_price_no_discount<-all_df %>%
  filter(prices==8.62 & discounts==0 & npv > 0) %>%
  rename(Discount_rate = discounts)

no_discount_plot<- 
    base_facet + 
    geom_raster(data = current_price_no_discount, aes(x=long,y=lat,fill=npv)) +
    ggtitle("10 year NPV ($)") +
    scale_fill_viridis() +
    facet_wrap(~Territory1, scales = "free")
  

no_cycles<-as.raster(no_cycles[[1]])
