
rm(list = ls())
library(raster)
library(rgdal)
library(tmap)
library(tmaptools)
library(tidyverse)

boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'


# Read in hurricane data layer from Chollet et al. paper and align with study area and resoultion
carib<-raster(paste0(boxdir,"Suitability/tmp/carib_depth.tif"))

hurricane<-raster(paste0(boxdir,"Suitability/raw/PECS/PECS_hurricanes.tif"))

hurricane_final<-raster::resample(hurricane,carib)

hurricane_final<-mask(hurricane_final,carib)

#number of months included in hurricane frequency representation

total_mon<-12*157

#Calculate hurricane probabilty for each cell

prob_hurricane<-calc(hurricane_final,function(x) {x/total_mon})
plot(prob_hurricane)

test_vector<-rep(0,120)

hurricane_data<-as_tibble(prob_hurricane, cell = TRUE, value = TRUE) %>%
  mutate(new_hurricane_prob=round(cellvalue*120),1,
         prob_vector=list(ifelse(!is.na(new_hurricane_prob),rep(1,new_hurricane_prob),NA)))
