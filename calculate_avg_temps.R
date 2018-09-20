######################################################################
## Function purpose: Calculate average temperature per cell per month
## Date: 09/19/2018
## Author: Tyler Clavelle
######################################################################

# Packages
library(tidyverse)
library(raster)

## Set User (lennon/tyler)
user <- 'tyler'
if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

# Load temperature data
temp <- brick(paste0(boxdir, 'sst/final1k/2005_2014 monthly SST_1k.nc'))

# Helper function for average temp
avg_temp<- function(t){
  # month index
  month_dex<-rep(1:12, 10)
  # average for each month
  monthly_temp <- raster::stackApply(t, month_dex, fun = mean)
  
  return(monthly_temp)
}

# Calculate monthly average temp
month_temps <- avg_temp(temp)

# Find cell index (use just first layer of raster brick)
cell_index <- Which(!is.na(month_temps[[1]]),cells=TRUE)

# Average monthly growth data frame
avg_m_temp <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
colnames(avg_m_temp) <- c('cell', c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))
avg_m_temp$cell <- cell_index # Add in indexes of active farms
avg_m_temp[,2:13] <- month_temps[cell_index] # Add in monthly average temperatures

# # Load suitability results to get EEZs associated with each cell
# suitable_cells <- read_csv(paste0(boxdir,"results/Suitability/suitable_area_df.csv"))
# s_areas <- gzfile(paste0(boxdir,"results/Suitability/suitable_areas.rds"))
# suit_areas <- readRDS(s_areas)
# 
# # Join temperature results with EEZ and Country data
# avg_temp_results <- suitable_cells %>% 
#   select(cell_no, eez, country) %>% 
#   rename(cell = cell_no) %>% 
#   left_join(avg_m_temp)
# 
# # Calculate EEZ wide average temps for suitable farms
# eez_temp_summary <- avg_temp_results %>% 
#   gather(key = 'month', value = 'temperature', 4:ncol(.)) %>% 
#   group_by(country, month) %>% 
#   summarize(temp_mean = mean(temperature, na.rm = T),
#             temp_sd   = sd(temperature, na.rm = T)) %>% 
#   mutate(month = fct_relevel(month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")))
# 
# # Plot of average temperatures by EEZ
# eez_temp_summary %>% 
#   ggplot(aes(y = temp_mean, x = country)) +
#   geom_line() +
#   coord_flip()

