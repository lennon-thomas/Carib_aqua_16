##################################################
## Project: Carib Aquaculture
## Script purpose: Explore growth results
## Date: 09/21/2018
## Author: Tyler Clavelle
##################################################

library(tidyverse)
library(raster)
library(scales)

## Set User (lennon/tyler)
user <- 'tyler'
if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 10),
          axis.text    = element_text(size = 8),
          legend.text  = element_text(size = 8))
}

# Read in growth data from thermal performance curve
growth <- brick(paste0(boxdir, 'data/economic_final/cobia_growth.nc'))

# Helper function to calculate mean growth across the stack
avg_growth<- function(growth){
  
  month_dex<-rep(1:12,10)
  
  #take average for each month
  monthly_growth<- raster::stackApply(growth, month_dex, fun = mean)
  
  return(monthly_growth)
}

# Mean growth
month_growth <- avg_growth(growth)

# Load suitability results to get EEZs associated with each cell
suitable_cells <- read_csv(paste0(boxdir,"data/cell_area_df.csv"))

# Find indexes of suitable cells
cell_index <- suitable_cells %>% 
  filter(suit_index == 1) %>% .$cell_no

# Average monthly growth data frame
avg_m_growth <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
colnames(avg_m_growth) <- c('cell', c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))
avg_m_growth$cell <- cell_index # Add in indexes of active farms
avg_m_growth[,2:13] <- month_growth[cell_index] # Add in monthly average temperatures

# Join temperature results with EEZ and Country data
avg_growth_results <- suitable_cells %>%
  filter(suit_index == 1) %>%
  dplyr::select(cell_no, eez, country) %>%
  rename(cell = cell_no) %>%
  left_join(avg_m_growth) %>% 
  gather(key = 'month', value = 'growth', 4:ncol(.))

# Get eez summaries of avg growth by month
growth_summary <- avg_growth_results %>%
  filter(!is.na(growth)) %>% 
  group_by(country, month) %>%  
  summarize(farm_months = length(month),
            growth_mean   = mean(growth, na.rm = T),
            growth_median = median(growth, na.rm = T),
            growth_sd     = sd(growth, na.rm = T)) %>% 
  mutate(month = fct_relevel(month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")))

# Overall caribbean average growth for midpoint
carib_avg <- avg_growth_results %>%
  summarize(avg_carib_growth = mean(growth, na.rm = T),
            median_carib_growth = median(growth, na.rm = T))
            
# Plot of average temperatures by EEZ
ggplot(growth_summary, aes(y = fct_reorder(country, growth_median),  x = month, fill = growth_median)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = unique(carib_avg$median_carib_growth), low = muted("blue"), high = muted("green")) +
  labs(y = "Country",
       x = "Month",
       fill = "Median\ngrowth")
