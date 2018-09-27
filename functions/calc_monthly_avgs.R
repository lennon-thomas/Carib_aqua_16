######################################################################
## Function purpose: Calculate monthly average temperature and growth for every suitable cell
## Date: 09/19/2018
## Author: Tyler Clavelle
######################################################################

calc_monthly_avgs <- function(boxdir, data_folder) {
  
  # Load temperature and growth data
  temp <- brick(paste0(boxdir, 'sst/2007_2016_monthly_SST.nc'))
  growth <- brick(paste0(boxdir, 'data/economic_final/cobia_growth.nc'))
  
  # Helper functions --------------------------------------------------------
  
  # Cell average across raster stack
  stack_avg <- function(t){
    # month index
    month_dex<-rep(1:12, 10)
    # average for each month
    monthly_avg <- raster::stackApply(t, month_dex, fun = mean)
    return(monthly_avg)
  }
  
  # Analysis ----------------------------------------------------------------
  
  # Calculate monthly average temp
  month_temps <- stack_avg(temp)
  month_growth <- stack_avg(growth)
  
  # Load suitability results to get EEZs associated with each cell
  suitable_cells <- read_csv(paste0(boxdir,"data/cell_area_df.csv"))
  
  # Find cell index (use just first layer of raster brick)
  cell_index <- suitable_cells %>% 
    filter(suit_index == 1) %>% .$cell_no
  
  # Average monthly  temperature and growth data frames
  avg_m_temp <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
  colnames(avg_m_temp) <- c('cell', c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))
  avg_m_temp$cell <- cell_index # Add in indexes of active farms
  avg_m_temp[,2:13] <- month_temps[cell_index] # Add in monthly average temperatures
  
  avg_m_growth <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
  colnames(avg_m_growth) <- c('cell', c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))
  avg_m_growth$cell <- cell_index # Add in indexes of active farms
  avg_m_growth[,2:13] <- month_growth[cell_index] 
  
  # Join results with EEZ and Country data
  avg_temp_results <- suitable_cells %>%
    filter(suit_index == 1) %>% 
    dplyr::select(cell_no, eez, country) %>%
    rename(cell = cell_no) %>%
    left_join(avg_m_temp) %>% 
    gather(key = 'month', value = 'value', 4:ncol(.)) %>% 
    mutate(metric = 'temperature') %>% 
    filter(!is.na(value))
  
  avg_growth_results <- suitable_cells %>%
    filter(suit_index == 1) %>% 
    dplyr::select(cell_no, eez, country) %>%
    rename(cell = cell_no) %>%
    left_join(avg_m_growth) %>% 
    gather(key = 'month', value = 'value', 4:ncol(.)) %>% 
    mutate(metric = 'growth') %>% 
    filter(!is.na(value))
  
  # Bind temperature and growth results dataframes
  avg_results <- bind_rows(avg_temp_results, avg_growth_results)
  
  # Save csv of cell averages
  write_csv(avg_results, path = paste0(data_folder,'monthly_temp_and_growth.csv'))
  
  return(avg_results)

}
