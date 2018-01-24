##########################################################################
##
## Aquaculture simulation function
## Caribbean aquaculture project
##########################################################################

sim_aqua <- function(avg_month_growth, stocking_n, month_mort, int_weight, sim_length,fcr) {

# avg_month_growth <- raster(x = paste0(boxdir,'data/avg_month_growth_stack.nc',sep = ""))
# stocking_n <- raster(x = paste0(boxdir,'data/initial_stocking_stack.nc',sep = ""))
# month_mort <- 0.02368842
# int_weight <- 0.015
# sim_length <- 120
# fcr <- 2

# Find cell index
cell_index <- Which(!is.na(stocking_n),cells=TRUE)

# Average monthly growth data frame
avg_m_growth <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
colnames(avg_m_growth) <- c('cell', c(paste0('month_',1:12)))
avg_m_growth[,2:13] <- avg_month_growth[cell_index]
avg_m_growth$cell <- cell_index

# Number of harvest cycles for each site
harv_cycles <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 2))
colnames(harv_cycles) <- c('cell', 'harvest_cycles')
harv_cycles[,2] <- floor(harvest_cycles[cell_index])
harv_cycles$cell <- cell_index

# Initial stocking dataframe
stocking <- data_frame(cell = cell_index,
                       stocking = stocking_n[cell_index])

# Initialize storage data frame for results
results <- as_data_frame(matrix(NA, nrow = nrow(stocking) * (sim_length + 1), ncol = 8))
colnames(results) <- c('cell','month','alive','weight','mortality','harvest','feed', 'harvest_cycle')

results$month <- rep(0:(sim_length), each = nrow(stocking))
results$cell <- rep(cell_index, times = sim_length + 1)

# Initialize starting conditions
results$alive[results$month == 0] <- stocking$stocking
results$weight[results$month == 0] <- 0.015
results$mortality[results$month == 0] <- 0
results$harvest[results$month == 0] <- 0
# results$feed[results$month == 0] <- fcr*results$weight[results$month == 0]*  results$alive[results$month == 0]
# results$feed[results$month == 0] <- 0.02 * results$weight[results$month == 0] * results$alive[results$month == 0] * 30 # feeding 5% of total biomass every day of the month
results$feed[results$month == 0] <- 0
results$harvest_cycle[results$month == 0] <- 1

sim_cell <- function(x) {
  
  # Write table to show progress of projections
  write.table(
    paste(round(100 * (match(x, cell_index) / length(
      cell_index
    )), 2), '% Done with Projections', sep = ''),
    file = 'Projection Analysis Progress.txt',
    append = TRUE,
    sep = ";",
    dec = ".",
    row.names = FALSE,
    col.names = FALSE
  )

  # Pull out results for cell of interest
  temp_results <- dplyr::filter(results, cell == x)
  
  # Generate growth vector representing average growth for each calendar month
  growth_vec <-(avg_m_growth[avg_m_growth$cell == x,])%>%
    slice(1) %>%
    unlist(.,use.names=FALSE)
  growth_vec<-growth_vec[2:13]
  
  # Repeat growth vector for length of simulation
  growth_vec <- as.vector(rep(growth_vec, times = sim_length / 12) )

  # initial counter for time since stocking. Determines mortality
  stock_counter <- 1 
  # initial counter for harvest cycles. Exit loop after completing harvest cycles
  num_harv_cycles <- as.numeric(harv_cycles$harvest_cycles[harv_cycles$cell==x])
  
  # Set sim length to number of harvest cycles times
  cycle_counter <- 1

  # while(cycle_counter < num_harv_cycles) {
  # Loop over simulation months

  for(i in 2:(sim_length + 1)) {
    #break cycle if number of harvest cycles is exceeded.
    if (cycle_counter > num_harv_cycles)
    {break}
    else{
      # initial stocking number
      temp_stocking <-as.numeric(stocking[stocking$cell==x,2]) 
      
      # If individual weight has reached harvest weight (~5 kg), harvest and restock
      if(temp_results$weight[i-1] >= 5) {
        
        # label harvest cycle and calculate harvest
        temp_results$harvest_cycle[i] <- cycle_counter
        temp_results$harvest[i-1] <- temp_results$alive[[i-1]] * temp_results$weight[[i-1]]
        
        # Update harvest cycle counter
        cycle_counter <- cycle_counter + 1
        temp_results$harvest_cycle[i] <- cycle_counter
        
        # If harvest cycle counter is still less than or equal to the number of harvest cycles, restock.
        if(cycle_counter <= num_harv_cycles) {
          temp_results$alive[i] <- temp_stocking
          temp_results$weight[i] <- 0.015
          temp_results$mortality[i] <- 0
          # temp_results$feed[i] <- 0.02 * mean(temp_results$weight[[i]],temp_results$weight[[i-1]]) * mean(temp_results$alive[[i]], temp_results$alive[[i-1]]) * 30
          temp_results$feed[i] <- 0
        }
        
        # Update stock counter
        stock_counter <- 1
        
      } else { 
        
        # Calculate growth
        temp_results$weight[i] <- temp_results$weight[i - 1] + growth_vec[i-1]
        # Subtract mortality
        temp_results$alive[i] <- temp_stocking - temp_stocking * (1-exp(-month_mort*stock_counter))
        temp_results$mortality[i] <- temp_results$alive[i-1]-temp_results$alive[i] #or should this be temp_results$mortality[i-1]?
        
        # Calculate feed use
        temp_results$feed[i] <- 0.02 *  mean(temp_results$weight[[i]],temp_results$weight[[i-1]]) * mean(temp_results$alive[[i]], temp_results$alive[[i-1]]) * 30
        stock_counter <- stock_counter + 1 # advance stocking counter
        temp_results$harvest_cycle[i] <- cycle_counter
        
        if(i == 121 & is.na(temp_results$weight[i]) == F){
          temp_results$harvest[i] <- temp_results$alive[[i]] * temp_results$weight[[i]]
        }
        
        }
      }
      
    # }
      
  }
  
  return(temp_results)   
  
}

# Apply function in parallel
final_results <- mclapply(cell_index, function(x) sim_cell(x),mc.cores=4) %>%
  bind_rows()

#write.csv(final_results,paste0(run_dir,"sim_function_results.csv"))

  return(final_results)
}
