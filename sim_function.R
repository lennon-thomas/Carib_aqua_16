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

# Convert rasters to matrixes
cell_index <- Which(!is.na(stocking_n),cells=TRUE)

# Average monthly growth data frame
avg_m_growth <- as_data_frame(matrix(NA, nrow = length(cell_index), ncol = 13))
colnames(avg_m_growth) <- c('cell', c(paste0('month_',1:12)))
avg_m_growth[,2:13] <- avg_month_growth[cell_index]
avg_m_growth$cell <- cell_index

# Initial stocking dataframe
stocking <- data_frame(cell = cell_index,
                       stocking = stocking_n[cell_index])

# Initialize storage data frame for results
results <- as_data_frame(matrix(NA, nrow = nrow(stocking) * sim_length, ncol = 7))
colnames(results) <- c('cell','month','alive','weight','mortality','harvest','feed')

results$month <- rep(1:sim_length, each = nrow(stocking))
results$cell <- rep(cell_index, times = sim_length)

# Initialize starting conditions
results$alive[results$month == 1] <- stocking$stocking
results$weight[results$month == 1] <- 0.015
results$mortality[results$month == 1] <- 0
results$harvest[results$month == 1] <- 0
results$feed[results$month == 1] <- fcr*results$weight[results$month == 1]*  results$alive[results$month == 1]


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
  
  temp_results <- dplyr::filter(results, cell == x)
  growth_vec <-(avg_m_growth[avg_m_growth$cell == x,])%>%
    slice(1) %>%
    unlist(.,use.names=FALSE)
  growth_vec<-growth_vec[2:13]
  growth_vec <- as.vector(rep(growth_vec, times = sim_length / 12) )
  
  # Loop over simulation months
  for(i in 2:sim_length) {
    
    # If individual weight has reached harvest weight (~5 kg), harvest and restock
    if(temp_results$weight[i-1] >= 5) {
      temp_results$alive[i] <- as.numeric(stocking[stocking$cell==x,2])
      temp_results$weight[i] <- 0.015
      temp_results$mortality[i] <- 0
      temp_results$harvest[i-1] <- temp_results$alive[[i-1]]
      temp_results$feed[i] <- fcr*temp_results$weight[[i]]*  temp_results$alive[[i]]
      
    } else {
      
      # Calculate growth
      temp_results$weight[i] <- temp_results$weight[i - 1] + growth_vec[i]
      # Subtract mortality
      temp_results$mortality[i] <- temp_results$alive[[i-1]] * month_mort
      temp_results$alive[i] <- temp_results$alive[[i-1]] - temp_results$mortality[i]
      temp_results$feed[i] <- fcr*growth_vec[i]*  temp_results$alive[[i]]
    }
    
  }
  
  return(temp_results)   
  
}

# Apply function in parallel
final_results <- mclapply(cell_index, function(x) sim_cell(x),mc.cores=4) %>%
  bind_rows()

write.csv(results,paste0(run_dir,"sim_function_results.csv"))

  return(final_results)
}
