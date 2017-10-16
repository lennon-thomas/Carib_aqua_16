##########################################################################
##
## Aquaculture simulation function
## Caribbean aquaculture project
##########################################################################

sim_aqua <- function(avg_month_growth, stocking_n, month_mort, int_weight, sim_length) {
  
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
  results <- as_data_frame(matrix(NA, nrow = nrow(stocking) * sim_length, ncol = 6))
  colnames(results) <- c('cell','month','alive','weight','mortality','harvest')
  
  results$month <- rep(1:sim_length, each = nrow(stocking))
  results$cell <- rep(cell_index, times = sim_length)

  # Initialize starting conditions
  results$alive[results$month == 1] <- stocking$stocking
  results$weight[results$month == 1] <- 0.015
  results$mortality[results$month == 1] <- 0
  results$harvest[results$month == 1] <- 0
  
  sim_cell <- function(x) {
  print(x)
    browser()
    temp_results <- filter(results, cell == x)
    growth_vec <- as.vector(avg_m_growth[avg_m_growth$cell == x,])
    growth_vec <- rep(growth_vec, times = sim_length / 12) 
  
    # Loop over simulation months
  for(i in 2:sim_length) {
    
    # If individual weight has reached harvest weight (~5 kg), harvest and restock
    if(temp_results$weight[i-1] >= 5) {
      temp_results$alive[i] <- stocking[x]
      temp_results$weight[i] <- 0.015
      temp_results$mortality[i] <- 0
      temp_results$harvest[i-1] <- temp_results$alive[i-1]
    } else {
      
      # Calculate growth
      temp_results$weight[i] <- temp_results$weight[i - 1] + growth_vec[i]
      # Subtract mortality
      temp_results$mortality[i] <- temp_results$alive[i-1] * month_mort
      temp_results$alive[i] <- temp_results$alive[i-1] - temp_results$mortality[i]
    }
    
  }
 return(temp_results)   
  }
  
  test_apply <- lapply(cell_index[1:100], function(x) sim_cell(x)) %>%
    bind_rows()
  