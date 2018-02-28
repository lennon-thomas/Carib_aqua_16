#8/11/17
#This file calculates annual productiona and estimates the number of fingerlings per cell

library(rts)

# Function to calculate growth of one individual each month (this takes the average growth for)

ann_prod<-function(growth,start_weight=0.15) {                  

# Calculate cumulative growth in each month
all_growth <- raster::calc(growth,fun = function(x) cumsum(x))

# Calculate average number of harvest cycles by dividing cumulative growth in last month by harvest weight (5 kg)
avg_harvest_cycles <- all_growth[[120]] / 5

# Calculate average length (months) of farm cycle
avg_growth_cycle <- 120 / avg_harvest_cycles

# Calculate survival over duration of growth cycle
survival_rate <- (1-month_mort) ^ avg_growth_cycle

# Extract avg growth cycles into matric
avg_harvest <- as.matrix(avg_harvest_cycles)

# number of individuals at harvest for optimal harvest density
final_stock<-(harv_den*total_vol)/harvest_weight 

# number of individuals needed to stock each farm.
init_stock <- final_stock / survival_rate 

return(list(stocking_n = init_stock, harvest_cycles = avg_harvest_cycles, harvest_cycle_length = avg_growth_cycle))

}

#Create layer of average growth by month (will need this for feed calc costs)

avg_growth<- function(growth){

  month_dex<-rep(1:12,10)

  #take average for each month
  monthly_growth<- raster::stackApply(growth, month_dex, fun = mean)

  return(monthly_growth)
}




