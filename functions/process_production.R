#8/11/17
#This file calculates annual productiona and estimates the number of fingerlings per cell

library(rts)


##determine number left each month by applying monthly mortality to initial number of fingerlings stocked.
ann_number<-function(inital_no=256000,monthly_mort=month_mort){
  
  no<-c(as.vector(rep(NA,120)))
  
  for (i in 1:120){
    
 no[i]<-ifelse(i==1, inital_no-(inital_no*monthly_mort),
    
                  no[i-1]-(no[i-1]*monthly_mort))
    
  }
  return(no)
}




# Function to calculate growth of one individual over time

ann_prod<-function(growth,start_weight=0.15) {                  

total_growth<-growth

no<-c(1:120)

# Calculate cumulative growth in each month
all_growth<-calc(growth,fun = function(x) cumsum(x))

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

plot(init_stock)

