# 8/9/17
# This file writes functions for economic model

capital_costs <- function (base_cost,depth_charge,distance_charge){
  
  c_costs<-base_cost * depth_charge * distance_charge
  
  return(c_costs)
  
}