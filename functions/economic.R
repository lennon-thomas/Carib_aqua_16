# 8/9/17
# This file writes functions for economic model

### Start-up cost functions
capital_costs <- function (base_cost,vessel,depth_charge,distance_charge,lease,installation){
  
  c_costs<-base_cost + (base_cost * depth_charge) + (base_cost * distance_charge) + installation + (installation*depth_charge) + (installation*distance_charge) + vessel + installation
  
  return(c_costs)
  
}

### Cost of feed
#annual feed costs

feed_cost_est<-function(prod,inital_weight,feed_price,yeardex,fcr){
  

i_feed<-(inital_weight*fcr)*feed_price

mon_feed<-calc(prod,function(x)(x*fcr)*feed_price,filename=paste0(run_dir,"monthly_feed_cost.tif"),overwrite=TRUE)

annual_feed<-stackApply(mon_feed,yeardex,sum,filename=paste0(run_dir,"annual_feed_cost.tif"),overwrite=TRUE)

annual_feed[annual_feed==0]<-NA

total_feed<-annual_feed + i_feed

writeRaster(total_feed,paste0(run_dir,"annual_feed_cost.tif"),overwrite = TRUE)

return(total_feed)

}


### Operating cost function
operating_costs <- function(econ, prod, site_workers, site_hours, avg_boat_spd,
                            fuel_eff, site_days ,no_fingerlings, price_fingerlings, feed_cost, FCR) {
  
  # Labor costs (cost of labor on farm plus hours traveling to/from farm)
  labor <- (econ_stack[['min_wage']] * site_hours * site_workers * site_days) + (econ_stack[['shore_distance']] / avg_boat_spd * site_workers * econ_stack[['min_wage']])
  
  # Travel costs to farm (fuel used times fuel cost times 2 for round trip)
  travel <- econ_stack[['fuel_price']] * (econ_stack[['shore_distance']] / fuel_eff) * site_days * 2
  
  # Feed cost 
#  cost_feed<-((prod*FCR)*feed_price)
  
  # Fingerling cost
  cost_fingerlings<-(no_fingerlings*stock_weight)*price_fingerlings
  
  # Total Operating Costs
  operate <- labor + travel + feed_cost+ cost_fingerlings
  
  # return raster of operating costs per cell
  return(operate)
  
}

### Revenue Function
revenue <- function(x, cobia_price) {
  x <- x * cobia_price
  # Get production values
  # prod_values <- getValues(prod)
  # # Multiply by cobia price
  # revenue_values <- prod_values * cobia_price
  # # Set values to revenue
  # revenue_raster <- setValues(prod, revenue_values)
  # # Return new raster
  return(x)
}
