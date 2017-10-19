# 10/18/17
# This file writes functions for economic model

# Calculate monthly and total costs of feed for each farm assuming a constant fcr and different stocking_n

monthly_cost_est<-function (sim_results,econ_stack,feed_cost,stocking_n,site_lease,no_cages,labor_installation,support_vessel,site_hours,site_workers,site_days,avg_boat_speed,feed_price,price_fingerlings,cage_cost){
  
  
#Rename econstack layers- can't figure out how to preserve layer names when saving raster stack, so just reassing them here  
  names(econ_stack)<-c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez")
  
#Turn economic parameters into data frame
  all_economic<-as.data.frame(econ_stack)
  
#subset only values for suitable cells
  s_cells<-unique(sim_results$cell)
  
  suitable_economic<-all_economic[s_cells,] # make sure it is actually taking the right cells here
   suitable_economic<-cbind(s_cells,suitable_economic)
# Calculate capital costs

 base_cost<-sum(cage_cost)
 
 suitable_economic<-suitable_economic %>%
  mutate(c_costs = (base_cost + (base_cost * depth_charge) + (base_cost * distance_charge) + labor_installation + (labor_installation*depth_charge) + (labor_installation*distance_charge) + support_vessel + site_lease))

# Calculate monthly labor costs (same for all months)
  suitable_economic<-suitable_economic %>%
    mutate(site_labor = min_wage * site_hours * site_workers * site_days,
           travel_labor = 2*(shore_distance/avg_boat_spd * site_workers* min_wage),
           total_monthly_labor = site_labor + travel_labor)
         #  cell = as.numeric(row.names(suitable_economic)))
  
 #  monthly_costs<-left_join(monthly_costs,labor,by='cell')
  
#C alculate monthly fuel costs (same for all months)
   
   suitable_economic<-suitable_economic %>%
     mutate(mo_fuel_cost = fuel_price * (shore_distance/fuel_eff) * site_days * 2)
           # cell = as.numeric(row.names(suitable_economic)))
   
  

       #convert into monthly costs                     
 monthly_costs<-left_join(sim_results,suitable_economic,by=c('cell'='s_cells'))  %>%
                              select(cell,month,alive,weight,mortality,harvest,feed,eez,c_costs,site_labor,travel_labor,total_monthly_labor,mo_fuel_cost)                            
                            
 
 
 #set capital costs to 0 for all month except month one
 
 monthly_costs$c_costs<-ifelse(monthly_costs$month>1,0,monthly_costs$c_costs)                                

 # Calculate all cost components
  monthly_costs<-monthly_costs %>%
    mutate(feed_cost = feed * feed_price,
           fingerling_cost = ifelse(weight==0.015,alive*weight*price_fingerlings,
                             0)) 
       
  #sum everything to get total monthly costs, revenues and profits
 monthly_costs<-monthly_costs %>%
   mutate(total_operating_cost = total_monthly_labor + mo_fuel_cost + feed_cost + fingerling_cost)

monthly_costs<-monthly_costs %>%
   mutate (total_monthly_cost = total_operating_cost + c_costs,
           monthly_profit = harvest * cobia_price,
           unadj_reveune= monthly_profit - total_monthly_cost,na.rm=TRUE)

monthly_costs[is.na(monthly_costs$monthly_profit)]<-0

write.csv(monthly_costs,paste0(run_dir,"monthly_costs.csv"),overwrite = TRUE)

return(monthly_costs)
}

total_cost_raster<-(monthly_costs,stocking_n) function {
  
  total_cost_stack<-stocking_n
  
  total_costs<-monthly_costs %>%
    group_by(cell) %>%
    summarise(total_capital = sum(c_costs),
              total_labor = sum(total_monthly_labor),
              total_fuel = sum(mo_fuel_cost),
              total_feed = sum(feed_cost),
              total_fingerling = sum(fingerling_cost),
              total_costs = total_fingerling + total_feed + total_fuel + total_labor + total_capital,
              total_profit = sum(monthly_profit,na.rm=TRUE)) %>%
    mutate(unadju_revenue = total_profit - total_costs,na.rm=TRUE) %>%
    ungroup ()
  
  s_cells<-unique(monthly_costs$cell)
  
  capital_raster<-stocking_n
  fuel_raster<-stocking_n
  feed_raster<-stocking_n
  fingerling_raster<-stocking_n
  total_cost_raster<-stocking_n
  total_profit_raster<-stocking_n
  total_unadj_raster<-stocking_n
  
  capital_raster[total_costs$cell]<-total_costs$total_capital
  labor_raster[total_costs$cell]<-total_costs$total_labor
  fuel_raster[total_costs$cell]<-total_costs$total_fuel
  feed_raster[total_costs$cell]<-total_costs$total_feed
  fingerling_raster[total_costs$cell]<-total_costs$total_fingerling
  total_cost_raster[total_costs$cell]<-total_costs$total_costs
  total_profit_raster[total_costs$cell]<-total_costs$total_profit
  total_unadj_raster[total_costs$cell]<-total_costs$unadju_revenue
  
  econ_results_raster<-stack(capital_raster,fuel_raster,feed_raster,fingerling_raster,total_cost_raster,total_profit_raster,total_unadj_raster)
  
  writeRaster(econ_results_raster,paste(rundir,'economic/data/final/econ_results_raster.nc',sep = ""), format = "CDF",varname="econ", overwrite =TRUE)
  
  
}

 total_feed_cost<-monthly_costs %>%
 group_by(cell) %>%
 summarise(total_feed_cost = sum(feed_cost)) %>%
 ungroup()

 total_feed_cost_raster<-stocking_n
 total_feed_cost_raster[total_feed_cost$cell]<-total_feed_cost$total_feed_cost

 writeRaster(total_feed_cost_raster,paste0(run_dir,"total_feed_cost.tif"))

 return(monthly_costs)

}


#Calculate monthly and total costs of fingerlings used to stock each farm

fingerling_cost<-function(monthly_costs,price_fingerlings,stocking_n){
  
 monthly_costs<- monthly_costs %>%
  mutate(fingerling_cost = ifelse(weight==0.015,alive*weight*price_fingerlings,
                                    0)
           )
  
 write.csv(monthly_costs,paste0(run_dir,"monthly_costs.csv"))
 
 total_fingerling_cost<-monthly_costs %>%
  group_by(cell)%>%
  summarize(total_fingerling_cost = sum(fingerling_cost)) %>%
  ungroup()
   
 total_fingerling_cost_raster<-stocking_n
  
 total_fingerling_cost_raster[total_fingerling_cost$cell]<-total_fingerling_cost$total_fingerling_cost
   
 writeRaster(total_fingerling_cost_raster,paste0(run_dir,"total_fingerling_cost.tif"),overwrite = TRUE)
   
 return(monthly_costs)
  
}


##Calculate monthly labor costs

labor_cost<-function (econ_stack,site_workers,site_hours,avg_boat_spd,site_days){
  
 
 #calculate monthly labor costs
 
 
 total_labor_costs<-monthly_costs %>%
    group_by(cell) %>%
    summarise(total_labor=sum(monthly_labor)) %>%
    ungroup()
 
 
 labor_cost_raster<-stocking_n

 labor_cost_raster[total_labor_costs$cell]<-total_labor_costs$total_labor
 
 write.csv(monthly_costs, paste0(run_dir,"monthly_costs.csv")
 
 return(monthly_costs)

}

# Calculate monthly fuel costs

fuel_cost<-(econ_stack,suitable_economic,site_hours,shore_distance,fuel_eff, site_days)function{
  
 
  
}


# Calculate total monthly operating costs

operating_costs<-(monthly_costs){
  
  monthly_costs<-monthly_costs %>%
    mutate(operate_costs = sum(feed_cost,fingerling_cost,monthly_labor,mo_fuel_cost)) %>%
    write.csv(paste0(run_dir,"monthly_costs.csv"))

}

# Calculate capital costs and divide over months

capital_costs <- function (base_cost,vessel,depth_charge,distance_charge,lease,installation){
  
  c_costs<-(base_cost + (base_cost * depth_charge) + (base_cost * distance_charge) + installation + (installation*depth_charge) + (installation*distance_charge) + vessel + installation)
  
  c_cost_data<-as.data.frame(c_costs) %>%
    mutate(cell=as.numeric(row.names(c_costs)))
  
  s_cell<-unique(monthly_costs$cell)
  
  c_cost_data<-c_cost_data[s_cells]
  
  monthly_costs<-left_join(monthly_costs,c_cost_data,by = 'cell') %>%
    mutate(total_costs = operate_cost + c_cost_data)
  
  all_total_cost<-monthly_costs %>%
    group_by(cell) %>%
    summarise(total_c_cost= sum(c_cost_data),
              all_costs = sum(total_costs)) %>%
    ungroup()
  
  c_cost_raster<-stocking_n
  
  c_cost_raster[s_cell]<-all_total_cost$total_c_cost
  
  all_cost_raster<-stocking_n
  
  all_cost_raster[s_cell]<-all_total_cost$all_costs
  
  writeRaster(c_cost_raster,paste0(rundir,"capital_costs.tif"))
  
  writeRaster(all_cost_raster,paste0(rundir,"all_costs.tif"))
  
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
