# 10/18/17
# This file writes functions for economic model

# Calculate monthly and total costs of feed for each farm assuming a constant fcr and different stocking_n

monthly_cost_est<-function (sim_results,econ_stack,stocking_n,site_lease,no_cage,labor_installation,support_vessel,site_hours,site_workers,avg_boat_spd,feed_price,price_fingerlings,cage_cost,site_days){
  
  #Rename econstack layers- can't figure out how to preserve layer names when saving raster stack, so just reassing them here  
  names(econ_stack)<-c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez","cell_no")
  
  #Turn economic parameters into data frame
  all_economic<-as.data.frame(econ_stack)
  
  #subset only values for suitable cells
  s_cells<-unique(sim_results$cell)
  
 # make sure it is actually taking the right cells here
  
  suitable_economic<- all_economic %>%
    filter(cell_no %in% s_cells)
  
  # Calculate capital costs
  
  base_cost<-sum(cage_cost*no_cage)
  
  suitable_economic<-suitable_economic %>%
    mutate(c_costs = (base_cost + (base_cost * depth_charge) + labor_installation + (labor_installation*depth_charge) + support_vessel + site_lease)) 
  
  # Calculate monthly labor costs (same for all months)
  suitable_economic<-suitable_economic %>%
    mutate(total_monthly_labor = min_wage * site_hours * site_workers)

  
  # Calculate monthly fuel costs (same for all months) for (2 trips every site day for 4 vessels)
  
  suitable_economic<-suitable_economic %>%
    mutate(mo_fuel_cost = fuel_price * (shore_distance/fuel_eff) * site_days * 2 *4) 

  
  
  #convert into monthly costs                     
  monthly_costs<-left_join(sim_results,suitable_economic,by=c('cell'='cell_no'))  %>%
    select(cell,month,alive,weight,mortality,harvest,feed,c_costs,total_monthly_labor,mo_fuel_cost,eez)                            
 
   # Match country names
  eez_shape<-readOGR(dsn = paste(boxdir,"Suitability/tmp/",sep=""),layer = "carib_eez_shape") 
  
  country<- eez_shape %>%
    as_data_frame %>%
    select(MRGID,Territory1)
  
  monthly_costs$eez<-as.factor(monthly_costs$eez)
  
  monthly_costs<-left_join(monthly_costs,country,by = c('eez' = 'MRGID'))
  #set capital costs to 0 for all month except month one
  
  monthly_costs$c_costs<-ifelse(monthly_costs$month==1,monthly_costs$c_costs,0)                                
  
  # Calculate all cost components and sum everything to get total monthly costs, revenues and profits
  monthly_costs$harvest[is.na(monthly_costs$harvest)]<-0
  
  monthly_cashflow<-monthly_costs %>%
    mutate(feed_cost = feed * feed_price,
           fingerling_cost = ifelse(weight==0.015,alive*price_fingerlings,
                                    0),
           total_operating_cost = total_monthly_labor + mo_fuel_cost + feed_cost + fingerling_cost,
           total_monthly_costs = total_operating_cost + c_costs,
           monthly_revenue = harvest * cobia_price,
           cash_flow = monthly_revenue - total_operating_cost, # I think this should be 'total_monthly costs' instead
           year=ifelse(month <= 12,1,                       #probably don't need this.
                       ifelse(month > 12 & month <= 24, 2,
                              ifelse( month >24 & month <= 36, 3,
                                      ifelse( month > 26 & month <= 48, 4,
                                              ifelse( month > 48 & month <= 60, 5,
                                                      ifelse( month > 60 & month <= 72, 6,
                                                              ifelse( month > 72 & month <= 84, 7,
                                                                      ifelse( month > 84 & month <= 96, 8,
                                                                              ifelse( month > 96 & month <= 108, 9,
                                                                                      10))))))))))#,
       #    present_value = cash_flow/(1+(discount_rate/12))^(month))  # check with tyler to make sure this is correct
  
  
  
# write.csv(monthly_cashflow,paste0(run_dir,"monthly_cashflow.csv"))
  
  # Calculate annual NPV- no bc harvest cycle isn't annual
  # 
  # monthly_PV<-monthly_costs %>%
  #   group_by(cell) %>%
  #   summarise(total_operating_cost = sum(total_operating_cost),
  #          total_revenue = sum(monthly_revenue),
  #          total_cap_cost = sum(c_costs)) %>%
  #   mutate (annual_cash_flow = total_revenue - total_operating_cost,
  #          cml_cash_flow = cumsum(annual_cash_flow),
  #          present_value = cml_cash_flow/(1+discount_rate)^year) %>%
  #          ungroup()
  # 
  # write.csv(annual_PV,paste0(run_dir,"annual_PV.csv")) 
  
  return(monthly_cashflow)
}



total_cost_raster<-function(monthly_costs,stocking_n)  {
  
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

 