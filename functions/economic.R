# 8/9/17
# This file writes functions for economic model

### Start-up cost functions
capital_costs <- function (base_cost,depth_charge,distance_charge){
  
  c_costs<-base_cost + (base_cost * depth_charge) + (base_cost * distance_charge)
  
  return(c_costs)
  
}

### Operating cost function
operating_costs <- function(econ, prod, site_workers = 2, site_hours = 8, avg_boat_spd = 8,
                            fuel_eff = 1, no_trips = 40, no_fingerlings = 1500, cost_fingerlings = 1500) {
  
  # Labor costs (cost of labor on farm plus hours traveling to/from farm)
  labor <- (econ_stack[['min_wage']] * site_hours * site_workers * no_trips) + (econ_stack[['shore_distance']] / avg_boat_spd * site_workers * econ_stack[['min_wage']])
  
  # Travel costs to farm (fuel used times fuel cost times 2 for round trip)
  travel <- econ_stack[['fuel_price']] * (econ_stack[['shore_distance']] / fuel_eff) * no_trips * 2
  
  # Feed cost (INSERT)

  
  # Total Operating Costs
  operate <- labor + travel + cost_fingerlings
  
  # return raster of operating costs per cell
  return(operate)
  
}

