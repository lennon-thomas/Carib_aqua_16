######################################################
## Function purpose: Calculate National Supply Curves
## Date: 11/3/2017
## Author: Tyler Clavelle & Lennon Thomas
######################################################

supply_curves <- function(cashflow, prices, discount_rates) {
  
  # Select relevant cashflow data
  supply <- cashflow %>%
    select(cell, eez, month, total_monthly_costs, weight, harvest) %>%
    group_by(cell, eez) %>%
    nest() %>%
    mutate(prices = list(prices))

  return(supply)
}