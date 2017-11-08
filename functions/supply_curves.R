######################################################
## Function purpose: Calculate National Supply Curves
## Date: 11/3/2017
## Author: Tyler Clavelle & Lennon Thomas
######################################################

supply_curves <- function(cashflow, prices = c(1:10), discount_rates = seq(0.02, 0.2, by = 0.02)) {
  
  # Calculate discounted profits for a range of prices and discount rates
  supply <- cashflow %>%
    mutate(harvest_weight = weight * harvest) %>%
    select(cell, eez, month, harvest_weight) %>%
    filter(harvest_weight > 0) %>%
    mutate(prices    = list(prices),
           profit    = map2(harvest_weight, prices, `*`)) %>%
    unnest() %>%
    mutate(discounts = list(discount_rates),
           disc_profit = pmap(list(month, profit, discounts), 
                              function(month, profit, discounts) profit / (1 + discounts) ^ month)) %>% 
    unnest()
  
  # Calculate discounted costs for a range of discount rates
  supply_costs <- cashflow %>%
    select(cell, eez, month, total_monthly_costs) %>%
    mutate(discounts = list(discount_rates),
           disc_costs = pmap(list(month, total_monthly_costs, discounts), 
                              function(month, total_monthly_costs, discounts) total_monthly_costs / (1 + discounts) ^ month)) %>% 
    unnest()
  
  

  return(supply)
}