######################################################
## Function purpose: Calculate National Supply Curves
## Date: 11/3/2017
## Author: Tyler Clavelle & Lennon Thomas
######################################################

supply_curves <- function(cashflow,
                          cobia_price,
                          prices = c(5:15),
                          feed_price_index = c(1, 0.75),
                          discount_rates = seq(0, 0.15, by = 0.05),
                          eezs,
                          result_folder,
                          figure_folder) {
  
  # insert current price into vector of prices
  prices <- c(prices, cobia_price)   
  
  # Add discount rates by joining cashflow with eezs
  cashflow <- cashflow %>% 
    left_join(eezs %>% 
                dplyr::select(eez, disc_rate) %>% 
                mutate(disc_scenario = 'cntry')) 
   
  # Calculate discounted profits for a range of prices and the baseline (country specific) discount rate
  supply <- cashflow %>%
    dplyr::select(cell, eez, month, harvest, disc_rate, disc_scenario) %>%
    filter(harvest > 0) %>%
    mutate(prices       = list(prices),
           profit       = map2(harvest, prices, `*`)) %>%
    unnest() %>% 
    group_by(eez) %>% 
    mutate(disc_rate = list(c(unique(disc_rate), discount_rates)),
           disc_scenario = list(c('cntry', as.character(discount_rates))),
           disc_profit = pmap(list(month, profit, disc_rate), # map discounting function across discount rates
                              function(month, profit, disc_rate) profit / (1 + disc_rate) ^ (month * 1/12))) %>%
    unnest()
   
  # Calculate discounted costs for a range of feed costs and discount rates
  supply_costs <- cashflow %>%
    mutate(feed_price_index = list(feed_price_index)) %>% 
    unnest() %>% 
    mutate(feed_cost           = feed_cost * feed_price_index,
           total_monthly_costs = total_monthly_labor + mo_fuel_cost + feed_cost + fingerling_cost) %>% 
    dplyr::select(cell, eez, month, feed_price_index, feed_cost, total_monthly_costs, disc_rate) %>%
    group_by(eez) %>% 
    mutate(disc_rate = list(c(unique(disc_rate), discount_rates)),
           disc_scenario = list(c('cntry', as.character(discount_rates))),
           disc_costs = pmap(list(month, total_monthly_costs, disc_rate), 
                              function(month, total_monthly_costs, disc_rate) total_monthly_costs / (1 + disc_rate) ^ (month * 1/12))) %>%
    unnest() %>%
    group_by(cell, disc_scenario, feed_price_index) %>%
    mutate(total_disc_costs = cumsum(disc_costs)) # take cumulative sum of discounted costs
  
  # Calculate median percent monthly feed cost by cell
  feed_cost_percs <- supply_costs %>% 
    ungroup() %>% 
    dplyr::select(cell, month, feed_cost, total_monthly_costs) %>% 
    group_by(cell) %>% 
    summarize(feed_cost_perc = sum(feed_cost, na.rm = T) / sum(total_monthly_costs, na.rm = T))
  
  # Find cells with zero costs ( !! find out why this is happeneing !!  ) - fixed now
  no_costs <- filter(supply_costs, total_disc_costs == 0) 
  
  # Join discounted costs with discounted profits before summing NPV
  cashflow_disc <- supply %>%
    left_join(supply_costs) %>%
    filter(!(cell %in% unique(no_costs$cell))) %>% # remove any cells missing costs
    dplyr::select(-total_monthly_costs, -profit, -disc_costs) %>%
    mutate(disc_profit = ifelse(is.na(disc_profit), 0, disc_profit))
  
  # Convert eez data to tibble for left join
  eez_df <- dplyr::select(eezs, eez, country) %>%  tbl_df()
  eez_df <- eez_df %>%
    mutate(eez = as.numeric(levels(eez)[eez]),
           country = as.character(country))


# NPV Calculations --------------------------------------------------------

  # Calculate present value as discounted profits minus discounted costs
  npv_df <- cashflow_disc %>%
    group_by(cell, disc_scenario, prices, feed_price_index) %>%
    mutate(total_disc_profit = cumsum(disc_profit),
           total_harvest     = sum(harvest, na.rm = T),
           npv               = total_disc_profit - total_disc_costs,
           eez               = as.numeric(eez)) %>%
    left_join(eez_df) %>% 
    left_join(feed_cost_percs)
  
  ### Only profitable farms ###  
  
  # Total Caribbean supply from profitable farms 
  carib_supply <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e3), # supply = 0 if NPV is negative, otherwise convert from kg to MT
           npv    = ifelse(npv < 0, 0, npv)) %>% # npv set to 0 for non profitable farms
    filter(npv > 0) %>%
    group_by(prices, disc_scenario, feed_price_index) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              top95_supply  = quantile(supply, probs = 0.95, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              top95_npv     = quantile(npv, probs = 0.95, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) %>%  # npv from all farms 
    mutate(supply_scenario = 'Profitable farms')  
  
  # Country level supply
  eez_supply <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e3), # supply = 0 if NPV is negative, otherwise convert from kg to MT
           npv    = ifelse(npv < 0, 0, npv)) %>% # npv set to 0 for non profitable farms
    group_by(country, prices, disc_scenario, feed_price_index) %>%
    filter(npv > 0) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              top95_supply  = quantile(supply, probs = 0.95, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              top95_npv     = quantile(npv, probs = 0.95, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) %>%  # npv from all farms
    mutate(supply_scenario = 'Profitable farms')
  
  ### All suitable farms ###  
  # Total Caribbean supply from profitable farms 
  carib_supply_allsuit <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = total_harvest / 1e3) %>% # supply converted from kg to MT
    group_by(prices, disc_scenario, feed_price_index) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              top95_supply  = quantile(supply, probs = 0.95, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              top95_npv     = quantile(npv, probs = 0.95, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) %>%  # npv from all farms
    mutate(supply_scenario = 'All farms') 
  
  # Country level supply
  eez_supply_all_suit <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e3)) %>% # supply converted from kg to MT
    group_by(country, prices, disc_scenario, feed_price_index) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              top95_supply  = quantile(supply, probs = 0.95, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              top95_npv     = quantile(npv, probs = 0.95, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) %>%  # npv from all farms
    mutate(supply_scenario = 'All farms')
  
  # Summary Table
  supply_summary_tbl <- eez_supply %>%
    bind_rows(eez_supply_all_suit) %>% 
    filter(prices == cobia_price) %>%
    bind_rows(carib_supply %>%
                filter(prices == cobia_price) %>%
                mutate(country = 'Caribbean')) %>% 
    bind_rows(carib_supply_allsuit %>%
                filter(prices == cobia_price) %>%
                mutate(country = 'Caribbean'))
  
  # Save summary table to results
  write_csv(supply_summary_tbl, path = paste0(result_folder, 'supply_summary.csv'))
  
  return(list('carib_supply' = carib_supply, 'eez_supply' = eez_supply, 'npv' = npv_df))
}