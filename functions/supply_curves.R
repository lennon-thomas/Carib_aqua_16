######################################################
## Function purpose: Calculate National Supply Curves
## Date: 11/3/2017
## Author: Tyler Clavelle & Lennon Thomas
######################################################

supply_curves <- function(cashflow,
                          cobia_price,
                          prices = c(5:15), 
                          discount_rates = seq(0, 0.15, by = 0.05),
                          eezs,
                          result_folder,
                          figure_folder) {
  
  # insert current price into vector of prices
  prices <- c(prices, cobia_price)
  
  # Calculate discounted profits for a range of prices and discount rates
  supply <- cashflow %>%
    select(cell, eez, month, harvest) %>%
    filter(harvest > 0) %>%
    mutate(prices       = list(prices),
           profit       = map2(harvest, prices, `*`)) %>%
    unnest() %>%
    mutate(discounts   = list(discount_rates),
           disc_profit = pmap(list(month, profit, discounts), 
                              function(month, total_monthly_costs, discounts) total_monthly_costs / (1 + discounts) ^ (month * 1/12))) %>%
    unnest()
  
  # Calculate discounted costs for a range of discount rates
  supply_costs <- cashflow %>%
    select(cell, eez, month, total_monthly_costs) %>%
    mutate(discounts  = list(discount_rates),
           disc_costs = pmap(list(month, total_monthly_costs, discounts), 
                              function(month, total_monthly_costs, discounts) total_monthly_costs / (1 + discounts) ^ (month * 1/12))) %>%
    unnest() %>%
    group_by(cell, discounts) %>%
    mutate(total_disc_costs = cumsum(disc_costs)) # take cumulative sum of discounted costs
  
  # Find cells with zero costs ( !! find out why this is happeneing !! )
  no_costs <- filter(supply_costs, total_disc_costs == 0)
  
  # Join discounted costs with discounted profits before summing NPV
  cashflow_disc <- supply %>%
    left_join(supply_costs) %>%
    filter(!(cell %in% unique(no_costs$cell))) %>% # remove any cells missing costs
    select(-total_monthly_costs, -profit, -disc_costs) %>%
    mutate(disc_profit = ifelse(is.na(disc_profit), 0, disc_profit))
  
  # Convert eez data to tibble for left join
  colnames(eezs) <- c('eez','country','coords')
  eezs <- as_data_frame(eezs)
  eezs <- eezs %>%
    select(eez, country) %>%
    mutate(eez = as.numeric(levels(eez)[eez]),
           country = as.character(country))
  
  # Calculate present value as discounted profits minus discounted costs
  npv_df <- cashflow_disc %>%
    group_by(cell, discounts, prices) %>%
    mutate(total_disc_profit = cumsum(disc_profit),
           total_harvest = sum(harvest, na.rm = T),
           npv = total_disc_profit - total_disc_costs) %>%
    left_join(eezs)

  # Total Caribbean supply 
  carib_supply <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e4), # supply = 0 if NPV is negative
           npv    = ifelse(npv < 0, 0, npv)) %>% # npv set to 0 for non profitable farms
    filter(npv > 0) %>%
    group_by(prices, discounts) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) # npv from all farms 
  
  # Country level supply
  eez_supply <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month)) %>%
    mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e4), # supply = 0 if NPV is negative
           npv    = ifelse(npv < 0, 0, npv)) %>% # npv set to 0 for non profitable farms
    group_by(country, prices, discounts) %>%
    filter(supply > 0) %>%
    summarize(total_supply  = sum(supply, na.rm = T), # supply only from profitable farms
              min_supply    = min(supply, na.rm = T),
              max_supply    = max(supply, na.rm = T),
              median_supply = median(supply, na.rm = T),
              var_supply    = var(supply, na.rm = T),
              total_npv     = sum(npv, na.rm = T),
              min_npv       = min(npv, na.rm = T),
              max_npv       = max(npv, na.rm = T),
              median_npv    = median(npv, na.rm = T),
              var_npv       = var(npv, na.rm = T),
              farms         = n_distinct(cell)) # npv from all farms 
  
  # Summary Table
  supply_summary_tbl <- eez_supply %>%
    filter(prices == cobia_price) %>%
    bind_rows(carib_supply %>%
                filter(prices == cobia_price) %>%
                mutate(country = 'Caribbean'))
  
  # Save summary table to results
  write_csv(supply_summary_tbl, path = paste0(result_folder, 'supply_summary.csv'))
  
  ### Plots
  
  # Caribbean supply curve
  carib_supply %>%
    ggplot(aes(x = prices, y = total_supply / 1e6, color = factor(discounts))) +
    geom_line() +
    labs(x = 'Price ($US/kg)',
         y = 'Caribbean Supply (MT)',
         color = 'Discount Rate') +
    theme_bw()
    
  ggsave(filename = paste0(figure_folder,'carib_supply_curves.png'), width = 6, height = 5)
  
  # Country supply curves
  eez_supply %>%
    ggplot(aes(x = prices, y = total_supply / 1e6, color = factor(discounts))) +
    geom_line() +
    labs(x = 'Price ($US/kg)',
         y = '10-yr EEZ Supply (MMT)',
         color = 'Discount Rate') +
    theme_bw() +
    facet_wrap(~country) +
    theme(legend.position = 'bottom')
  
  ggsave(filename = paste0(figure_folder,'eez_supply_curves.png'), width = 12, height = 12)
  
  return(list('carib_supply' = carib_supply, 'eez_supply' = eez_supply, 'npv' = npv_df))
}