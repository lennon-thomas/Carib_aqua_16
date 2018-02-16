####################################################################################################
## Project: Caribbean aquaculture
## Script purpose: Plots and tables for economic results
## Date: February 15, 2018
## Author: Tyler Clavelle
####################################################################################################

# Load packages and functions -----------------------------------------------------------

library(broom)
library(tidyverse)
library(parallel)
library(pander)
library(ggridges)
library(skimr)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(scales)

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 10),
          axis.text    = element_text(size = 10),
          legend.text  = element_text(size = 10))
}

# Run settings -------------------------------------------------------------

## Set User (lennon/tyler)
user <- 'tyler'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

run_name = 'est_Feb_13'  

# Load run results 
result_folder <- paste0(boxdir,'results/',run_name,"/Results")
figure_folder <- paste0(boxdir,'results/',run_name,"/Figures")

# Extract individual result data sets
carib_supply <- read_csv(file = paste0(result_folder,"/carib_supply.csv"))
eez_supply_df <- read_csv(file = paste0(result_folder,"/eez_supply_df.csv"))
cashflow <- read.csv(file = paste0(result_folder,"/monthly_cashflow.csv")) %>% tbl_df()
npv_df <- read_csv(file = paste0(result_folder,"/npv_df.csv"))
sim_results <- read_csv(file = paste0(result_folder,"/sim_function_results.csv"))
supply_summary <- read_csv(file = paste0(result_folder,"/supply_summary.csv"))

# NPV Summaries -----------------------------------------------------------

# Extract only farms (cells) with positive NPV
pos_npv <- filter(npv_df, npv > 0)

# Summarize NPV by price and discount options
npv_summary <- pos_npv %>% 
  select(prices, discounts, feed_price_index, npv, total_harvest) %>% 
  group_by(prices, discounts, feed_price_index) %>% 
  skim()

# Caribbean NPV summary table
npv_summary_tbl <- npv_summary %>% 
  select(-type, -level, - formatted) %>% 
  filter(stat %in% c('median', 'mean','sd', 'p75', 'p100')) %>% 
  spread(stat, value) %>% 
  arrange(prices, variable, feed_price_index)

# Country level NPV summary
npv_cntry_summary <- pos_npv %>% 
  select(country, prices, discounts, feed_price_index, npv, total_harvest) %>% 
  group_by(country, prices, discounts, feed_price_index) %>% 
  skim()

# Pull out base scenario results from NPV ($8.62 and 0% discount results)
main_npv <- filter(pos_npv, prices == 8.62 & discounts == 0.15 & feed_price_index == 1)

# Country total NPV and farms 
dense_df1 <- main_npv %>%
  group_by(country) %>% 
  mutate(total_npv = sum(npv, na.rm = T),
         farms     = length(unique(cell)),
         npv_cv    = raster::cv(npv, na.rm = T),
         npv_cv    = ifelse(npv_cv > 60, 60, npv_cv)) %>% 
  ungroup() %>% 
  mutate(discounts = factor(discounts),
         carib_npv = median(npv, na.rm = T))

# Filter out first harvest cycle and calculate total costs per cell
cost_summary <- sim_results %>% 
  filter(harvest_cycle == 1) %>% 
  select(cell, month, harvest_cycle) %>% 
  left_join(cashflow) %>% 
  filter(cell %in% main_npv$cell) %>% 
  group_by(eez, cell) %>% 
  summarize(capital_cost = sum(c_costs, na.rm = T),
            labor_cost   = sum(total_monthly_labor, na.rm = T),
            fuel_cost    = sum(mo_fuel_cost, na.rm = T),
            feed_cost    = sum(feed_cost, na.rm = T),
            seed_cost    = sum(fingerling_cost, na.rm = T)) %>% 
  select(-cell)

# Summarize average farm costs per EEZ
eez_cost_summary <- cost_summary %>% 
  group_by(eez) %>% 
  summarize_all(mean, na.rm = T) %>% 
  left_join(npv_df %>% 
              select(eez, country) %>% distinct())

# Summarize average farm costs per EEZ
carib_cost_summary <- cost_summary %>% 
  ungroup() %>% 
  summarize_all(mean, na.rm = T) %>% 
  mutate(country = 'Caribbean')

# NPV Plots ---------------------------------------------------------------

# ridge plot of NPV by EEZ
dense_df1 %>% 
  ggplot(aes(x = npv / 1e6, fill = npv_cv)) +
  geom_density_ridges2(aes(y = fct_reorder(country, npv, fun = median, text = npv_cv)), alpha = 0.8, scale = 1.2) +
  geom_vline(aes(xintercept = carib_npv / 1e6), linetype = 2, color = 'red') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis() +
  labs(title    = 'NPV of cobia aquaculture per farm by Caribbean EEZ',
       subtitle = 'Discount rate = 15%',
       x        = 'Net Present Value ($USD, millions)',
       y        = NULL,
       fill     = paste0('Coefficient of\nvariation')) +
  carib_theme()

ggsave(filename = paste0(figure_folder, '/npv_cntry_ridgeplot.png'), width = 6, height = 5)

# Stacked barplot of average farm cost per eez
eez_cost_summary %>% 
  select(-eez) %>% 
  filter(is.na(country)==F) %>% 
  gather(key = 'category', value = 'cost', 1:5) %>% 
  ggplot(aes(x = country, y = cost, fill = fct_relevel(category, c('seed_cost','feed_cost'), after = Inf))) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), labels = paste0(seq(from = 0, to = 100, by = 10),'%')) +
  scale_fill_viridis(option = 'D', discrete = T) +
  labs(y    = '% of total cost',
       x    = NULL,
       fill = 'Cost category') +
  coord_flip() +
  theme_minimal()

ggsave(filename = paste0(figure_folder, '/cost_cntry_barplot.png'), width = 6, height = 4)

# Density plot of profitable farms by price and discount scenarios
pos_npv %>%  
  filter(feed_price_index == 1 & prices == 8.62) %>% 
  ungroup() %>%
  ggplot(aes(x = npv / 1e6, fill = factor(discounts))) +
  geom_histogram(binwidth = 0.25, position = 'dodge') +
  labs(x = 'NPV (millions)') +
  carib_theme() 

ggsave(filename = paste0(figure_folder, '/npv_discount_scenarios.png'), width = 6, height = 8)

# Caribbean supply curve
# Total Caribbean supply from profitable farms - 10% discount rate
supply_plot_df <- npv_df %>%
  group_by(cell) %>%
  filter(month == max(month) & discounts == 0.15) %>%
  mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e4 / 10)) %>%  # supply = 0 if NPV is negative
  group_by(prices, discounts, feed_price_index) %>%
  summarize(total_supply  = sum(supply, na.rm = T)) %>%  # supply
  ungroup()

# Find supply curve intercepts
intercepts <- supply_plot_df %>% 
  filter(prices == 8.62)

supply_plot_df %>%
  ggplot(aes(x = prices, y = total_supply / 1e6, color = factor(feed_price_index))) +
  geom_line() +
  geom_vline(xintercept = 8.62, linetype = 2, color = 'grey50') +
  geom_segment(aes(x = 0, xend = 8.62,
                   y = total_supply[prices == 8.62 & feed_price_index == 1] / 1e6,
                   yend = total_supply[prices == 8.62 & feed_price_index == 1] / 1e6),
               linetype = 2, color = 'grey50') +
  geom_segment(aes(x = 0, xend = 8.62,
                   y = total_supply[prices == 8.62 & feed_price_index == 0.9] / 1e6,
                   yend = total_supply[prices == 8.62 & feed_price_index == 0.9] / 1e6),
               linetype = 2, color = 'grey50') +
  coord_cartesian(xlim = c(7, 12)) +
  scale_x_continuous(breaks = unique(supply_plot_df$prices), 
                     labels = unique(supply_plot_df$prices)) +
  scale_color_brewer(palette = 'Set1') +
  labs(x     = 'Price ($US/kg)',
       y     = 'Caribbean Supply (MMT)',
       color = 'Feed price\nindex',
       title = 'Supply of cobia in the Caribbean',
       subtitle = 'Discount rate = 15%') +
  carib_theme()

ggsave(filename = paste0(figure_folder,'/carib_supply_curves.png'), width = 5, height = 4)
