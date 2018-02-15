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
          title        = element_text(size = 12),
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
file.names <- list.files(path = result_folder, pattern = ".csv")
result_files <- lapply(paste0(result_folder,"/",file.names),read.csv, stringsAsFactors = F)

# Extract individual result data sets
carib_supply <- result_files[[1]]
eez_supply_df <- result_files[[2]]
cashflow <- result_files[[3]]
npv_df <- result_files[[4]]
sim_results <- result_files[[5]]
supply_summary<-result_files[[6]]

# NPV Summaries -----------------------------------------------------------

# Extract only farms (cells) with positive NPV
pos_npv <- filter(npv_df, npv > 0)

# Summarize NPV by price and discount options
npv_summary <- pos_npv %>% 
  select(prices, discounts, npv, total_harvest) %>% 
  group_by(prices, discounts) %>% 
  skim()

# Caribbean NPV summary table
npv_summary_tbl <- npv_summary %>% 
  select(-type, -level, - formatted) %>% 
  filter(stat %in% c('median', 'mean','sd', 'p75', 'p100')) %>% 
  spread(stat, value) %>% 
  arrange(prices, variable)

# Country level NPV summary
npv_cntry_summary <- pos_npv %>% 
  select(country, prices, discounts, npv, total_harvest) %>% 
  group_by(country, prices, discounts) %>% 
  skim()

npv_cntry_plot_df <- npv_cntry_summary %>% 
  ungroup() %>% 
  filter(prices == 8.62 & discounts == 0) %>% 
  filter(stat %in% c('n', 'mean')) %>% 
  select(country, variable, stat, value) %>% 
  spread(variable, value) %>% 
  group_by(country) %>% 
  mutate(farms = npv[stat == 'n']) %>% 
  filter(stat == 'mean')

# Pull out base scenario results from NPV ($8.62 and 0% discount results)
main_npv <- filter(pos_npv, prices == 8.62 & discounts == 0)

# Country total NPV and farms 
dense_df1 <- main_npv %>%
  group_by(country) %>% 
  mutate(total_npv = sum(npv, na.rm = T),
         farms     = length(unique(cell))) %>% 
  ungroup() %>% 
  mutate(discounts = factor(discounts))

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
  ggplot(aes(x = npv, fill = log10(total_npv))) +
  geom_density_ridges2(aes(y = fct_reorder(country, npv, fun = median)), alpha = 0.8) +
  scale_x_continuous(limits = c(0,NA), expand = c(0,0)) +
  scale_fill_viridis() +
  labs(title = 'NPV of cobia aquaculture per farm by Caribbean EEZ',
       x     = 'Net Present Value (NPV)',
       y     = NULL,
       fill  = paste0('Total country\nNPV (log)')) +
  carib_theme()

ggsave(filename = paste0(figure_folder, '/npv_cntry_ridgeplot.png'), width = 8, height = 5)

# dotplot of NPV against total harvest
ggplot(npv_cntry_plot_df, aes(x = total_harvest, y = npv)) +
  geom_point(aes(size = farms)) +
  geom_text_repel(aes(label = country)) +
  scale_size_continuous(breaks = seq(from = 1000, to = 9000, by = 1000)) +
  scale_x_continuous(labels = comma) +
  labs(x = 'Average farm harvest (kg)',
       y = 'Average farm net present value (NPV)',
       size = "Number of farms") +
  carib_theme()

ggsave(filename = paste0(figure_folder, '/npv_harvest_dotplot.png'), width = 8, height = 5)

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
  ggplot(aes(x = npv / 1e6)) +
  geom_density() +
  facet_grid(prices ~ discounts, scales = 'free_y') +
  labs(x = 'NPV (millions)') +
  carib_theme() +
  theme(axis.text.y = element_blank())

ggsave(filename = paste0(figure_folder, '/npv_price_discount_scenarios.png'), width = 6, height = 8)
