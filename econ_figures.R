####################################################################################################
## Project: Caribbean aquaculture
## Script purpose: Plots and tables for economic results
## Date: February 15, 2018
## Author: Tyler Clavelle
####################################################################################################

# Load packages and functions -----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(skimr)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(rlang)

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 10),
          axis.text    = element_text(size = 8),
          legend.text  = element_text(size = 8))
}

# Run settings -------------------------------------------------------------

## Set User (lennon/tyler)
user <- 'tyler'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

run_name = '2018-03-15_est'  

# Load run results 
result_folder <- paste0(boxdir,'results/',run_name,"/Results")
figure_folder <- paste0(boxdir,'results/',run_name,"/Figures")

# Extract individual result data sets
cashflow <- read_csv(file = paste0(result_folder,"/monthly_cashflow.csv"))
npv_df <- read_csv(file = paste0(result_folder,"/npv_df.csv"))
sim_results <- read_csv(file = paste0(result_folder,"/sim_function_results.csv"))
supply_summary <- read_csv(file = paste0(result_folder,"/supply_summary.csv"))

# NPV Summaries -----------------------------------------------------------

# Extract only farms (cells) with positive NPV
pos_npv <- filter(npv_df, npv > 0)

# Pull out base scenario results from NPV ($8.62 and 0% discount results)
main_npv <- filter(pos_npv, prices == 8.62 & disc_scenario == 0.1 & feed_price_index == 1)

# Country total NPV and farms 
dense_df1 <- pos_npv %>%
  group_by(country, prices, disc_scenario, feed_price_index) %>% 
  mutate(total_npv     = sum(npv, na.rm = T),
         total_annuity = sum(annuity, na.rm = T),
         farms         = length(unique(cell)),
         npv_cv        = raster::cv(npv, na.rm = T),
         npv_cv        = ifelse(npv_cv > 60, 60, npv_cv),
         annuity_cv    = raster::cv(annuity, na.rm = T),
         annuity_cv    = ifelse(annuity_cv > 60, 60, annuity_cv)) %>% 
  group_by(prices, disc_scenario, feed_price_index) %>% 
  mutate(carib_npv     = median(npv, na.rm = T),
         carib_annuity = median(annuity, na.rm = T),
         carib_supply  = median(total_harvest, na.rm = T)) %>% 
  ungroup()

# Find how many farms are required to match current annual Caribbean production (330,000 MT) and imports (144,000 MT)
# Use only profitable farms and divide total production by 10 for annual estimate
supply_replace <- main_npv %>% 
  ungroup() %>% 
  select(cell, country, total_harvest, study_area_km) %>% 
  arrange(desc(total_harvest)) %>% 
  mutate(harvest_per_yr_mt = total_harvest / 1e3 / 10,
         all_supply = cumsum(harvest_per_yr_mt)) %>% 
  filter(all_supply <= 331000) %>% 
  mutate(replace_domestic = TRUE,
         replace_imports  = ifelse(all_supply <= 144000, TRUE, FALSE)) %>% 
  group_by(country, replace_domestic, replace_imports) %>% 
  summarize(area   = sum(study_area_km, na.rm = T),
            supply = sum(harvest_per_yr_mt, na.rm = T))

# save results of caribbean supply/import replacement 
write_csv(supply_replace, path = paste0(result_folder, '/supply_replace_results.csv'))

# NPV Plots ---------------------------------------------------------------

# boxplot of farm NPV by EEZ
bpA <- dense_df1 %>%
  filter(prices == 8.62 & disc_scenario == 0.1 & feed_price_index == 1) %>% 
  ggplot(aes(x = fct_reorder(country, annuity, fun = 'median'), y = annuity, color = farms)) +
  geom_boxplot() +
  scale_color_gradientn(name = 'Farms', trans = 'log10',
                       breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                       colors = viridis(100)) +
  scale_y_continuous(labels = comma) +
  geom_hline(aes(yintercept = carib_annuity), linetype = 2, color = 'red') +
  coord_flip() +
  guides(color = F) +
  labs(x = 'Country',
       y = 'Annuity ($USD)') +
  carib_theme() 

# ggsave(filename = paste0(figure_folder, '/npv_cntry_boxplot.png'), width = 6, height = 8)

# boxplot of farm supply by EEZ
bpB <- dense_df1 %>% 
  filter(prices == 8.62 & disc_scenario == 0.1 & feed_price_index == 1) %>% 
  ggplot(aes(x = fct_reorder(country, annuity, fun = 'median'), y = total_harvest / 1e3, color = farms)) +
  geom_boxplot() +
  scale_color_gradientn(name = 'Farms', trans = 'log10', 
                        breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                        colors = viridis(100)) +
  geom_hline(aes(yintercept = carib_supply / 1e3), linetype = 2, color = 'red') +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(x = 'Country',
       y = 'Annual supply (MT)') +
  carib_theme() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

# ggsave(filename = paste0(figure_folder, '/supply_cntry_boxplot.png'), width = 6, height = 8)

# Combine plots in grid
boxplot_final <- bpA + bpB

ggsave(boxplot_final, filename = paste0(figure_folder, '/cntry_boxplot.png'), width = 8, height = 10)


# Histogram of profitable farms by price and discount scenarios
pos_npv %>%  
  filter(feed_price_index == 1 & prices == 8.62) %>% 
  ungroup() %>%
  ggplot(aes(x = npv / 1e6, fill = disc_scenario)) +
  geom_histogram(binwidth = 0.5, position = 'dodge') +
  labs(x = 'NPV (millions)') +
  carib_theme() 

ggsave(filename = paste0(figure_folder, '/npv_discount_scenarios.png'), width = 6, height = 8)

# Caribbean supply curve
# Total Caribbean supply from profitable farms - 10% discount rate
supply_plot_df <- npv_df %>%
  group_by(cell) %>%
  filter(month == max(month)) %>%
  mutate(supply = ifelse(npv < 0, 0, total_harvest / 1e3 / 10)) %>%  # supply = 0 if NPV is negative
  group_by(prices, disc_scenario, feed_price_index) %>%
  summarize(total_supply  = sum(supply, na.rm = T)) %>%  # supply
  ungroup()

supply_plot_df <- supply_plot_df %>%
  filter(total_supply > 0 & disc_scenario == "0.1") %>% 
  mutate(scenario = ifelse(feed_price_index == 1, "Current", "10% feed price\nreduction")) %>% 
  bind_rows(supply_plot_df %>%
              filter(total_supply > 0 & disc_scenario == "cntry" & feed_price_index == 1) %>% 
              mutate(scenario = 'Investment risk'))

# Find supply curve intercepts
intercepts <- supply_plot_df %>% 
  filter(prices == 8.62) %>% 
  mutate(total_supply = total_supply / 1e6) # convert to millions of metric tons

# Plot supply curves
supply_plot_df %>%
  ggplot(aes(y = prices, x = total_supply / 1e6, color = scenario)) +
  geom_line() +
  geom_hline(yintercept = 8.62, linetype = 2, color = 'grey50') +
  geom_segment(aes(y = 0, yend = 8.62,
                   x = total_supply[prices == 8.62 & scenario == 'Current'] / 1e6,
                   xend = total_supply[prices == 8.62 & scenario == 'Current'] / 1e6),
               linetype = 2, color = 'grey50') +
  geom_segment(aes(y = 0, yend = 8.62,
                   x = total_supply[prices == 8.62 & scenario == 'Investment risk'] / 1e6,
                   xend = total_supply[prices == 8.62 & scenario == 'Investment risk'] / 1e6),
               linetype = 2, color = 'grey50') +
  geom_segment(aes(y = 0, yend = 8.62,
                   x = total_supply[prices == 8.62 & scenario == '10% feed price\nreduction'] / 1e6,
                   xend = total_supply[prices == 8.62 & scenario == '10% feed price\nreduction'] / 1e6),
               linetype = 2, color = 'grey50') +
  coord_cartesian(ylim = c(min(supply_plot_df$prices),max(supply_plot_df$prices))) +
  scale_y_continuous(breaks = unique(supply_plot_df$prices),
                     labels = unique(supply_plot_df$prices)) +
  scale_color_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C"),
                     labels = c('2a) 10% discount rate, current feed price',
                                '2b) 10% discount rate, reduced feed price',
                                '2c) Country specific discount rate, current feed price')) +
  labs(y     = 'Cobia price ($US/kg)',
       x     = 'Caribbean Supply (MMT)',
       color = NULL) +
  carib_theme() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'vertical')

ggsave(filename = paste0(figure_folder,'/carib_supply_curves.png'), width = 5, height = 6)
