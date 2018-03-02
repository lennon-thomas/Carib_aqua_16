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

run_name = '2018-02-27_est'  

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
  mutate(total_npv = sum(npv, na.rm = T),
         farms     = length(unique(cell)),
         npv_cv    = raster::cv(npv, na.rm = T),
         npv_cv    = ifelse(npv_cv > 60, 60, npv_cv)) %>% 
  group_by(prices, disc_scenario, feed_price_index) %>% 
  mutate(carib_npv    = median(npv, na.rm = T),
         carib_supply = median(total_harvest, na.rm = T)) %>% 
  ungroup()

# NPV Plots ---------------------------------------------------------------

# boxplot of farm NPV by EEZ
bpA <- dense_df1 %>%
  filter(prices == 8.62 & disc_scenario == 0.1 & feed_price_index == 1) %>% 
  ggplot(aes(x = fct_reorder(country, npv, fun = 'median'), y = npv / 1e6, color = farms)) +
  geom_boxplot() +
  scale_color_gradientn(name = 'Farms', trans = 'log10',
                       breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                       colors = viridis(100)) +
  geom_hline(aes(yintercept = carib_npv / 1e6), linetype = 2, color = 'red') +
  coord_flip() +
  guides(color = F) +
  labs(x = 'Country',
       y = 'NPV ($USD, millions)') +
  carib_theme() 

ggsave(filename = paste0(figure_folder, '/npv_cntry_boxplot.png'), width = 6, height = 8)

# boxplot of farm supply by EEZ
bpB <- dense_df1 %>% 
  filter(prices == 8.62 & disc_scenario == 0.1 & feed_price_index == 1) %>% 
  ggplot(aes(x = fct_reorder(country, npv, fun = 'median'), y = total_harvest / 1e3, color = farms)) +
  geom_boxplot() +
  scale_color_gradientn(name = 'Farms', trans = 'log10', 
                        breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                        colors = viridis(100)) +
  geom_hline(aes(yintercept = carib_supply / 1e3), linetype = 2, color = 'red') +
  coord_flip() +
  labs(x = 'Country',
       y = 'Annual supply (MT)') +
  carib_theme() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggsave(filename = paste0(figure_folder, '/supply_cntry_boxplot.png'), width = 6, height = 8)

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

# Find supply curve intercepts
intercepts <- supply_plot_df %>% 
  filter(prices == 8.62)

supply_plot_df %>%
  filter(total_supply > 0 & disc_scenario == "0.1") %>% 
  ggplot(aes(y = prices, x = total_supply, color = factor(feed_price_index))) +
  geom_line() +
  geom_hline(yintercept = 8.62, linetype = 2, color = 'grey50') +
  geom_segment(aes(y = 0, yend = 8.62,
                   x = total_supply[prices == 8.62 & feed_price_index == 1],
                   xend = total_supply[prices == 8.62 & feed_price_index == 1]),
               linetype = 2, color = 'grey50') +
  geom_segment(aes(y = 0, yend = 8.62,
                   x = total_supply[prices == 8.62 & feed_price_index == 0.9],
                   xend = total_supply[prices == 8.62 & feed_price_index == 0.9]),
               linetype = 2, color = 'grey50') +
  coord_cartesian(ylim = c(5,12)) +
  scale_y_continuous(breaks = unique(supply_plot_df$prices),
                     labels = unique(supply_plot_df$prices)) +
  scale_color_brewer(palette = 'Set1', labels = c('10% reduction', 'Current')) +
  labs(y     = 'Cobia price ($US/kg)',
       x     = 'Caribbean Supply (MT)',
       color = 'Feed price') +
  carib_theme() +
  theme(panel.grid.minor = element_blank())

ggsave(filename = paste0(figure_folder,'/carib_supply_curves.png'), width = 5, height = 4)

supply_plot_df %>%
  filter(total_supply > 0 & feed_price_index == 1) %>% 
  ggplot(aes(y = prices, x = total_supply, color = disc_scenario)) +
  geom_line() +
  geom_hline(yintercept = 8.62, linetype = 2, color = 'grey50') +
  # geom_segment(aes(y = 0, yend = 8.62,
  #                  x = total_supply[prices == 8.62 & feed_price_index == 1],
  #                  xend = total_supply[prices == 8.62 & feed_price_index == 1]),
  #              linetype = 2, color = 'grey50') +
  # geom_segment(aes(y = 0, yend = 8.62,
  #                  x = total_supply[prices == 8.62 & feed_price_index == 0.9],
  #                  xend = total_supply[prices == 8.62 & feed_price_index == 0.9]),
  #              linetype = 2, color = 'grey50') +
  coord_cartesian(ylim = c(5,12)) +
  scale_y_continuous(breaks = unique(supply_plot_df$prices),
                     labels = unique(supply_plot_df$prices)) +
  scale_color_brewer(palette = 'Set1', labels = c('10% discount\nrate','Investment risk')) +
  labs(y     = 'Price ($US/kg)',
       x     = 'Caribbean Supply (MT)',
       color = 'Investment\nscenario') +
  carib_theme() +
  theme(panel.grid.minor = element_blank())

ggsave(filename = paste0(figure_folder,'/carib_supply_curves_invest.png'), width = 5, height = 4)
