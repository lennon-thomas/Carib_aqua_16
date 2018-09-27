######################################################################
## Function purpose: Calculate average temperature per cell per month
## Date: 09/19/2018
## Author: Tyler Clavelle
######################################################################

heatmaps <- function(avg_results, boxdir, figure_folder, carib_theme) {
  
  # Calculate EEZ wide average temps for suitable farms
  eez_summary <- avg_results %>%
    filter(!is.na(value)) %>%
    group_by(country, month, metric) %>%
    summarize(farm_months   = length(value),
              mean     = mean(value, na.rm = T),
              median   = median(value, na.rm = T),
              sd       = sd(value, na.rm = T)) %>%
    ungroup() %>% 
    mutate(month = fct_relevel(month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")))
  
  # Get overall caribbean avg temp by month for midpoint
  carib_temp_avgs <- avg_results %>%
    filter(metric == 'temperature') %>% 
    summarize(avg_carib_temp = mean(value, na.rm = T),
              median_carib_temp = median(value, na.rm = T))
  
  carib_growth_avgs <- avg_results %>%
    filter(metric == 'growth') %>% 
    summarize(avg_carib_growth = mean(value, na.rm = T),
              median_carib_growth = median(value, na.rm = T))
  
  # Join Caribbean average to plot data
  temp_plot_df <- eez_summary %>%
    ungroup() %>% 
    mutate(month = fct_relevel(month, c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")))
  
  # Plot of average temperatures by EEZ
  temp_plot_df %>% 
    dplyr::select(country, month, metric, median) %>% 
    spread(key = metric, value = median) %>% 
    ggplot(aes(y = fct_reorder(country, temperature),  x = month, fill = temperature)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = unique(carib_temp_avgs$median_carib_temp), low = muted("blue"), high = muted("red")) +
    geom_text(aes(label = round(growth, digits = 2))) +
    labs(y = "Country",
         x = "Month",
         fill = "Median\ntemperature") 
  
  temp_plot_df %>% 
    dplyr::select(country, month, metric, mean) %>% 
    spread(key = metric, value = mean) %>% 
    ggplot(aes(y = fct_reorder(country, growth, .fun = mean),  x = month, fill = growth)) +
    geom_tile() +
    geom_text(aes(label = round(temperature, digits = 1))) +
    scale_fill_gradient2(midpoint = unique(carib_growth_avgs$avg_carib_growth), low = muted("blue"), high = muted("green")) +
    labs(y = "Country",
         x = "Month",
         fill = "Mean\ngrowth\n(kg/m)") +
    carib_theme()
  
  ggsave(paste0(figure_folder,'growth_heatmap_labels.pdf'), width = 8, height = 6)
  
  temp_plot_df %>% 
    dplyr::select(country, month, metric, mean) %>% 
    spread(key = metric, value = mean) %>% 
    ggplot(aes(y = fct_reorder(country, growth, .fun = mean),  x = month, fill = growth)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = unique(carib_growth_avgs$avg_carib_growth), low = muted("blue"), high = muted("green")) +
    labs(y = "Country",
         x = "Month",
         fill = "Mean\ngrowth\n(kg/m)") +
    carib_theme() 
  
  ggsave(paste0(figure_folder,'growth_heatmap.pdf'), width = 180, height = 100, units = 'mm')
  
}
