####################################################################################################
## Project: Caribbean aquaculture
## Script purpose: Function to make plots and tables for economic results
## Date: February 15, 2018
## Author: Tyler Clavelle
####################################################################################################

econ_figures <- function(npv_df,
                      cashflow,
                      sim_results,
                      supply_summary,
                      carib_theme,
                      result_folder,
                      figure_folder,
                      countries) {

# NPV Summaries -----------------------------------------------------------

# Extract only farms (cells) with positive NPV
# pos_npv <- filter(npv_df, npv > 0) 
 
# Pull out base scenario results from NPV ($8.62 and 0% discount results)
npv_df$eez<-as.factor(npv_df$eez)

main_npv <- filter(npv_df, prices == 8.62 & disc_scenario == "0.1" & feed_price_index == 1)

cntr_risk<-countries %>%
  dplyr::select(country,risk_score,disc_rate)

cntry_disc_npv <- filter(npv_df, prices == 8.62 & disc_scenario == 'cntry' & feed_price_index == 1) %>%
  left_join(cntr_risk, by='country') 


# Find how many farms are required to match current annual Caribbean production (330,000 MT) and imports (144,000 MT)
# Use only profitable farms and divide total production by 10 for annual estimate
supply_replace <- main_npv %>% 
  ungroup() %>% 
  dplyr::select(cell, country, total_harvest, study_area_km) %>% 
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



# Scatterplot risk vs. prod------------------------------------------------------------


risk_plot<-cntry_disc_npv %>%
   filter(npv>0) 

 ggplot(risk_plot,
    aes(x =  harvest/1e6,
    y =npv,
    color = risk_score))+
  geom_point(size=1) +
  scale_color_gradientn(name = 'Risk score',  colors = viridis(1000)) + 
  #coord_flip() +
  labs(x = "Average Annual Production (MMT)",
       y = " 10 yr NPV") +
  carib_theme()

ggsave(filename = paste0(figure_folder, '/risk_v_prod.png'), width = 6.5, height = 6)

avg_risk_df<-cntry_disc_npv %>%
  filter(npv>0) %>%
  group_by(country) %>%
  summarize(avg_npv = sum(npv),
            average_production = sum(harvest/1e6),
            risk = unique(risk_score)) %>%
  ungroup() %>%
  mutate(unit = average_production/avg_npv)


ggplot(avg_risk_df,aes(x=fct_reorder(country,risk),y=unit,fill=risk))+
  geom_bar(stat= "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Country") +
  ylab("Production (MMT)/NPV")
  
  
p_v_npv<-ggplot(avg_risk_df,
            aes(x = risk,y =average_production,size = avg_npv)) +
            geom_point(size=3) +
        #    coord_flip() +
         #   scale_color_gradientn(name = 'risk',  colors = viridis(1000)) +
            labs(x = 'Average annunal farm production (MMT)',
                 y = 'Average Farm 10 year NPV') +
            carib_theme() 


r_v_npv<-ggplot(avg_risk_df,
                aes(x = risk,y =avg_npv)) +
  geom_point() +
  coord_flip() +
#  scale_color_gradientn(name = 'Avg. annual farm production (MMT)',  colors = viridis(1000)) +
  labs(x = 'Risk Score',
       y = 'Average Farm 10 year NPV') +
  carib_theme() 


p_v_npv + 
  r_v_npv +
    plot_layout(ncol = 1) +
       theme(plot.margin = unit(c(0,0,0,0), "cm"))

attach(avg_risk_df)
#scatterplot3d(risk,y = average_production,z = avg_npv, box= FALSE)


ggsave(filename = paste0(figure_folder, '/avg_risk_v_npv.png'), width = 6.5, height = 6)


# Boxplots ---------------------------------------------------------------


# boxplot of farm annuity by EEZ
bpA <- main_npv %>%  
  group_by(eez) %>% 
  mutate(farms = n_distinct(cell[annuity > 0]),
         annuity = ifelse(annuity / 1e6 < -2.5, -2.5, annuity)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(country, annuity, fun = 'median'), 
             y = annuity / 1e6,
             color = farms)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = median(annuity, na.rm = T) / 1e6), linetype = 2, color = 'black') +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  scale_y_continuous(labels = comma) +
  scale_color_gradientn(name = '# profitable', trans = 'log10',
                        breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                        colors = viridis(100)) +
  coord_flip() +
  labs(x = 'Country',
       y = 'Annuity ($USD, millions)',
       title = "Distribution of farm profitability by EEZ",
       subtitle = "10.6% discount rate") +
  carib_theme()  
 
ggsave(filename = paste0(figure_folder, '/annuity_cntry_boxplot.png'), width = 6.5, height = 6)

# boxplot of farm annuity by EEZ with country-specific discount rates
bpA2 <- cntry_disc_npv %>% 
  group_by(eez) %>% 
  mutate(farms = n_distinct(cell[annuity > 0]),
         annuity = ifelse(annuity / 1e6 < -2.5, -2.5, annuity)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(country, npv, fun = 'median'), 
             y = annuity / 1e6,
             color = farms)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = median(npv, na.rm = T) / 1e6), linetype = 2, color = 'black') +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  scale_y_continuous(labels = comma) +
  scale_color_gradientn(name = '# profitable', trans = 'log10',
                        breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
                        colors = viridis(100)) +
  coord_flip() +
  labs(x = 'Country',
       y = 'Annuity ($USD, millions)',
       title = "Distribution of farm profitability by EEZ",
       subtitle = "Country-specific discount rate") +
  carib_theme()

ggsave(filename = paste0(figure_folder, '/annuity_cntry_disc_boxplot.png'), width = 6.5, height = 6)

# boxplot of farm supply by EEZ
# bpB <- main_npv %>% 
#   group_by(eez) %>% 
#   mutate(farms = n_distinct(cell[annuity > 0])) %>% 
#   ungroup() %>%  
#   ggplot(aes(x = fct_reorder(country, annuity, fun = 'median'),
#              y = total_harvest / 1e3 / 10,
#              color = farms)) +
#   geom_boxplot() +
#   geom_hline(aes(yintercept = median(total_harvest, na.rm = T) / 1e3 / 10), linetype = 2, color = 'black') +
#   scale_y_continuous(labels = comma) +
#   scale_color_gradientn(name = '# profitable', trans = 'log10',
#                         breaks = c(10, 100, 1000, 10000), labels = c(10, 100, 1000, 10000),
#                         colors = viridis(100)) +
#   coord_flip() +
#   labs(x = 'Country',
#        y = 'Annual supply (MT)') +
#   carib_theme()
# 
# ggsave(filename = paste0(figure_folder, '/supply_cntry_boxplot.png'), width = 6.5, height = 6)
# 
# # Combine plots in grid
# boxplot_final <- bpA + bpB
# 
# ggsave(boxplot_final, filename = paste0(figure_folder, '/cntry_boxplot.png'), width = 8, height = 10)
# 
# # Histogram of profitable farms by price and discount scenarios
npv_df %>%  
  filter(feed_price_index == 1 & prices == 8.62) %>% 
  ungroup() %>%
  ggplot(aes(x = npv / 1e6, fill = disc_scenario)) +
  geom_histogram(binwidth = 0.5, position = 'dodge') +
  labs(x = 'NPV (millions)') +
  carib_theme() 

ggsave(filename = paste0(figure_folder, '/npv_discount_scenarios.png'), width = 6, height = 8)

# Country npv by discount scenario
bpC <- npv_df %>%
  filter(prices == 8.62 & feed_price_index == 1) %>% 
  group_by(disc_scenario) %>% 
  mutate(carib_avg = mean(npv, na.rm = T)) %>% 
  ungroup() 

med_bpC<-bpC %>%
  group_by(country,disc_scenario) %>%
  summarise(median_npv = median(npv)) %>%
  ungroup() %>%
  spread(disc_scenario,median_npv)

colnames(med_bpC)<-c("country","fixed","cntry")
 
med_bpC<-med_bpC%>%
   mutate(diff = abs(fixed-cntry),
          rel_change = abs(diff/fixed*100))

bpC<-left_join(bpC,med_bpC)

C<-  ggplot(bpC,aes(x = fct_reorder(country,rel_change), y = npv / 1e6, color = disc_scenario)) +
  geom_boxplot() +
  scale_color_brewer(palette = 'Paired', 
                     labels = c("'profitable uniform'", "'profitable risk'")) +
  labs(x = 'Country',
       y = 'NPV ($USD, millions)',
       color = "Scenario") +
  carib_theme()

# Calculate Caribbean boxplot stats
ylim1 <- boxplot.stats(C$data$npv)$stats[c(1, 5)] / 1e6

C +
  coord_flip(ylim = ylim1) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'red') +
  geom_hline(aes(yintercept = carib_avg / 1e6, color = disc_scenario), linetype = 2) +
  theme(legend.position = 'bottom')
  

ggsave(filename = paste0(figure_folder, '/boxplot_npv_disc_scenarios.pdf'), width = 180, height = 185, units = 'mm')

# Caribbean supply curve --------------------------------------------------

# Feed change variable
feed_change <- paste0("10 % discount rate, ", (unique(npv_df$feed_price_index)[2] - 1) * 100, "% increase in feed price") 

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
  mutate(scenario = ifelse(feed_price_index == 1, "Current", feed_change)) %>% 
  bind_rows(supply_plot_df %>%
              filter(total_supply > 0 & disc_scenario == "cntry" & feed_price_index == 1) %>% 
              mutate(scenario = 'Investment risk')) 
 # filter(feed_price_index == 1) #Took out scenario of increased feed price

write_csv(supply_plot_df, path = paste0(result_folder, '/supply_plot_df.csv'))

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
                   x = total_supply[prices == 8.62 & scenario == feed_change] / 1e6,
                  xend = total_supply[prices == 8.62 & scenario == feed_change] / 1e6),
              linetype = 2, color = 'grey50') +
  coord_cartesian(ylim = c(min(supply_plot_df$prices),max(supply_plot_df$prices))) +
  scale_y_continuous(breaks = unique(supply_plot_df$prices),
                     labels = unique(supply_plot_df$prices)) +
  scale_color_manual(values = c( "#377EB8", "#E41A1C" ,"#4DAF4A"),
                     labels = c("Current" = '10 % discount rate, current feed price',
                               feed_change,
                                "Investment risk" = 'Country specific discount rate, current feed price')) +
  labs(y     = 'Cobia price ($US/kg)',
       x     = 'Annual Caribbean supply (MMT)',
       color = NULL) +
  carib_theme() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'vertical')

ggsave(filename = paste0(figure_folder,'/carib_supply_curves.png'), width = 5, height = 6)

}
