##
## Caribean Aquaculture Figures
##


# Setup -------------------------------------------------------------------

# Packages
library(tidyverse)

run_name = 'fixed_Jan_22/' 
boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'
run_dir<-paste(boxdir,'results/',run_name, "/" ,sep = "")


# Data
npv_df <- read_csv(file = paste0(run_dir,"Results/npv_df.csv"))

eez_supply<- read_csv(file=paste0(run_dir,"Results/eez_supply_df.csv"))

sim_results<-read_csv(file=paste0(run_dir,"Results/sim_function_results.csv"))


monthly_cashflow<-read_csv(file=paste0(run_dir,"Results/monthly_cashflow.csv"))

# Plots -------------------------------------------------------------------

# Filter out final month
fig1_df <-  npv_df %>% 
  filter(month==120) %>% 
  select(country, npv, prices, discounts)

# remove full dataset
# rm(npv_df)
count<-unique(npv_df$country)

for ( i in 1: length(count)){

fig1_df %>% 
  filter(npv$country= count[i]) %>%
  ggplot(aes(y = npv, x = factor(prices))) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  main (count) +
  facet_wrap(~discounts)
  
}

# fig1_df %>% 
#   ggplot(aes(y = npv, x = factor(discounts))) +
#   geom_boxplot() +
#   geom_hline(yintercept = 0, linetype = 2, color = 'red') +
#   facet_wrap(~prices)
