##########################################################
## Project: Caribbean Aquaculture
## Script purpose: Calculate Caribbean seafood production
## Date: 02-27-2018
## Author: Tyler Clavelle
#########################################################


# Packages and data ------------------------------------------------------

# Packages
library(tidyverse)
library(scales)
library(viridis)

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 10),
          axis.text    = element_text(size = 10),
          legend.text  = element_text(size = 10))
}

# Aqua and fishery data
aqua <- read.csv(file = '../../Google Drive/Project Data/fao-data/20-02-2017_fao_aquaculture_production.csv', 
                 stringsAsFactors = F) %>% tbl_df()
fish <- read.csv(file = '../../Google Drive/Project Data/fao-data/20-02-2017_fao_capture.csv', 
                 stringsAsFactors = F, na.strings = c('...','0 0','-')) %>% tbl_df()

# Caribbean countries
carib <- c("Anguilla",
                   "Antigua and Barbuda",
                   "Aruba",
                   "Bahamas",
                   "Barbados",
                   "Bonaire/S.Eustatius/Saba",
                   "British Virgin Islands",
                   "Cayman Islands",
                   "Cura\xe7ao",
                   "Cuba",
                   "Dominica",
                   "Dominican Republic",
                   "Grenada",
                   "Guadeloupe",
                   "Haiti",
                   "Jamaica",
                   "Martinique",
                   "Montserrat",
                   "Puerto Rico",
                   "Saint Barth\xe9lemy",
                   "Saint Kitts and Nevis",
                   "Saint Lucia",
                   "Saint-Martin",
                   "Saint Vincent/Grenadines",
                   "Sint Maarten",
                   "Trinidad and Tobago",
                   "Turks and Caicos Is.",
                   "US Virgin Islands")

# Data wrangling ----------------------------------------------------------

## Reshape and clean FAO data
# Capture data
fish <- fish %>%
  gather('Year', Catch, contains('X'),convert=T) %>%
  mutate(Year  = as.numeric(gsub('X','',Year)),
         Catch = as.numeric(gsub(pattern = ' F', replacement = '', Catch)),
         Type  = 'Fisheries') %>%
  filter(!`Country..Country.` %in% c('Totals - Quantity (tonnes)', 'Totals - Quantity (number)')) 

# Aquaculture data
aqua <- aqua %>%
  gather('Year', Catch, contains('X'),convert=T) %>%
  mutate(Year  = as.numeric(gsub('X','',Year)),
         Catch = as.numeric(gsub(pattern = ' F', replacement = '',Catch)),
         Type  = 'Aquaculture') %>%
  filter(!`Country..Country.` %in% c('Totals - Quantity (tonnes)', 'Totals - Quantity (number)')) %>% 
  mutate(Type = ifelse(Environment..Environment. == "Freshwater", "Freshwater Aquaculture", "Mariculture"))
  
# Filter out Caribbean seafood production
carib_seafood <- fish %>% 
  filter(Country..Country. %in% carib) %>% 
  bind_rows(aqua %>% 
              filter(Country..Country. %in% carib))

# Summarize Caribbean Seafood Production ----------------------------------

carib_eez_fao_summary <- carib_seafood %>% 
  select(Year, Country..Country., Catch, Type) %>% 
  rename(Country = Country..Country.) %>% 
  group_by(Year, Country, Type) %>% 
  summarize(Production_MT = sum(Catch, na.rm = T)) %>% 
  arrange(Country, Year)

carib_eez_fao_summary2 <- carib_eez_fao_summary %>% 
  filter(Year == 2014)

write_csv(carib_eez_fao_summary2, path = '../../Box Sync/Carib_aqua_16/data/carib_seafood_2014.csv')

carib_fao_summary <- carib_seafood %>% 
  select(Year, Country..Country., Catch, Type) %>% 
  rename(Country = Country..Country.) %>% 
  group_by(Year, Type) %>% 
  summarize(Production_MT = sum(Catch, na.rm = T)) %>% 
  arrange(Year) %>% 
  ungroup()

ggplot(carib_fao_summary, aes(x = Year, 
                              y = Production_MT, 
                              fill = fct_relevel(Type, "Mariculture", "Freshwater Aquaculture","Fisheries"))) +
  geom_area() +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(discrete = T) +
  labs(fill = 'Sector',
       y    = 'Production (MT)') +
  carib_theme() +
  theme(legend.position = 'bottom')

ggsave(filename = '../../Box Sync/Carib_aqua_16/results/carib_seafood_production.png', width = 6, height = 4)
  