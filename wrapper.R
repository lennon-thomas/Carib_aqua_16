##################################################
## Project: Caribbean Aquaculture Potential
## Script purpose: Analysis wrapper
## Date: 11/02/2017
## Author: Lennon Thomas & Tyler Clavelle
##################################################

# Load packages and functions-----------------------------------------------------------

rm(list = ls())

library(raster)
library(rgdal)
library(tmap)
library(ncdf4)
library(stringr)
library(broom)
library(fasterize)
library(sf)
library(tidyverse)
library(patchwork)
library(parallel)
library(R.utils)
library(skimr)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(rlang)
library(ggforce)
library(forcats)
library(ggpubr)
library(ggridges)
library(forcats)
library(scatterplot3d)

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 7),
          axis.text    = element_text(size = 6),
          legend.text  = element_text(size = 5))
}

# Run settings -------------------------------------------------------------

## Set User (lennon/tyler)
user <- 'lennon'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

source(file = 'functions/sim_function.R')
source(file = 'functions/economic.R')
source(file = 'functions/process_production.R')
source(file = 'functions/econ_data_prep.R')
source(file = 'functions/plot_map.R')
source(file = 'functions/supply_curves.R')
source(file = "functions/econ_figures.R")
source(file = "functions/map_results.R")
source(file = "functions/calc_monthly_avgs.R")
source(file = "functions/heatmaps.R")

# Run name
run_name = 'nat_revisions'

# Paths to run folders 
run_dir<-paste(boxdir,'results/',run_name, "/" ,sep = "")
figure_folder <- paste0(run_dir,'Figures/')
result_folder <- paste0(run_dir,'Results/')
data_folder <-paste0(run_dir,'Data/')

if (dir.exists(run_dir) == F) {
  
  # Create directories if not already existing
  dir.create(run_dir, recursive = T)
  dir.create(figure_folder, recursive = T)
  dir.create(result_folder, recursive = T)
  dir.create(data_folder,recursive = T)
  
} else {
  print('Folder already exists')
}

# Analysis sections
monthly_avgs <- FALSE # Process SST and growth rasters to save CSV of monthly average temps and growth for suitable cells
econ_prep_data <-  FALSE # prep economic data files (TRUE) or just read in existing files (FALSE)
fix_int_stock <- FALSE # should the number of fingerlings used to stock each farm be fixed? false means they will be calculated to reach a stock density = havest density
process_growth <- FALSE # process growth data to get average growth and number of harvest cycles per cell
run_sim <- FALSE # run population simulation to calculate feed costs
run_econ <- FALSE # run economic analyses

# Parameters --------------------------------------------------------------

# Constant parameters
cage_cost <- 321000 #Lipton et al. 2007 seastation cages that are half the volume are $269,000 and total equitment for startup is $321,000 sUS$ cage and installation Lipton and Kim. For 3000 m^3 cages and this includes all the gear (anchors, etc) 
support_vessel <- 150000 # $US/year according to Rubino et al. (2008) and assuming 10 year timeperiod
onshore_cost <- 30000 # $US/yr for docking costs according to Rubino et al. (2008)
site_lease <- 10000 # Cost of 10 year permit in Gulf of Mexico
labor_installation <- 52563*2.655 # US$ from Bezerra et al. 2016 scaled to the volume of our farm
site_hours <- 160 # monthly hours per worker per month (8*4 weeks * 5 days)
site_workers <- 40 # Bezerra et al. 2016 . has 17 scaled to our farm volume open blue employs over 200 full time people. 1 admin 1 prod manager 1 boat captain and 37 crew
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
no_fingerlings <- 256000 # fixed fingerlings per farm
price_fingerlings <- 1.50 # between 1.37-1.93 personal comm with Dr. Rombenso May 2018# # $US/fingerling Bezerra et al. 2016
harv_den <- 10 # kg/m^3 harvest density Benetti et al. 2010 and Dane's industry contacts
no_cage <- 16 # cages
cage_volume <- 6400 # m^3
total_vol <- no_cage*cage_volume # m^3 total cage volume
harvest_weight <- 5 # kg from various souces (5-6 kg)
cobia_price <- 8.62 # $US/kg# Bezerra et al. 2016
fcr <- 1.75 # Benetti et al. 2010 for the whole Carib region. F.C.R. = Feed given / Animal weight gain. 
feed_price <- 2 ##personal comm with Dr. Rombenso May 2018 that said 1.87/kg # in units of $/kg  ($1.64 from Bezerra et al. 2016)
survival <- 0.75 # Benetti et al. 2007 over 12 monthes and Huang et al. 2011
month_mort <- 1 - survival ^ (1 / 12)
int_weight <- 0.015 # kg (15 grams) Bezerra et al. 2016
no_trips <- 2 # number of trips to farm per day
sim_length <- 120 # length of simulation (months) 
avg_boat_spd <- 15000 # in meters per hour (15 km/hr from Rubino et al. 2008)    
site_days <- 30
disc_rate <- 0.10 # discount rate to use in addition to country specific rates. can be a vector.
feed_rate <- c(0.03, 0.02, 0.01, 0.01) # feed rate is 2% body weight Benetti et al. 2010
mainent <- 0.00583 # 7% of capital costs annually from Knapp 
elec<-0.09 # 0.09% of monthly operating costs Bezerra et al. 2016

# Load and Process Data ---------------------------------------------------------------

# load cell area data
cell_area <- read_csv(file = paste0(boxdir, 'data/cell_area_df.csv')) %>% 
  dplyr::select(cell_no, study_area_km, suit_index)

suit_map<-raster(paste0(boxdir,"Suitability/results/suitable_areas.tif"))

suit_map[suit_map>0]<-1
suit_map[suit_map==0]<-NA

# need to break up economic data prep and sim function 
if (econ_prep_data == TRUE){
  # Run econ data prep function
  econ <- econ_data_prep()
  
  # extract rasters  
  prod <- econ[['prod']]
  growth <- econ[['growth']]
  econ_stack <- econ[['econ_stack']]
 
  # remove list
   rm(econ)
  
} else {
  
  file_names <- list.files(path = paste(boxdir,"data/economic_final/", sep = ""), pattern = ".nc")
  
  model_files <- lapply(paste(boxdir,"data/economic_final/",file_names, sep = ""), brick)
  
  growth <- model_files[[1]]
  
  prod <- model_files[[2]]
  
  econ_stack<-model_files[[3]]
 
}

# Calculate and save csv of average monthly growth and temperature by cell

if(monthly_avgs == TRUE) {
  # Run monthly average function
  monthly_temps_growth <- calc_monthly_avgs(boxdir, data_folder)
  # Produce heatmap figure
  heatmaps(avg_results = monthly_temps_growth, boxdir, figure_folder, carib_theme)
 } else {
  # Otherwise read in csv
  monthly_temps_growth <- read_csv(file = paste0(data_folder, 'monthly_temp_and_growth.csv'))
  # Produce heatmap figure
  heatmaps(avg_results = monthly_temps_growth, boxdir, figure_folder, carib_theme)
}

# Calculate average growth, cycle length, and no. of fingerlings

 if(process_growth == TRUE) {
  
   annual_prod <- ann_prod(growth = growth, start_weight = int_weight, harv_den = harv_den)  
 
   stocking_n <- annual_prod[[1]]
   
   stocking_n<-mask(stocking_n,suit_map)
  
   harvest_cycles <- annual_prod[[2]]
   
   harvest_cycles<-mask(harvest_cycles,suit_map)
  
   harvest_cycle_length <- annual_prod[[3]]
   
   harvest_cycle_length<-mask(harvest_cycles,suit_map)
  
   avg_month_growth <- avg_growth(growth = growth)
   
   avg_month_growth<-mask(avg_month_growth,suit_map)
   
   # Save avg monthly growth and initial stocking rasters
   writeRaster(avg_month_growth, paste0(data_folder,'avg_month_growth_stack.grd'), overwrite =TRUE) ##getting weird error when trying to save this file
   writeRaster(stocking_n, paste0(data_folder,'initial_stocking_stack.grd'), overwrite =TRUE)
   writeRaster(harvest_cycles, paste0(data_folder,'harvest_cycles.grd'), overwrite =TRUE)
   writeRaster(harvest_cycle_length, paste0(data_folder,'harvest_cycle_length.grd'), overwrite =TRUE)
 
 } else {
   
    stocking_n<-brick(paste0(data_folder,'initial_stocking_stack.grd'))
    
    harvest_cycles<-brick(paste0(data_folder,'harvest_cycles.grd'))
    
    harvest_cycle_length<-brick(paste0(data_folder,'harvest_cycle_length.grd'))
    
    avg_month_growth<-brick(paste0(data_folder,'avg_month_growth_stack.grd'))

}
  # Fixes or caluclates intial stocking number
  
  if (fix_int_stock == TRUE) {

    stocking_n[stocking_n > 0] <- no_fingerlings
  
  } else {
  
    stocking_n <- stocking_n
    
  }
   
  # Run Projection ----------------------------------------------------------

  if (run_sim == TRUE){
  
    # Run simulation
    sim_results <- sim_aqua(avg_month_growth = avg_month_growth, 
                            stocking_n = stocking_n, 
                            int_weight = int_weight, 
                            sim_length = sim_length, 
                            month_mort = month_mort, 
                            feed_rate  = feed_rate,
                            fcr        = fcr)
    
    # Set feed to either value calculated by the feeding rate or by the fixed FCR
    sim_results <- sim_results %>% 
      ungroup() %>% 
      mutate(feed = feed_by_rate)
    
    # FCR summary
    fcr_results <- sim_results %>%
      ungroup() %>% 
      filter(feed > 0) %>% 
      group_by(cell, harvest_cycle) %>% 
      summarize(fcr_rate  = sum(feed_by_rate, na.rm = T) / sum(harvest, na.rm = T),
                fcr_fcr   = sum(feed_by_fcr, na.rm = T) / sum(harvest, na.rm = T),
                harvest_length = n_distinct(month)) %>% 
      ungroup()
  
    # Save simulation results
    write_csv(sim_results, path = paste0(run_dir,"Results/sim_function_results.csv"))
    write_csv(fcr_results, path = paste0(run_dir,"Results/fcr_results.csv"))
  
    } else {
 
    sim_results <- read_csv(paste0(run_dir,"Results/sim_function_results.csv"))
    fcr_results <- read_csv(paste0(run_dir,"Results/fcr_results.csv"))
 
    }

# Run economic analyses ---------------------------------------------------------

if(run_econ == TRUE) {
  
  monthly_cashflow <- monthly_cost_est(sim_results,
                                       econ_stack,
                                       stocking_n,
                                       site_lease,
                                       no_cage,
                                       labor_installation,
                                       support_vessel,
                                       onshore_cost,
                                       site_hours,
                                       site_workers,
                                       avg_boat_spd,
                                       feed_price,
                                       price_fingerlings,
                                       cage_cost,
                                       site_days,
                                       no_trips,
                                       mainent)
  
  monthly_cashflow[is.na(monthly_cashflow)] <- 0
  
  # Save simulation results and monthly cashflow
  write_csv(monthly_cashflow, path = paste0(run_dir,"Results/monthly_cashflow.csv"))

  # Set file names to match Box directory path
  econ_stack@file@name <- paste0(boxdir,'data/economic_final/econ_stack.nc')
   
  # Read in Caribbean EEZ shapefile
  carib_eez <- readOGR(dsn = paste(boxdir, "data/economic_prep/",sep = ""), layer = "carib_eez_shape")
  countries <- as.data.frame(carib_eez[,c(2,6)])
  names(countries) <- (c("eez","country"))
    
  # Add risk scores and discount rates to EEZ dataframe
  risk_scores <- read_csv(paste0(boxdir, 'data/final_risk_score.csv')) %>% 
    rename(country = Territory1) %>% 
    mutate(country = ifelse(country == 'Curacao', 'Curaçao', country),
           country = ifelse(country == 'Saint-Barthelemy', "Saint-Barthélemy", country))
    
 # us_risk<-risk_scores$risk_score[risk_scores$country=="Cayman Islands"]         
  # Join country lookup table with risk scores and calculate discount rates based on Brazil score (2.93) and discount rate (14%)
  countries <- countries %>% 
    left_join(risk_scores) %>% 
  #  mutate(risk_score  = ifelse(country == "Saint-Barthélemy", 2.2, risk_score),
   #        disc_rate   = pmax(pmin(0.1 * (1-(us_risk - risk_score)), 0.25), 0.1)) # Using USVI as baseline for now
    mutate(disc_rate = rescale(risk_score, to = c(0.1,.25)))
  write.csv(countries, paste0(boxdir,"data/country_risk_and_discount.csv"))
  
  # Run supply curve analysis
  supply_curves_results <- supply_curves(cashflow = monthly_cashflow, 
                                         cobia_price = cobia_price, 
                                         prices = c(6, 6.5, 7, 7.5, 8, 9, 9.5,10,10.50,11),
                                         feed_price_index = c(1, 1.25),
                                         discount_rates = disc_rate,
                                         eezs = countries, 
                                         area = cell_area,
                                         figure_folder = figure_folder, 
                                         result_folder = result_folder,
                                         elec)
  
  # Extract supply curve results
  npv_df <- supply_curves_results[['npv']]
  eez_supply <- supply_curves_results[['eez_supply']]
  carib_supply <- supply_curves_results[['carib_supply']]
  supply_summary <- supply_curves_results[['supply_summary']]

  # Select on the last month of simulation for every cell
  npv_df <- npv_df %>%
    group_by(cell) %>%
    filter(month == max(month))
      
  # Save supply curve results 
  write.csv(npv_df,paste0(run_dir,"Results/npv_df.csv"))
  write.csv(eez_supply,paste0(run_dir,"Results/eez_supply_df.csv"))
  write.csv(carib_supply,paste0(run_dir,"Results/carib_supply.csv"))
  write_csv(supply_summary, path = paste0(run_dir,'Results/supply_summary.csv'))
  

} else {
  
  # Read in economic files
  monthly_cashflow <- read_csv(file = paste0(run_dir,"Results/monthly_cashflow.csv"))
  npv_df <- read_csv(file = paste0(run_dir,"Results/npv_df.csv"))
  eez_supply <- read_csv(file = paste0(run_dir,"Results/eez_supply_df.csv"))
  carib_supply <- read_csv(file = paste0(run_dir,"Results/carib_supply.csv"))
  supply_summary <- read_csv(file = paste0(run_dir,"Results/supply_summary.csv"))
  countries<- read_csv(paste0(boxdir,"data/country_risk_and_discount.csv"))
  
}

# Produce figures for paper -----------------------------------------------

# Econ figures
econ_figures(npv_df = npv_df,
             cashflow = monthly_cashflow,
             sim_results = sim_results,
             supply_summary = supply_summary,
             carib_theme,
             result_folder,
             figure_folder,
             countries)
# Maps  
map_results(boxdir,
              fig_folder = figure_folder,
              avg_growth = avg_month_growth,
              harv_cycle_length = harvest_cycle_length,
              harvest_cycles,
              stocking_n,
              carib_supply,
              eez_supply,
              npv_df,
              supply_summary,
              feed_price,
              price_fingerlings)

# Save text file documenting run settings ---------------------------------

# Write text file that includes run info- may want to add more to this (like price) later
cat(paste0("Date: ", Sys.Date()),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n")
cat(paste0("Run name: ",run_name),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)
cat(paste0("Fix_int_stock? ",fix_int_stock),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)   
cat(paste0("Run name: ",run_name),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)
cat(paste0("FCR: ",fcr),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)
cat(paste0("Discount rate (additional): ",disc_rate),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)
cat(paste0("Harvest density: ",harv_den),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)
cat(paste0("Feed price: ",feed_price),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE)  
cat(paste0("Feed rate: ",feed_rate),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE) 
cat(paste0("Fingerling price: ",price_fingerlings),file = paste0(run_dir,'Results/rundescription.txt'), sep="\n",append = TRUE) 
