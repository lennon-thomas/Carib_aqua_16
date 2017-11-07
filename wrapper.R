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
library(parallel)
library(R.utils)

# Run settings -------------------------------------------------------------

## Set User (lennon/tyler)
user <- 'tyler'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

source(file = 'functions/sim_function.R')
source(file = 'functions/economic.R')
source(file = 'functions/process_production.R')
source(file = 'functions/econ_data_prep.R')
source(file = 'functions/plot_map.R')

run_name = "test"

run_dir<-paste(boxdir,'results/',run_name, "/" ,sep = "")

if (dir.exists(run_dir) == F) {
  dir.create(run_dir, recursive = T)
} else {
  
  print('Folder already exists')
}

prep_data = FALSE# Prep economic data files (TRUE) or just read in existing files (FALSE)
fix_int_stock =FALSE #should the number of fingerlings used to stock each farm be fixed? false means they will be calculated to reach a stock density = havest density

# Parameters --------------------------------------------------------------

# Constant parameters
cage_cost <- 269701 # US$ cage and installation Lipton and Kim. For 3000 m^3 cages and this includes all the gear (anchors, etc) 
support_vessel <- 158331 #US$ Bezerra et al. 2016: 16-m-long boat with a 6-cylinder motor and a hydraulic winch  #50000 # US$ 32'ft from Kam et al. 2003
site_lease<-3265 # US$ from Bezerra et al. 2016 This is for 16 ha farm in Brazil (ours is larger so may want to increase)
labor_installation<-52563 # US$ from Bezerra et al. 2016
site_hours <- 160 # monthly hours per worker per month (8*4 weeks * 5 days)
site_workers <- 17 # Bezerra et al. 2016 
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
#no_fingerlings <- 256000 # fingerlings per farm
price_fingerlings <- 2.58 #$US/fingerling Bezerra et al. 2016
harv_den<-15 #kg/m^3 harvest density Benetti et al. 2010 and Dane's industry contacts
no_cage<-16 # cages
cage_volume<-6400 #m^3
total_vol<-no_cage*cage_volume # m^3 total cage volume
harvest_weight<-5 # kg from various souces (5-6 kg)
cobia_price<- 8.62 #$US/kg# Bezerra et al. 2016
fcr<-1.75 #Benetti et al. 2010 for the whole Carib region. F.C.R. = Feed given / Animal weight gain. 
feed_price<-1.64 #in units of $/kg  Bezerra et al. 2016
survival<-0.75 #Benetti et al. 2007 over 12 monthes and Huang et al. 2011
month_mort<- 1-survival ^ (1 / 12)
int_weight<-0.015 # kg (15 grams) Bezerra et al. 2016
no_trips<-2 #number of trips to farm per day
sim_length <- 120 # length of simulation (months) 
avg_boat_spd <- 48.28    
site_days <- 30
discount_rate<-0.05

# Load Data ---------------------------------------------------------------

if (prep_data == TRUE){
 
  # Run econ data prep function
   econ <- econ_data_prep()
   # extract rasters
   prod <- econ[['prod']]
   growth <- econ[['growth']]
   econ_stack <- econ[['econ_stack']]
   # remove list
   rm(econ)
  
# Calculate average growth, cycle length, and no. of fingerlings --------
   
   annual_prod<-ann_prod(growth = growth)  
   
   stocking_n<-annual_prod[[1]]
   
   harvest_cycles<-annual_prod[[2]]
   
   harvest_cycle_length<-annual_prod[[3]]
   
   avg_month_growth<-avg_growth(growth = growth)
   
   # Run Projection ----------------------------------------------------------
   
   sim_results <- sim_aqua(avg_month_growth = avg_month_growth, stocking_n = stocking_n, int_weight = int_weight, sim_length = sim_length, month_mort = month_mort, fcr = fcr)
   
   # Calculate monthly costs and revenue costs by cell ---------------------------------------------------------
   
   monthly_cashflow <- monthly_cost_est(sim_results,econ_stack,stocking_n,site_lease,no_cage,labor_installation,support_vessel,site_hours,site_workers,avg_boat_spd,feed_price,price_fingerlings,cage_cost,site_days,discount_rate)
   
   monthly_cashflow[is.na(monthly_cashflow)] <- 0

   # Save results ------------------------------------------------------------
   
   # Save avg monthly growth and initial stocking rasters
   writeRaster(avg_month_growth, paste0(boxdir,'data/avg_month_growth_stack.nc'), format = "CDF", overwrite =TRUE)
   writeRaster(stocking_n, paste0(boxdir,'data/initial_stocking_stack.nc'), format = "CDF", overwrite =TRUE)
   writeRaster(harvest_cycles, paste0(boxdir,'data/harvest_cycles.nc'), format = "CDF", overwrite =TRUE)
   writeRaster(harvest_cycle_length, paste0(boxdir,'data/harvest_cycle_length.nc'), format = "CDF", overwrite =TRUE)
   
   # Save simulation results and monthly cashflow
   write_csv(sim_results, path = paste0(run_dir,"sim_function_results.csv"))
   write_csv(monthly_cashflow, path = paste0(run_dir,"monthly_cashflow.csv"))

   } else {
  
  load(paste(run_dir, 'economic_data.Rdata',sep=""))
  
  stocking_n<-raster(paste0(boxdir,'data/initial_stocking_stack.nc'))
  harvest_cycles<-stack(paste0(boxdir,'data/harvest_cycles.nc')) 
  harvest_cycle_length<-stack( paste0(boxdir,'data/harvest_cycle_length.nc')) 
  avg_month_growth<-stack( paste0(boxdir,'data/avg_month_growth_stack.nc'))
  
  # Read simulation results
  sim_results <- read_csv(paste0(run_dir,"sim_function_results.csv"))
  monthly_cashflow <- read_csv(paste0(run_dir,"monthly_cashflow.csv"))
  
  # Set file names to match Box directory path
  econ_stack@file@name <- paste0(boxdir,'economic/data/final/econ_stack.nc')
  
}

# Summarize results ---------------------------------------------------------

cell_totals <- monthly_cashflow %>%
  group_by(cell,eez) %>%
  summarize(total_pv      = sum(present_value),
            total_c_costs = sum(c_costs),
            cum_cost      = sum(total_monthly_costs),
            cum_rev       = sum(monthly_revenue),
            total_feed    = sum(feed_cost),
            total_biomass = (sum(alive,na.rm=TRUE))*(sum(weight,na.rm=TRUE))) %>%
  mutate(profit = cum_rev - cum_cost, # undiscounted profit
         npv = total_pv - total_c_costs) %>% # NPV
  ungroup() 

# Read in Caribbean EEZ shapefile
carib_eez <- readOGR(dsn = paste(boxdir, "Suitability/tmp",sep = ""), layer = "carib_eez_shape")
carib_eez <- st_as_sf(carib_eez)
countries <- carib_eez[,c(2,6)]
names(countries) <- c("eez","country")

# Merge cell totals with EEZ shapefile
cell_totals_eez <- merge(cell_totals, countries, by ="eez")

# write.csv(cell_totals,paste0(run_dir,"cell_result_summary.csv"))

# EEZ totals
all_eez_summary <- cell_totals_eez %>%
  group_by(eez,country) %>%
  summarize(total_cost = sum(cum_cost),
            total_revenue = sum(cum_rev),
            total_profit = sum(profit),
            total_biomass=sum(total_biomass)) %>%
  ungroup()

# NPV by EEZ
eez_npv <- cell_totals %>%
  group_by(eez) %>%
  summarize(total_npv=sum(npv))

# Plots -------------------------------------------------------------------

# Create Figures folder in run directory
fig_folder <- paste0(boxdir,'results/',run_name, "/Figures/")

if (dir.exists(fig_folder) == F) {
  dir.create(fig_folder, recursive = T)
} else {
  
  print('Folder already exists')
}

# Read in Caribbean EEZ
carib_eez <- readOGR(dsn=paste(boxdir, "Suitability/tmp",sep = ""),layer = "carib_eez_shape")
test<-merge(carib_eez,eez_npv,by.x="MRGID",by.y="eez")

# Create raster layer of npv after 10 years
npv_raster <- stocking_n
npv_raster[cell_npv$cell] <- as.numeric(cell_npv$npv)

# Make Caribbean wide and regional maps of NPV
plot_map_zoom(carib_eez,npv_raster)
reg_map_plot(carib_eez,npv_raster)

# Plot variable cost layers
names(econ_stack) <- c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez")
tites <- c("Fuel price $US/gallon","Miniumum wage ($US)", "Permit fee $US","Risk score","Distance from shore (m)","Depth charge (%)","Distance charge(%)","EEZ")

var_costs <- tm_shape(carib_eez,is.master=TRUE)+
   tm_fill(col="lightblue",alpha = 0.4) +
   tm_borders(lwd = 1.2) +
   tm_shape(econ_stack) +
   tm_raster(c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez"),title=tites,style="cont") +
   tm_legend(position = c("right","top"),scale=1) 

# Save variable costs map
save_tmap(var_costs, paste0(fig_folder,"Variable costs.png"), width=1920, height=1080)

# Plot profits
profits <- tm_shape(test)+
           tm_fill("total_npv",palette=c("red","blue")) +
           tm_borders(lwd = 1.2) +
           tm_shape(profit_raster)+
           tm_raster( legend.show = TRUE,title="10 year Profit($USD)",palette=c("red","blue")) +
           tm_legend(position = c("right","top"),scale=1) 

# Save profits map
save_tmap(profits, paste0(fig_folder,"Profits.png"), width=1920, height=1080)
