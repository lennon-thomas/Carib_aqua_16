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
library(readr)

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

run_name = 'est_Jan_30'    #"calc_0.02" #run name reflects intital stocking density (calculated or fixed)and feed rates (as % body weight)

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


econ_prep_data = FALSE #Prep economic data files (TRUE) or just read in existing files (FALSE)
fix_int_stock = FALSE #should the number of fingerlings used to stock each farm be fixed? false means they will be calculated to reach a stock density = havest density
run_sim = TRUE #run population simulation to calculate feed costs

# Parameters --------------------------------------------------------------

# Constant parameters
cage_cost <- 269701 # US$ cage and installation Lipton and Kim. For 3000 m^3 cages and this includes all the gear (anchors, etc) 
support_vessel <- 158331 #US$ Bezerra et al. 2016: 16-m-long boat with a 6-cylinder motor and a hydraulic winch  #50000 # US$ 32'ft from Kam et al. 2003
site_lease <- 3265 # US$ from Bezerra et al. 2016 This is for 16 ha farm in Brazil (ours is larger so may want to increase)
labor_installation <- 52563 # US$ from Bezerra et al. 2016
site_hours <- 160 # monthly hours per worker per month (8*4 weeks * 5 days)
site_workers <- 17 # Bezerra et al. 2016 
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
no_fingerlings <- 256000 #fixed fingerlings per farm
price_fingerlings <- 2.58 #$US/fingerling Bezerra et al. 2016
harv_den <- 15 #kg/m^3 harvest density Benetti et al. 2010 and Dane's industry contacts
no_cage <- 16 # cages
cage_volume <- 6400 #m^3
total_vol <- no_cage*cage_volume # m^3 total cage volume
harvest_weight <- 5 # kg from various souces (5-6 kg)
cobia_price <- 8.62 #$US/kg# Bezerra et al. 2016
fcr <- 1.75 #Benetti et al. 2010 for the whole Carib region. F.C.R. = Feed given / Animal weight gain. 
feed_price <- 1.64 #in units of $/kg  Bezerra et al. 2016
survival <- 0.75 #Benetti et al. 2007 over 12 monthes and Huang et al. 2011
month_mort <- 1-survival ^ (1 / 12)
int_weight <- 0.015 # kg (15 grams) Bezerra et al. 2016
no_trips <- 2 #number of trips to farm per day
sim_length <- 120 # length of simulation (months) 
avg_boat_spd <- 48.28    
site_days <- 30
discount_rate <- 0.05
feed_rate <- 0.02 # feed rate is 2% body wweight Benetti et al. 2010

# Load Data ---------------------------------------------------------------
#need to break up economic data prep and sim function

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
  
  file.names <- list.files(path = paste(boxdir,"economic/data/final/", sep = ""), pattern = ".nc")
  
  model_files <- lapply(paste(boxdir,"economic/data/final/",file.names, sep = ""),brick)
  
  growth <- model_files[[1]]
  
  prod <- model_files[[2]]
  
  econ_stack<-model_files[[3]]

}
  
  # Calculate average growth, cycle length, and no. of fingerlings --------

  annual_prod<-ann_prod(growth = growth)  
 
  harvest_cycles<-annual_prod[[2]]
  harvest_cycle_length<-annual_prod[[3]]
  avg_month_growth<-avg_growth(growth = growth)
 
  #Fixes or caluclates intial stocking number
  
  if (fix_int_stock == TRUE) {
    stocking_n<-annual_prod[[1]]
    stocking_n[stocking_n > 0]<-no_fingerlings
  
    
  } else {
  
    stocking_n<-annual_prod[[1]]
  }
   
  # Run Projection ----------------------------------------------------------

  if (run_sim == TRUE){
  sim_results <- sim_aqua(avg_month_growth = avg_month_growth, stocking_n = stocking_n, int_weight = int_weight, sim_length = sim_length, month_mort = month_mort, fcr = fcr)
} else {
  sim_results <- read_csv(paste0(run_dir,"Results/sim_function_results.csv"))
 }
  
  # Calculate monthly costs and revenue costs by cell ---------------------------------------------------------
  
  monthly_cashflow <- monthly_cost_est(sim_results,econ_stack,stocking_n,site_lease,no_cage,labor_installation,support_vessel,site_hours,site_workers,avg_boat_spd,feed_price,price_fingerlings,cage_cost,site_days,discount_rate)
  monthly_cashflow[is.na(monthly_cashflow)] <- 0
  
  # Save results ------------------------------------------------------------
   
   # Save avg monthly growth and initial stocking rasters
   writeRaster(avg_month_growth, paste0(run_dir,'data/avg_month_growth_stack.nc'), format = "CDF", overwrite =TRUE) ##getting weird error when trying to save this file
   writeRaster(stocking_n, paste0(run_dir,'data/initial_stocking_stack.nc'), format = "CDF", overwrite =TRUE)
   writeRaster(harvest_cycles, paste0(run_dir,'data/harvest_cycles.nc'), format = "CDF", overwrite =TRUE)
   writeRaster(harvest_cycle_length, paste0(run_dir,'data/harvest_cycle_length.nc'), format = "CDF", overwrite =TRUE)
   
   # Save simulation results and monthly cashflow
   write_csv(sim_results, path = paste0(run_dir,"Results/sim_function_results.csv"))
   write_csv(monthly_cashflow, path = paste0(run_dir,"Results/monthly_cashflow.csv"))

  
  
  # Read simulation results
    #  sim_results <- read_csv(paste0(run_dir,"sim_function_results.csv"))
    # monthly_cashflow <- read_csv(paste0(run_dir,"monthly_cashflow.csv"))

  # Set file names to match Box directory path
    econ_stack@file@name <- paste0(boxdir,'economic/data/final/econ_stack.nc')
    #econ_names<-c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez")

  # Read in Caribbean EEZ shapefile
    carib_eez <- readOGR(dsn = paste(boxdir, "Suitability/tmp",sep = ""), layer = "carib_eez_shape")
    # carib_eez <- st_as_sf(carib_eez)
    countries <- as.data.frame(carib_eez[,c(2,6)])
    names(countries) <- (c("eez","country"))
    
# Run supply curve analysis
  supply_curves_results <- supply_curves(cashflow = monthly_cashflow, cobia_price = cobia_price, prices = c(5:15),
                                         discount_rates = c(0,0.05,0.1,0.15,0.2), eezs = countries, 
                                         figure_folder = figure_folder, result_folder = result_folder)
  

# Extract supply curve results
  npv_df <- supply_curves_results[['npv']]
  eez_supply <- supply_curves_results[['eez_supply']]
  carib_supply <- supply_curves_results[['carib_supply']]

# Save supply curve results  
  write.csv(npv_df,paste0(run_dir,"Results/npv_df.csv"))
  write.csv(eez_supply,paste0(run_dir,"Results/eez_supply_df.csv"))
  write.csv(carib_supply,paste0(run_dir,"Results/carib_supply.csv"))


# Summarize results ---------------------------------------------------------
  
  cell<-unique(npv_df$cell)
  
  coords<-xyFromCell(stocking_n,cell,spatial=FALSE)
  
  coords<-cbind(coords,cell)

  npv_sp<-merge(npv_df,coords,by='cell')
 
  #create sf object 
  npv_sp<-npv_sp %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(13,14))
  
  plot(npv_sp)
  
  #filter for one price
  
  run_price<- 8
  
npv_sp<-  npv_sp %>%
          filter(prices==run_price) %>%
          filter(month==120) %>%
          filter(discounts==0)
  
  ggplot(npv_sp) +
    geom_sf(aes(fill=npv))
  

  
# NPV results  
 # By cell
  npv_cell<-npv_df %>%
    group_by(cell,prices,discounts) %>%
    summarise(total_npv=sum(npv,na.rm =TRUE)) %>%
    ungroup()

 # By eez 
  npv_eez<-npv_df %>%
    filter(month==120) %>%
    group_by(eez,prices,discounts) %>%
    summarise(total_npv=sum(npv,na.rm =TRUE)) %>%
    ungroup()

  npv_eex<-merge(carib_eez,npv_eez,by.x = "MRGID", by.y = "eez", all.y = TRUE, all.x = TRUE)  
  
#H arvest results 
  # By cell
  total_harvest<-sim_results %>%
    group_by(cell) %>%
    summarise(t_harvest = sum(weight,na.rm=TRUE))
  
  eez_id<-monthly_cashflow %>%
    filter(year==1) %>%
    select("cell","eez") 
  
  eez_id <- merge(total_harvest, eez_id , by="cell", all = FALSE)
  
  harvest_totals_eez <- merge(carib_eez, eez_id , by.x ="MRGID",by.y="eez")
  
  # By eez
  
  # 
  # c_price <- 8 #specify the results you want nto extract (set the price of cobia)
  # 
  # npv_cell<-npv_df %>%
  #     filter(month==120) %>%
  #     filter(prices==c_price) 
  # 
  # 
  # carib_npv<-merge(carib_eez,npv_eez, by.x="MRGID",by.y="eez",all.y = TRUE)
  # 
  #  npv_cell$discounts<-as.factor(npv_cell$discounts)
  #    
  # npv_discounts<- split(npv_cell,npv_cell$discounts)
  # 
  # disc_0<-npv_discounts[[1]]
  # disc_05<-npv_discounts[[2]]
  # disc_1<-npv_discounts[[3]]
  # disc_15<-npv_discounts[[4]]
  # disc_2<-npv_discounts[[5]]
  # 
  # disc_0_raster<-stocking_n
  # disc_05_raster<-stocking_n
  # disc_1_raster<-stocking_n
  # disc_15_raster<-stocking_n
  # disc_2_raster<-stocking_n
  # disc_0_raster[disc_0$cell]<-disc_0$npv
  # disc_05_raster[disc_05$cell]<-disc_05$npv
  # disc_1_raster[disc_1$cell]<-disc_1$npv
  # disc_15_raster[disc_15$cell]<-disc_15$npv
  # disc_2_raster[disc_2$cell]<-disc_2$npv
  # 
  # discount_stack<-stack(disc_0_raster,disc_05_raster,disc_1_raster,disc_15_raster,disc_2_raster)
  # names(discount_stack)<-c("disc=0","disc=0.05","disc=0.10","disc=0.15","disc=0.20")
  # #discount_stack<-mask(discount_stack, depth)
  # #discount_stack[discount_stack==0]<-NA
  # 
  # npv_plot <- tm_shape(carib_eez,is.master=TRUE)+
  #   tm_fill(col="lightblue",alpha = 0.4,title = c_price) +
  #   tm_borders(lwd = 1.2) +
  #   tm_shape(discount_stack) +
  #   tm_raster(c("disc.0","disc.0.05","disc.0.10","disc.0.15","disc.0.20")),style="cont") +
  #   tm_legend(position = c("right","top"),scale=1)
  # 
  # save_tmap(npv_plot, paste0(figure_folder,"price= ",c_price, "npv.png"), width=1920, height=1080)

#Summarize total biomass harvested by cell








# !! Should rething remainder of code given supply chain analysis !!

# Calculate farm totals
# cell_totals <- monthly_cashflow %>%
#   group_by(cell,eez) %>%
#   summarize(total_pv      = sum(present_value),
#             total_c_costs = sum(c_costs),
#             cum_cost      = sum(total_monthly_costs),
#             cum_rev       = sum(monthly_revenue),
#             total_feed    = sum(feed_cost),
#             total_biomass = (sum(alive,na.rm=TRUE))*(sum(weight,na.rm=TRUE))) %>%
#   mutate(profit = cum_rev - cum_cost, # undiscounted profit
#          npv = total_pv - total_c_costs) %>% # NPV
#   ungroup() 
# 
# # Merge cell totals with EEZ shapefile
# cell_totals_eez <- merge(cell_totals, countries, by ="eez")
# 
# # write.csv(cell_totals,paste0(run_dir,"cell_result_summary.csv"))
# 
# # EEZ totals
# all_eez_summary <- cell_totals_eez %>%
#   group_by(eez,country) %>%
#   summarize(total_cost = sum(cum_cost),
#             total_revenue = sum(cum_rev),
#             total_profit = sum(profit),
#             total_biomass=sum(total_biomass)) %>%
#   ungroup()
# 
# # NPV by EEZ
# eez_npv <- cell_totals %>%
#   group_by(eez) %>%
#   summarize(total_npv=sum(npv))

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
eez_harvest<-merge(carib_eez,total_harvest,by.x="MRGID",by.y="cell") %>%
  as.data.frame() %>%
  dplyr::select(Territory1,Area_km2,t_harvest)

# Create raster layer of npv after 10 years
npv_raster <- stocking_n
npv_raster[cell_totals$cell] <- as.numeric(cell_totals$npv)

# Make Caribbean wide and regional maps of NPV
plot_map_zoom(carib_eez,npv_raster)
reg_map_plot(carib_eez,npv_raster)

plot_map_zoom(carib_eez,eez_harvest)
reg_map_plot(carib_eez,eez_harvest)

# Plot variable cost layers
names(econ_stack) <- c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez")
tites <- c("Fuel price $US/gallon","Miniumum wage ($US)", "Permit fee $US","Risk score","Distance from shore (m)","Depth charge (%)","Distance charge(%)","EEZ")

var_costs <- tm_shape(carib_eez,is.master=TRUE)+
   tm_fill(col="lightblue",alpha = 0.4) +
   tm_borders(lwd = 1.2) +
   tm_shape(econ_stack) +
   tm_raster(c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez"),title=tites) #,style="cont") +
   tm_legend(position = c("right","top"),scale=1)
# 
# # Save variable costs map
# save_tmap(var_costs, paste0(fig_folder,"Variable costs.png"), width=1920, height=1080)
# 
# # Plot profits
 profits <- tm_shape(carib_eez,is.master = TRUE)+
            tm_borders(lwd = 1.2) +
            tm_shape(test)+
            tm_raster(legend.show = TRUE,title="10 year Profit($USD)") +
            tm_legend(position = c("right","top"),scale=1) 
# 
# # Save profits map
# save_tmap(profits, paste0(fig_folder,"NPV.png"), width=1920, height=1080)
# on = c("right","top"))
#                    
#                  
#                   
# 
# luc_arc_suitable.fig<- tm_shape(LUC_suit) +
#                           tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
#                        tm_shape(LUC,is.master = TRUE) +
#                           tm_fill(col="lightblue",alpha = 0.4,title ="TESt") +
#                           tm_borders(lwd = 1.2) +
#                           tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"),main.title = "Lucayan Archipelago")
# 
# 
# 
# lee_ant_suitable.fig<- tm_shape(LEE_suit) +
#                           tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
#                        tm_shape(LEE,is.master = TRUE) +
#                           tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
#                           tm_borders(lwd = 1.2) +
#                           tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"), main.title = "Leeward Antilles")
# 
# 
# less_ant_suitable.fig<- tm_shape(LS_suit) +
#                           tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
#                        tm_shape(LS,is.master = TRUE) +
#                           tm_fill(col="lightblue",alpha = 0.4,title = "Leeward Antilles") +
#                           tm_borders(lwd = 1.2) +
#                           tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"), main.title = "Lesser Antilles")
# 

#all_plots<- tmap_arrange(g_antilles_fig, luc_arc_suitable.fig,lee_ant_suitable.fig,less_ant_suitable.fig