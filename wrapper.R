#8/10/17

# Wrapper for bioeconmic model


# Load packages and functions-----------------------------------------------------------

rm(list = ls())

library(raster)
library(rgdal)
library(tmap)
library(dplyr)
library(ncdf4)
library(stringr)
library(broom)
library(fasterize)
library(sf)
library(demons)
library(tidyr)
library(parallel)



load_functions(func_dir = 'functions')

# Run settings -------------------------------------------------------------

boxdir<-('/Users/Lennon/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

run_name = "test"

run_dir<-paste(boxdir,'results/',run_name, "/" ,sep = "")

if (dir.exists(run_dir) == F) {
  dir.create(run_dir, recursive = T)
} else {
  
  print('Folder already exists')
}

prep_data = TRUE# Prep economic data files (TRUE) or just read in existing files (FALSE)
#fix_int_stock =FALSE #should the number of fingerlings used to stock each farm be fixed? false means they will be calculated to reach a stock density = havest density

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
  
} else {


load(paste(run_dir, 'economic_data.Rdata',sep=""))

}


# Parameters --------------------------------------------------------------

# Constant parameters

cage_cost <- 269701 # US$ cage and installation Lipton and Kim. For 3000 m^3 cages and this includes all the gear (anchors, etc) 
support_vessel <- 158331 #US$ Bezerra et al. 2016: 16-m-long boat with a 6-cylinder motor and a hydraulic winch  #50000 # US$ 32'ft from Kam et al. 2003
site_lease<-3265 # US$ from Bezerra et al. 2016 This is for 16 ha farm in Brazil (ours is larger so may want to increase)
labor_installation<-52563 # US$ from Bezerra et al. 2016
site_hours <- 240 # monthly hours per worker per month (8*30)
site_workers <- 17 # Bezerra et al. 2016 
avg_boat_spd <- 48.28 #km/hour assumes about 30 miles per hour. Check distance raster to make sure units are correct
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
#no_fingerlings <- 256000 # fingerlings per farm
price_fingerlings <- 2.58 #$US/fingerling Bezerra et al. 2016
harv_den<-15 #kg/m^3 harvest density Benetti et al. 2010 and Dane's industry contacts
no_cage<-16 # cages
cage_volume<-6400 #m^3
total_vol<-no_cage*cage_volume # m^3 total cage volume
harvest_weight<-5 # kg from various souces (5-6 kg)
cobia_price<- 8.62 $US/kg# Bezerra et al. 2016
#F.C.R.  = Feed given / Animal weight gain.
fcr<-1.75 #Benetti et al. 2010 for the whole Carib region 
feed_price<-1.64 #in units of $/kg  Bezerra et al. 2016
survival<-0.75 #Benetti et al. 2007 over 12 monthes and Huang et al. 2011
month_mort<-1 - (1 - (1-survival) ^ (1 / 12)
int_weight<-0.015 # kg (15 grams) Bezerra et al. 2016
no_trips<-2 #number of trips to farm per day
sim_length <- 120 # length of simulation (months)
                 
# Calculate average growth, cycle length, and no. of fingerlings --------

annual_prod<-ann_prod(growth = growth) 

stocking_n<-annual_prod[[1]]

harvest_cycles<-annual_prod[[2]]

harvest_cycle_length<-annual_prod[[3]]

avg_month_growth<-avg_growth(growth = growth)

# Save avg monthly growth and initial stocking rasters
writeRaster(avg_month_growth, paste0(boxdir,'data/avg_month_growth_stack.nc',sep = ""), format = "CDF", overwrite =TRUE)
writeRaster(stocking_n, paste0(boxdir,'data/initial_stocking_stack.nc',sep = ""), format = "CDF", overwrite =TRUE)


# Run Projection ----------------------------------------------------------

sim_results <- sim_aqua(avg_month_growth = avg_month_growth, stocking_n = stocking_n, int_weight = int_weight, sim_length = sim_length, month_mort = month_mort, fcr = fcr)

#sim_results<-read.csv(paste0(run_dir,"sim_function_results.csv"))

# Calculate costs ---------------------------------------------------------

# Start-up costs
cap_costs<-capital_costs(cage_cost,support_vessel,econ_stack[["depth_charge"]],econ_stack[['distance_charge']],site_lease,labor_installation)

# Operating costs
operate_costs <- operating_costs(econ = econ_stack, prod = total_ann_production,site_workers, site_hours, avg_boat_spd,
                                 fuel_eff, site_days, no_fingerlings, price_fingerlings, feed_cost, fcr)

# Total costs for one year
total_cost_yr_one <- (cap_costs + operate_costs[[1]])

writeRaster(total_cost,paste0(run_dir,"total_cost.tif"),overwrite=TRUE)

# Calculate annual profit -------------------------------------------------

revenue_results <- revenue(total_ann_production[[1]], cobia_price)
writeRaster(revenue_results,paste0(run_dir,'revenue_raster.tif'))
revenue_results<-brick(paste0(run_dir,'revenue_raster.tif'))
profit<-revenue_results - total_cost_yr_one
writeRaster(profit,paste0(run_dir,"profit.tif"))
#profit <- calc(revenue_results, fun=function(x) x - total_cost,filename = paste0(run_dir,'profit_raster.tif'),overwrite=TRUE)

writeRaster(profit,paste(run_dir,"profit_test.tif",sep = ""))

profit<-raster(paste(run_dir,"profit_test.tif",sep = ""))

cells <- Which(prod[[1]] > 0, cells = TRUE) 
test<-length(cells)
c<-Which(profit[[1]] >0,cells = TRUE)
t<-length(c)
# Calculate 10 year NPV ---------------------------------------------------





# Process results ---------------------------------------------------------
# costs

perc_capital<-cap_costs/total_cost_yr_one*100

perc_feed<-feed_cost[[1]]/total_cost_yr_one*100

perc_fingerlings<-cost_fingerlings[[1]]/total_cost_yr_one*100

perc_labor<-(labor[[1]]+travel[[1]])/total_cost_yr_one*100

perc_capital[1315011]
perc_feed[1315011]
perc_fingerlings[1315011]
perc_labor[1315011]

carib_eez<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep = ""))

eez_capital<-zonal(perc_capital,carib_eez,fun = 'mean', na.rm = TRUE)

eez_feed<-as.data.frame(zonal(perc_feed,carib_eez,fun = 'mean', na.rm = TRUE))

eez_fingerlings<-as.data.frame(zonal(perc_fingerlings,carib_eez,fun = 'mean', na.rm = TRUE))

eez_labor<-as.data.frame(zonal(perc_labor,carib_eez,fun = 'mean', na.rm = TRUE))

cost_results<-as.data.frame(cbind(eez_capital,eez_feed[,2],eez_fingerlings[,2],eez_labor[,2]))

colnames(cost_results)<-c("eez","eez_capital","eez_feed","eez_fingerlings","eez_labor")

cost_results<-  gather(cost_results,'cost','value',dplyr::contains('_')) 

ggplot(cost_results,aes(x=eez,y=value,fill=cost)) +
  geom_bar(stat="identity")+
  xlab("EEZ") +
  ylab("% of total cost")

#revenue

eez_revenue<-zonal(revenue_results,carib_eez,fun="sum",na.rm = TRUE)



#plot
carib_eez<-readOGR(dsn=paste(boxdir, "Suitability/tmp",sep = ""),layer = "carib_eez_shape",)

tm_shape(carib_eez)+
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(lwd = 1.2) +
tm_shape(profit)+
  tm_raster( legend.show = TRUE,title="Annual Profit($USD)",style="cont",palette=c("red","blue")) +
  tm_legend(position = c("right","top"),scale=1) 


