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


load_functions(func_dir = 'functions')

# Run settings -------------------------------------------------------------

boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

run_name = "test"

run_dir<-paste(boxdir,'results/',run_name, "/" ,sep = "")

if (dir.exists(run_dir) == F) {
  dir.create(run_dir, recursive = T)
} else {
  
  print('Folder already exists')
}

prep_data = FALSE# Prep economic data files (TRUE) or just read in existing files (FALSE)
fix_int_stock =FALSE #should the number of fingerlings used to stock each farm be fixed? false means they will be calculated to reach a stock density = havest density

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

#growth<-brick(paste(boxdir,"economic/data/final/cobia_prod.nc",sep = ""))

#prod<-brick(paste(boxdir,"economic/data/final/cobia_prod.nc",sep = ""))

#econ_stack<-raster(paste(boxdir,'economic/data/final/econ_stack.nc',sep = ""))

load(paste(run_dir, 'economic_data.Rdata',sep=""))

}


# Parameters --------------------------------------------------------------

# Constant parameters

cage_cost <- 5708800 # cage and installation
support_vessel <- 50000 #32'ft from Kam et al. 2003
site_lease<-3265 # from Bezerra et al. 2015
labor_installation<-52563 # from Bezerra et al. 2015
site_hours <- 8 # hours per worker per day
site_workers <- 4 # number of workers per farm per day
site_days <- 260 #number of days workers are on a farm per year(5 days a week)
avg_boat_spd <- 48280.3 # average boat speed (meters per hour)~30 miles per hour
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
no_fingerlings <- 256000 # fingerlings per farm
price_fingerlings <- 17.3 # cost per kg of fingerling (average from Huang et al. 2011)
#feed_cost <- 1.8 # cost per kg of feed (Sclodnick 2013)
stock_weight<-0.15  #kg of inividuals when cage is stocked
harv_den<-15 #kg/m^3 harvest density
total_vol<-16*6400 #total cage volume
harvest_weight<-5 # from various souces (5-6 kg)
cobia_price<- 8.62 # Bezerra et al. 2016
#F.C.R.  = Feed given / Animal weight gain.
fcr<-2 #Bezerra et al. 2016 (feed conversion ratio)
feed_price<-1.64 #in units of $/kg  Bezerra et al. 2016
survival<-0.75 #Benetti et al. 2007 over 12 monthes and Huang et al. 2011
month_mort<-1 - (1 - (1-survival)) ^ (1 / 12)
#no_trips<-2 #number of trips to farm per day
# Calculate annual production ---------------------------------------------

#index<-format(as.Date(layernames,format = "X%Y.%m"),format = "%Y") # extract the year from each layer

yeardex<-rep(1:10,each=12)

annual_prod_test<-ann_prod(growth = growth) #,yeardex) # Get sum of annual production from TPC model. NOTE: function doesn't seem to be working when process data is false.. may need to get values in that case

#annual_prod<-brick(paste(run_dir,"annual_prod.tif",sep=""))

annual_prod[annual_prod==0]<-NA

# determine inital # of fingerlings stocked at each farm each year 

if (fix_int_stock == TRUE) {
  
  initial_stock<-raster(paste(boxdir,"TPC/ initial_stock.tif",sep = ""))   #fixed number for each year

  
} else {

  initial_stock<-calc_initial_stock(harv_den,annual_prod,stock_weight,total_vol) # Determine no of fingerlings to stock at each farm each year to reach a harvest density of 15 kg/m^3
}

# Multiply number of individuals by indiviudal fingerling weight

inital_weight<- initial_stock * stock_weight

#Calculate total annual production

total_ann_production<-inital_weight + annual_prod


h_density<-total_ann_production/total_vol #This should be in the 5-15 range
  
# Calculate costs ---------------------------------------------------------

#annual feed costs

feed_cost<-feed_cost_est(prod,inital_weight,feed_price,yeardex,fcr)


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


