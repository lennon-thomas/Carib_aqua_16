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

prep_data = FALSE # Prep economic data files (TRUE) or just read in existing files (FALSE)

# Load Data ---------------------------------------------------------------

if (prep_data == TRUE){
 
  # Run econ data prep function
   econ <- econ_data_prep()
   # extract rasters
   prod <- econ[['prod']]
   growth <- econ[['growth']]
   econ_stack <- econ[['econ_stack']]
   fixed_cost <- econ[['fixed_cost']]
   # remove list
   rm(econ)
  
} else {

load(paste(run_dir, 'economic_data.Rdata',sep=""))

}


# Parameters --------------------------------------------------------------

# Constant parameters
cage_cost <- 5708800 # cage and installation
support_vessel <- 50000 #32'ft from Kam et al. 2003
site_hours <- 8 # hours per worker per day
site_workers <- 4 # number of workers per farm per day
site_days <- 260 #number of days workers are on a farm per year(5 days a week)
avg_boat_spd <- 48280.3 # average boat speed (meters per hour)~30 miles per hour
fuel_eff <- 3219 # average fuel efficiency (meters per gallon)~2 miles per gallon
no_fingerlings <- 1500 # fingerlings per farm
cost_fingerlings <- 0.31 # cost per fingerling (Sclodnick 2013)
feed_cost <- 1.8 # cost per kg of feed (Sclodnick 2013)
stock_weight<-.003  #kg of inividuals when cage is stocked
harv_den<-15 #kg/m^3 harvest density
total_vol<-16*6400 #total cage volume
harvest_weight<-6
cobia_price<- 3000# estimate from cobia_price.pdf...can we get this from supply/demand curve? 

# Calculate annual production ---------------------------------------------

#index<-format(as.Date(layernames,format = "X%Y.%m"),format = "%Y") # extract the year from each layer

yeardex<-rep(1:10,each=12)

annual_prod<-ann_prod(prod,yeardex) # Get sum of annual production from TPC model
annual_prod<-brick(paste(run_dir,"annual_prod.tif",sep=""))
annual_prod[annual_prod==0]<-NA

init_stock<-calc_initial_stock(harv_den,annual_prod,stock_weight,total_vol) # Determine no of fingerlings to reach a harvest density of 15 kg/m^3

int=(init_stock*stock_weight)     

totalp<-annual_prod

totalp[totalp>0]<-NA

for (i in 1:nlayers(int)){

temp<- sum(annual_prod[[i]],int[[i]])

totalp[[i]]<-temp

print(i)
}
h_density<-totalp/total_vol
  
# Calculate costs ---------------------------------------------------------

# Start-up costs
cap_costs<-capital_costs(fixed_cost[1,2],econ_stack[["depth_charge"]],econ_stack[['distance_charge']])

# Operating costs
operate_costs <- operating_costs(econ = econ_stack, prod = prod)

# Total costs
total_cost <- cap_costs + operate_costs



# Calculate annual profit -------------------------------------------------


revenue <- lapply(prod, function(x) x * cobia_price)

profit <- revenue - total_cost

writeRaster(profit,paste(run_dir,"profit_test.tif",sep = ""))

profit<-raster(paste(run_dir,"profit_test.tif",sep = ""))



# Calculate 10 year NPV ---------------------------------------------------








#plot
carib_eez<-readOGR(dsn=paste(boxdir, "Suitability/tmp",sep = ""),layer = "carib_eez_shape",)

tm_shape(carib_eez)+
  tm_fill(col="lightblue",alpha = 0.4) +
  tm_borders(lwd = 1.2) +
tm_shape(profit)+
  tm_raster( legend.show = TRUE,title="Annual Profit($USD)",style="cont",palette=c("red","blue")) +
  tm_legend(position = c("right","top"),scale=1) 


