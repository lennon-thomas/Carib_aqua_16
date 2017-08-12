#8/11/17
#This file calculates annual productiona and estimates the number of fingerlings per cell




# Function to calculate annual production
ann_prod<-function(production,i) {                  
  prod1<-getValues(prod)
  new<-setValues(prod,prod1)
  annual_prod<-stackApply(new,indices,fun=sum,filename=paste(run_dir,'annual_prod.nc')) # calculate the sum of production for each year

  return(annual_prod)
}
# Function to calculate the number of individual fingerlings needed to stock each farm, each year

calc_initial_stock<-function(harvest_density,production,stock_weight,total_vol) {  
                 
   init_stock<-(harvest_density-(annual_prod/total_vol))
                 
    return(init_stock)
}
 
# Function to calculate the total production value each year

calc_annual_prod<- function(init_stock,production,int_weight){
  
  total_production<-(init_stock*int_weight) + production
  
  return(total_ann_production)
}