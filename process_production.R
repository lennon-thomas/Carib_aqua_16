#8/11/17
#This file calculates annual productiona and estimates the number of fingerlings per cell




# Function to calculate annual production
ann_prod<-function(production,i) {                  

  annual_prod<-stackApply(prod,yeardex,fun=sum, na.rm = TRUE,filename=paste(run_dir,"annual_prod.tif",sep=""),overwrite=TRUE) # calculate the sum of production for each year

  return(annual_prod)
}
# Function to calculate the number of individual fingerlings needed to stock each farm, each year

calc_initial_stock<-function(harvest_density,production,stock_weight,total_vol) {  
                 
   init_stock<-(harvest_density*total_vol-production)/stock_weight
   
   writeRaster(init_stock,paste(run_dir,"yearone_fingerlings.tif",sep = ""),overwrite=TRUE)
                 
    return(init_stock)
}
 
# Function to calculate the total production value each year

# calc_total_prod<- function(init_stock,production,int_weight){
#   
#   total_ann_production<-(init_stock*int_weight) + production
#   
#   writeRaster(total_ann_production,paste(run_dir,"total_annual_production.tif",sep = ""),overwrite=TRUE)
# 
#     return(total_ann_production)
# }
