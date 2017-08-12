## Some examples of how to work with raster stacks

econ_stack[["fuel_price"]][1:100] # example of how to subset cells of layer 

m1<-cellStats(prod[[1]],"max") #apply a function to a layer

test<-calc(prod,function(x)x *depth)   #econ_stack[["permit_fee"]]) ## apply functions to an entire raster stack

# Demo function for raster math
test_function <- function(rast_1, rast_2) {
  old_values1 <- getValues(rast_1) # get values for first raster
  old_values2 <- getValues(rast_2) # get values for second raster
  new_values <- old_values1 * old_values2 # do raster math
  new_rast <- setValues(rast_1, new_values) # update values for first raster and return as new raster. !! Make sure this is not overwriting values of rast_1 file but actually creating a new raster
  return(new_rast)
}

test <- test_function(rast_1 = prod[[1]], rast_2 = fuel_price)

m2<-cellStats(test[[1]],"max") #compare with m1 (should be x100 greater)



cells <- Which(prod[[1]] > 0, cells = TRUE) 

ex<-1315011

one<-prod[[1]][ex]
two<-prod[[2]][ex]
twelve<-prod[[3]][ex]

total_vol/test

initial_stock


int_weight<-256000*3

test<-int_weight+prod[[1]][ex]+prod[[2]][ex]+prod[[3]][ex]+prod[[4]][ex]+ prod[[5]][ex]+prod[[6]][ex]+prod[[7]][ex]+prod[[8]][ex]+prod[[9]][ex]+prod[[10]][ex]+prod[[11]][ex]+prod[[12]][ex]


## Wave exposure layer (not really sure if/how to incorporate this layer yet for increased mainentance costs on exposed areas)

waves<-raster(paste(boxdir,"economic/data/PECS/PECS_exposure.tif",sep=""))
hurricanes<-raster(paste(boxdir,"economic/data/PECS/PECS_hurricanes.tif",sep=""))
carib_waves<-crop(waves,depth)
carib_waves<-resample(waves,depth)
carib_waves<-mask(carib_waves,shore_distance)


normal_waves<-10^log_waves

# Test by multiplying permit values times production quantities (meaningless, can delete)
test2 <- prod_values * permit_values



today <- Sys.Date()
format(today, format="X%Y.%m")
"June 20 2007"


