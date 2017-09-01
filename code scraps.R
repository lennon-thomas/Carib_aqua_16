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



cells <- Which(annual_prod[[1]] > 0, cells = TRUE) 

cellex1<-prod[[1]][1315011]

cellex2<-prod[[2]][1315011]

cellex12<-prod[[12]][1315011]
cellex2/cellex3

g<-growth[[12]][1315011]
test<-((two*0.003)+annual_prod[[1]][1315011])/total_vol

this<-(harv_den*total_vol-test)

two<-this/0.003

sum(prod[[1:12]][1315011])

yearone<-1926*0.003

(q/12)/(1-q/12)

one<-prod[[1]][1315011]
two<-prod[[2]][1315011]
three<-prod[[3]][1315011]
four<-prod[[4]][1315011]
five<-prod[[5]][1315011]
six<-prod[[6]][1315011]
seven<-prod[[7]][1315011]
eight<-prod[[8]][1315011]
nine<-prod[[9]][1315011]
ten<-prod[[10]][1315011]
ele<-prod[[11]][1315011]
twel<-prod[[12]][1315011]


yearone<-sum(one,two,three,four,five,six,seven,eight,nine,ten,ele,twel)
y_0<-annual_prod[[1]][1315011]

zero<-256000*0.15

test<-yearone+zero

cc<-cap_costs[1315011]
op<-operate_costs[[1]][1315011]
test2<-cc+op
total_cost[1315011]

revenue<-test*4000
revenue_results[[1]][1315011]

final<-revenue-test2
profit[[1]][1315011]

total_ann_production[[1]][1315011]
test<-sum(prod[[1:12]])
test2<-annual_prod[[1]]
identical(test,test2)


int_weight<-256000*.15

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

survival<-0.75
mort<-0.25
month_mort<-1 - (1 - mort) ^ (1 / 12)
one<-100-(100*month_mort)
two<-one-(one*month_mort)
three<-two-(two*month_mort)
four<-three-(three*month_mort)
five<-four-(four*month_mort)
six<-five-(five*month_mort)
seven<-six-(six*month_mort)
eight<-seven-(seven*month_mort)
nine<-eight-(eight*month_mort)
ten<-nine-(nine*month_mort)
eleven<-ten-(ten*month_mort)
twelve<-eleven-(eleven*month_mort)
thirth<-twelve-(twelve*month_mort)
#annual_prod<-stackApply(production,i,fun=sum, na.rm = TRUE,filename=paste(run_dir,"annual_prod.tif",sep=""),overwrite=TRUE) # calculate the sum of production for each year