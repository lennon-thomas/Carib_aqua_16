##To determine number of individuals (fish) per farm as input for the TPC model

boxdir<-('/Users/Lennon/Documents/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')

#Volume per cage
cage_vol<-6400

#Totalcages
num_cage<-16

#Totalfarmvolume
total_vol<-cage_vol*num_cage

#Stockdensity (indivduals per m^3^ based on Benetti et al. 2010)
st_density<-2.4

#average intial weight in grams
int_weight<-3

#Stocking number in one cage at one cage (individuals)
fish_cage<-cage_vol*st_density

#Total stocking number for all cages in a farm
farm_fish_cage<-cage_vol*st_density*num_cage

#inital stocking density in units of weight
st_density_wt<-int_weight*st_density

#average weight at harvest in grams


# average density at harvest  (g/m^3^)

hv_density_wt<-5000

#Total of fish per farm at stocking density
total_fish<-total_vol*st_density

#Bahamas cage volume
bene_vol<-2700
bene_final_density<-5000 #g/m3
harv_weight<-6066

total_harvest_wt<-bene_vol*bene_final_density
no_indiv_final<-total_harvest_wt/harv_weight
hv_density_ind<-no_indiv_final/bene_vol

#calculate rate of decline
decline<-((hv_density_ind/st_density)^(1/12))-1

#Months and number of cages stocked per month
no_months<-12
cage_per_month<-as.vector(c(2,3,5,6,8,9,10,11,12,14,15,16)


#find number of total indiviudals per farm in each month
no_indiv_month<-as.vector(rep(NA,12))

for (i in 1:no_months){
  no_indiv_month[i]<-cage_vol*st_density*cage_per_month[i]
}
  
#bring in raster of study area
carib<-raster(paste(boxdir,"Suitability/tmp/suitable_eez.tif",sep=""))
plot(carib)

#Dane's simple equation just to check numbers.
int_max_stock_density<-total_vol*st_density

carib[carib==1]<-int_max_stock_density
writeRaster(carib,paste(boxdir,"int_individuals_per_farm.tif"))
