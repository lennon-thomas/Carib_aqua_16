
### Load packages --------------------------------------------------
library(rgdal)
library(raster)
library(rgeos)
library(ncdf4)
### **TEMPORARY** Set directory to location of sst raster files. Should probably put all these files on Google Drive to standardize the code 
#setwd('/Users/Tyler/Desktop/GitHub/Caribbean-Aquaculture/carib-aqua-sst')

#Lennon's wd
boxdir<-"/Users/Lennon/Documents/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/"
#setwd("C:/Users/Lennon Thomas/Dropbox/Caribbean Aquaculture/Data/SST")
### Interpolate raster -----------------------------------------

# Get all file names in the interpolated sst directory
sstfiles<-list.files(paste(boxdir,'sst/cropped',sep=""),full.names = T)


# Store all  interpolated sst files in lists
sstlist<-lapply(sstfiles,raster)

#import land mask layer
land<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))
plot(land)
outputnames <- sstfiles

for(i in seq_along(sstlist)){mask(sstlist[[i]],land,file=outputnames[i],sep="",
                                  inverse=FALSE, maskvale=NA,overwrite=TRUE)}

depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep = "")) #This file has a 1km resolution
ext<- c(-87.29167, -57.04167, 7.375, 30.20833) 
extent(depth)<-ext


for(i in seq_along(sstlist)){resample(sstlist[[i]],depth,file=outputnames[i],sep="",
                                  inverse=FALSE, maskvale=NA,overwrite=TRUE)}
plot(sst_final[[95]])
 names(sst_final)                            
sst_final<-stack(sstlist)                          
plot(sst_final)
# Write netCDF files
writeRaster(sst,"2007_2016 monthly SST.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)

