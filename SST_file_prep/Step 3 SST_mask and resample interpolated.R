
### Load packages --------------------------------------------------


rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(ncdf4)
### **TEMPORARY** Set directory to location of sst raster files. Should probably put all these files on Google Drive to standardize the code 
#setwd('/Users/Tyler/Desktop/GitHub/Caribbean-Aquaculture/carib-aqua-sst')

#Lennon's wd
#boxdir<-"/Users/Lennon/Documents/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/"
boxdir<-("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/")
### Interpolate raster -----------------------------------------

# Get all file names in the interpolated sst directory
sstfiles<-list.files(paste(boxdir,'sst /cropped',sep=""),full.names = T)
#sstfiles<-list.files('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/sst /cropped')

# Store all  interpolated sst files in lists
sstlist<-lapply(sstfiles,raster)

#import land mask layer
land<-raster(paste(boxdir,"Suitability/tmp/carib_eez_raster.tif",sep=""))


outputnames <- sstfiles

#resample to 1 km

depth<-raster(paste(boxdir,"Suitability/tmp/carib_depth.tif",sep = "")) #This file has a 1km resolution

for(i in seq_along(sstlist)){resample(sstlist[[i]],depth,file=outputnames[i],sep="",
                                      inverse=FALSE, maskvale=NA,overwrite=TRUE)}

#mask with eez study area file

for(i in seq_along(sstlist)){mask(sstlist[[i]],land,file=outputnames[i],sep="",
                                  inverse=FALSE, maskvale=NA,overwrite=TRUE)}


#stack all layers to create final file with dates

st <- as.Date("2007-01-01")
en <- as.Date("2016-12-01")
filenames <- as.character(seq(st, en, by = "1 month"))

filenames<-substr(filenames,1,nchar(filenames)-3)


sst_final<-stack(sstlist) 

names(sst_final)<-filenames
plot(sst_final[[34]])
# Write netCDF files
writeRaster(sst_final,"2007_2016_monthly_SST.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)
sst<-nc_open("2007_2016_monthly_SST.nc")
test<-raster("2007_2016_monthly_SST.nc")
for(i in 1:120){plot(sst_final[[i]],main=names(sst_final[[i]]))}
ext(sst)
