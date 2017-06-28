
#updated 6/8/17

### Load packages --------------------------------------------------

rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(ncdf4)


#boxdir<-('/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/')
boxdir<-('/Users/Lennon/Documents/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/sst')



### Load data --------------------------------------------------

sstfiles<-list.files(paste(boxdir,'/cropped',sep = ""),full.names = T)

sstlist<-lapply(sstfiles,raster)


### Data scaling info (from raw metadata)--------------
for(i in seq_along(sstlist)){sstlist[[i]][sstlist[[i]]==-32767]<-NA}


for(i in seq_along(sstlist)){sstlist[[i]]<-sstlist[[i]]*0.005}

sstlist<-sstlist*0.005





### Stack rasters and write netCDF file--------------------------

sst<-stack(sstlist)

names(sst)<-sstfiles

writeRaster(sst,paste(boxdir,"2005_2014 monthly SST.NetCDF",sep=""),format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)



# Resave all raster layers using the same file name 

s <- unstack(sst)

outputnames <- sstfiles

for(i in seq_along(s)){writeRaster(s[[i]], file=outputnames[i],overwrite=TRUE)}

## Next step is to create the int files using python

