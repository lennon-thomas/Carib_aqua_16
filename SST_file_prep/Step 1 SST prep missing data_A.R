
### Load packages --------------------------------------------------
library(rgdal)
library(raster)
library(rgeos)

### **TEMPORARY** Set directory to location of sst raster files. Should probably put all these files on Google Drive to standardize the code 
#setwd('/Users/Tyler/Desktop/GitHub/Caribbean-Aquaculture/carib-aqua-sst')

#Lennon's wd
setwd("Y:/Documents/Work for waitt/WI 2016/Caribbean-Aquaculture")

### Interpolate raster -----------------------------------------

# Get all file names in the sst directory
sstfiles<-list.files('sst_data/',full.names = T)




# Store all sst in lists
sstlist<-lapply(sstfiles,raster)
plot(sstlist[83])

# Stack rasters
sst<-stack(sstlist)
names(sst)<-sstfiles


# Write netCDF files
writeRaster(sst,"2005_2014 monthly SST.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)

# Replace values greater than 45 (no data) with NA
sst[sst[]>45]<-NA

# Resave all raster layers using the same file name 

s <- unstack(sst)
outputnames <- sstfiles
for(i in seq_along(s)){writeRaster(s[[i]], file=outputnames[i],overwrite=TRUE)}

## Next step is to create the int files using python

