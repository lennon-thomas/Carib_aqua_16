library (raster)
library(stringr)
library(animation)
setwd("H:/Documents/Work for waitt/WI 2015/MSP and aquaculture/Monthly modis sst data/Final 1km data to be stacked")
All<-list.files(pattern='*.tif')

Alls<-str_split(All[],pattern = '_')
#Alls = unlist(strsplit(All[], split='_', fixed=TRUE))[1:2]
#Alls <- sapply(strsplit(All[], split='_', fixed=TRUE), function(x) (x[1:2]))

dates<-matrix(unlist(Alls), ncol=3, byrow=TRUE)

df<-data.frame(All)

df$month<-NA
df$year<-NA
df$filename<-NA
df$month<-as.numeric(dates[,1])
df$year<-as.numeric(dates[,2])
str(as.character(filelist))


df$filename <- paste(df$month, df$year, sep='_') 
df<-df[with(df, order(year,month)), ]

filelist<-as.character(df[,1])
layername<-as.character(df[,4])
all_monthly_sst<-stack(filelist)
nlayers(all_monthly_sst)

names(all_monthly_sst)<-layername
writeRaster(all_monthly_sst,"2005_2014 monthly SST.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)

plot(all_monthly_sst)



im.convert("all_monthly_sst", output = "SST.gif",ani.options(convert = 'c:/program files/imagemagick/convert.exe'))
## use GraphicsMagick
gm.convert("all_monthly_sst", output = "bm-animation2.gif")
ani.options(convert = 'c:/program files/imagemagick/convert.exe')

saveGIF(
  print(spplot(all_monthly_sst, layout=c(1, 1))),
  height = 500, width = 350, interval = .3, o
