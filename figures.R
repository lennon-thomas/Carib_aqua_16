library(ggplot2)
library(reshape2)
ggplot(melt(annual_prod$stocking_n))+
    geom_histogram(position="dodge") 

# the distribution of values in the raster
hist(annual_prod$stocking_n, main="No. of fingerlings for stocking", 
     col= "purple", 
     maxpixels=9150080)

hist(annual_prod$harvest_cycles, main="No. of harvest cycles", 
     col= "purple", 
     maxpixels=9150080)


hist(annual_prod$harvest_cycle_length, main="No. of harvest cycles", 
     col= "purple", 
     maxpixels=9150080)

hist(sdev_growth[[4]],main="April",
     maxpixels=9150080)
