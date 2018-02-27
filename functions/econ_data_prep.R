# 18/10/17

# Economic data prep
econ_data_prep <- function() {
  
# Read in raw data --------------------------------------------------------

prod<-brick(paste(boxdir,"data/TPC/Carib_cobia_suitable.nc",sep=""),varname="Prod") 

growth<-brick(paste(boxdir,"data/TPC/Carib_cobia_suitable.nc",sep=""),varname="Grow")

initial_stock<-raster(paste(boxdir,"data/TPC/ initial_stock.tif",sep = ""))

eez_shape<-readOGR(dsn = paste(boxdir,"data/economic_prep/",sep=""),layer = "carib_eez_shape")

econ_params<-read.csv(paste(boxdir,"data/economic_prep/eez_parameters.csv",sep=""))

depth<-raster(paste(boxdir,"data/economic_prep/carib_depth.tif",sep=""))

shore_distance<-raster(paste(boxdir,"data/economic_prep/shore_distanc.tif",sep=""))

# Format Data-------------------------------------------------------------

st <- as.Date("2007-01-01")

en <- as.Date("2016-12-01")

layernames <- as.Date(as.character(seq(st, en, by = "1 month"))) %>%
   as.Date( str_sub(1,7))

names(prod)<-layernames
names(growth)<-layernames

crs(prod) <- crs(shore_distance)
crs(growth) <- crs(shore_distance)
extent(prod) <- extent(shore_distance)
extent(growth) <- extent(shore_distance)
names(growth)<-layernames

econ_params$Territory1 <- gsub('Curacao','Curaçao', econ_params$Territory1)
econ_params$Territory1 <- gsub('Cura<ed>_ao','Curaçao', econ_params$Territory1)

econ_params$Territory1 <- gsub('Saint-Barth_lemy', 'Saint-Barthélemy', econ_params$Territory1)

# Create stacked spatial economic data layers ---------------------------------------------

econ_shape<-merge(eez_shape,econ_params,by="Territory1",all=TRUE)

#econ_shape<-st_as_sf(econ_shape) #Convert to sf object for fasterize (actually, don't use fasterize it messes resolution up)

fuel_price<-rasterize(econ_shape,depth, field = 'fuel_price')

min_wage<-rasterize(econ_shape,depth,field = "min_wage")

permit_fee<-rasterize(econ_shape,depth, field="permit_fee")

risk_score<-rasterize(econ_shape,depth, field="risk")

depth_charge<-calc(depth, function(x) ifelse(x > 50, 0.10, 0))

distance_charge<-calc(shore_distance, function(x) ifelse(x > 46300, 0.10,0))

# Create raster layer of cell numbers

cell_nos<-rasterFromCells(fuel_price, 1:length(fuel_price), values=TRUE)

cell_nos<-mask(cell_nos,fuel_price)

# Create raster of eez id numbers

eez_shape$MRGID<-{as.numeric(levels(eez_shape$MRGID))[eez_shape$MRGID]}

eez<-rasterize(eez_shape,depth,field = 'MRGID')

econ_stack<-stack(fuel_price,min_wage,permit_fee,risk_score,shore_distance,depth_charge, distance_charge,eez,cell_nos)

econ_names<-c("fuel_price","min_wage","permit_fee","risk_score","shore_distance","depth_charge","distance_charge","eez","cell_no")

names(econ_stack)<-econ_names

# Write files -------------------------------------------------------------

#final files

writeRaster(prod,paste(boxdir,"data/economic_final/cobia_prod.nc",sep = ""),format = "CDF",varname = "prod", overwrite =TRUE)

writeRaster(growth,paste(boxdir,"data/economic_final/cobia_growth.nc",sep = ""),format = "CDF",varname= "growth",overwrite =TRUE)

writeRaster(econ_stack,paste(boxdir,'data/economic_final/econ_stack.nc',sep = ""), format = "CDF",varname="econ", overwrite =TRUE)

# Read files back in (to save space) and save workspace image---------------------------------------

file.names <- list.files(path = paste(boxdir,"data/economic_final/", sep = ""), pattern = ".nc")

model_files <- lapply(paste(boxdir,"data/economic_final/",file.names, sep = ""),brick)

growth <- model_files[[1]]

prod <- model_files[[2]]

econ_stack<-model_files[[3]]

# This saves economic_data to the results folder of the run.. the actual economic data files are saved in the data folder of main directory (may want to change if these economic parameters end up varying)
save(list = c('growth', 'prod', 'econ_stack'), file = paste(run_dir, "Data/economic_data.Rdata",sep = ""))

return(list('growth' = growth, 'prod' = prod, 'econ_stack' = econ_stack))
}



