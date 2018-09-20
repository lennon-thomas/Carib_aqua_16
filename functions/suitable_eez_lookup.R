##################################################################
## Function purpose: Create lookup table of suitable cells and EEZ
## Date: 2018-09-20
## Author: Tyler Clavelle
##################################################################

# Load EEZ shapefile for plotting
EEZ <- readOGR(dsn=paste(boxdir,"Suitability/tmp",sep = ""),layer="carib_eez_shape") 

# Load suitablity results
s_areas <- gzfile(paste0(boxdir,"results/Suitability/suitable_areas.rds"))
suit_areas <- readRDS(s_areas)

# Prep data for plotting --------------------------------------------------
tidy_eez <- tidy(EEZ)

temp_df <- data.frame(EEZ@data)
temp_df$id <- seq(0,nrow(temp_df)-1)

EEZ_df <- merge(tidy_eez, temp_df, by="id")

cells <- as.vector(Which(suit_areas > 0, cells =TRUE))

raster_coords <- as_data_frame(rasterToPoints(suit_areas))

suit_coords <- cbind(cells,raster_coords) %>%
  set_names(c("cell","long","lat","suitable"))