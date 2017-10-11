#6/22/17

1.	Download mapped, monthly MODIST 4 km SST data from January 2007-Dec. 2016 from here: http://oceandata.sci.gsfc.nasa.gov/MODIST/Mapped/Monthly/4km/SST/
In total 120 files will be downloaded.
2.	Download SeaDas tool from here: http://seadas.gsfc.nasa.gov/seadas64/
3.	Load each raw (downloaded in .nc format) monthly SST data file (separately into SeaDas). 
4.	Click on the Bands (Products) folder and then click  Sea Surface Temperature
5.	Click processing (top menu) and  then select crop. Select the geo coordinates tab under the spatial subset tab. Add the following coordinates:
North lat bound: 31
West long bound: -87.
South lat bound: 9
East long bound: -56
6.	Click on the Bands (Products) folder and then click  Sea Surface Temperature and check to make sure the cropped area is correct.
7.	File->raster export->GeoTiff and save file as X(month)_XXXX(year)
8.	Create land mask from annual (resampled) SST by running code in “resample to 1 km and mask land for monthly sst data.R”
9.	Resample all monthly files to 1 km res and add land mask by running “resample to 1 km and mask land for monthly sst data.R”
10.	Stack monthly raster files to create .nc file by running “Create monthly SST netCDF file.R”

 
