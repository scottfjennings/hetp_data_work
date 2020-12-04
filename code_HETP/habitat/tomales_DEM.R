

## downloaded Tomales Bay DEM (above and below MLLW) from https://data.noaa.gov//metaview/page?xml=NOAA/NESDIS/NGDC/MGG/DEM/iso/xml/tomales_bay_P110_2018.xml&view=getDataView&header=none#Documentation

## file format is netCDF, which can be read by raster and 2 other packages according to this: http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

## file is saved at data_files/habitat/tomales_bay_P110_2018.nc

library(raster)
library(ncdf4)
library(sf)

marin.dem <- raster::raster('data_files/habitat/tomales_bay_P110_2018.nc')

eel <- st_read('data_files/habitat/ds890.shp')

plot(marin.dem)
plot(eel)

eel_ll <- st_transform(eel, crs = crs(marin.dem))

eel_bathy <- mask(marin.dem, eel_ll)
