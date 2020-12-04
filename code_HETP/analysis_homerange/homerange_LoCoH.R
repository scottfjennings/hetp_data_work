

library(adehabitatHR)
library(sp)
#library(rgeos)


## start with a filtered dataframe from hetp_covariate_filter.R
zpoints = dplyr::select(df2, location.long, location.lat)
zdata = select(df, -location.lat, -location.long)
points_sp <- SpatialPoints(coords = zpoints, proj4string = CRS("+proj=longlat +datum=WGS84"))


points_sp_utm <- spTransform(points_sp, CRS("+proj=utm +zone=10 +datum=WGS84"))



system.time(goof <- LoCoH.k(points_sp_utm))




