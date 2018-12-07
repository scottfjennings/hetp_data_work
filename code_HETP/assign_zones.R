
## a quick and sort of hacky way to assign gps points from the HTP project (or other) to a particular geographic area.
## the geographic area must first be established by drawing a polygon in Google Earth, saving it as a KML, importing the kml to R, then using the polygon as an overlay for the gps points


library(sp)
library(maptools) # for getKMLcoordinates()

## not sure yet how to generalize this first part (through pol =) and wrap it in a function
## for now if another zone is added just need to manually add to the zone.name list, add another call to kml2df, and add another line in pol =

zone.name = c("NTomales", "ChilenoValley", "TwoRock")

## start with a funtion to read in kml and make into df
## be sure to check file path in function
kml2df <- function(zfile){
   goo <- getKMLcoordinates(paste("C:/Users/scott.jennings/Documents/Projects/HETP/HETP_working/polys_from_GE/", zfile, ".kml", sep = ""), ignoreAltitude=TRUE)
    goo.df <- as.data.frame(goo)
    
   return(goo.df)
   }
# calls to function for 3 kmls
NTomales <- kml2df("NTomalesBay")
ChilenoValley <- kml2df("ChilenoValley")
TwoRock <- kml2df("TwoRock")


## bind the coordinates for each individual polygon into a SpatialPolygon
## will need to add a row for each additonal polygon
pol = SpatialPolygons(list(
  Polygons(list(Polygon(cbind(NTomales$X1, NTomales$X2))), ID="NTomales"),
  Polygons(list(Polygon(cbind(ChilenoValley$X1, ChilenoValley$X2))), ID="ChilenoValley"),
  Polygons(list(Polygon(cbind(TwoRock$X1, TwoRock$X2))), ID="TwoRock")
))



overlay2zones <- function(){
## use hetp.df from data_visualization.R
hetp.sub <- dplyr::select(hetpDF, lon, lat, event_id) %>% 
  na.omit()

pts <- cbind(hetp.sub$lon, hetp.sub$lat)
hetp.spd <- SpatialPointsDataFrame(pts, hetp.sub)

## do overlay of gps points and polygons
hoo <- over(hetp.spd, pol)


## bind back to the spatial points df
hetp.hoo <- cbind(hetp.spd, hoo) 

colnames(hetp.hoo@data)[4]="zone.num"
hetp.hoo@data$zone.num[is.na(hetp.hoo@data$zone.num)]=0


zones <- data.frame(zone.num = 0:length(zone.name),
                    zone.name = c("other", zone.name))


hetp.hoo@data<- merge(hetp.hoo@data, zones, all=T)

bird.zones <- data.frame(hetp.hoo@data) %>% 
  dplyr::select(event_id, zone.name)
return(bird.zones)
}

bird.zones<-overlay2zones()


