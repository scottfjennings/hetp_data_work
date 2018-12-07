
#load the required libraries
#if necessary, can use install.packages([package name])
library(tidyverse)
library(lubridate)
library(maptools)
library(ggmap)
library(ggsn)
library(gridExtra)
library(RColorBrewer)


## read in a hetp data file with a few additional fields
hetp_start <- read.csv("data_files/hetp_use_temp.csv")

# this file has the following fields:
# date            
# timestamp       
# location_lat    
# location_long   
# bird: bird ID
# water.level: predicted tidal water level at the time of each GPS location 
# num.hours: number of daylight hours each day the eelgrass was available for foraging
# dawn.time: the time each day of dawn
# dusk.time: the time each day of dusk
# inlight: whether or not a GPS point was collected during dark (FALSE) or daylight (TRUE)


## fix the format for the date fields
hetp_plus <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
  timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
  dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
  dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
  int.time = as.integer(timestamp))


##get base map for area covering all points
#bbox.allpoints <- c(left = min(hetp_plus$location_long)-.025, bottom = min(hetp_plus$location_lat)-.025, right = max(hetp_plus$location_long)+.025, top = max(hetp_plus$location_lat)+.025)
#allpoints.basemap <- get_stamenmap(bbox.allpoints, maptype = "terrain-background", zoom = 12, color="bw")

## get base map for just Northern Tomales Bay
bbox.NTomales <- c(left = -122.99, bottom = 38.19, right = -122.9, top = 38.25)
NTomales.terrain.map <- get_stamenmap(bbox.NTomales, maptype = "terrain-background", zoom = 12, color="bw")

NTomales.satellite.map <- get_map(location = c(lon = -122.90, lat = 38.239), maptype = "satellite", zoom = 13)



## now a function to create a map showing all points for a certain bird on a certain date
## point color indicates predicted tidal water level
## the bottom plot shows the tides and night vs day for the selected date.
birdy_mapper_tides <- function(zbird, zdate){
  
  #zbird = "GREG_3"
  #zdate = "2017-06-14"
  
 fooz <- filter(hetp_plus, bird == zbird, date == zdate)
 first.fooz = min(fooz$timestamp)
 last.fooz = max(fooz$timestamp)
 
 a = ggmap(NTomales.terrain.map)+
    geom_point(aes(x=location_long, y= location_lat, color = water.level), data = filter(fooz, inlight=="TRUE")) +
   geom_line(aes(x=location_long, y= location_lat, color = water.level), data = filter(fooz, inlight=="TRUE")) +
   theme(legend.position="none",
         axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank())

 z= ggplot(data = fooz)+
    geom_rect(data=fooz, aes(xmax=dawn.time, xmin=first.fooz, ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3)+
    geom_rect(data=fooz, aes(xmax=last.fooz, xmin=dusk.time, ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3)+
    geom_line(aes(x=timestamp, y=water.level, color = water.level), size = 2) + 
   scale_fill_gradient(limits = c(-2, 6.5)) +
   ylim(c(-2, 6.5))
 
grid.arrange(a, z, ncol=1, top=paste(zbird, zdate, sep = ", "))
}
 
## and now the call to the function, where you can select the bird and date.
GREG1_20170624 <- birdy_mapper_tides(zbird = "GREG_1",
              zdate = "2017-06-24")
 
 ggsave("GREG1_20170624line.jpg", GREG1_20170624)
 
#############################################################

  
   #fooz <- filter(hetp_plus, bird == zbird, date >= date1, date <= date2)
   
   
   fooz <- filter(hetp_plus, bird == "GREG_2", date >= "2018-05-11", date <= "2018-05-11")
   
 birdy_mapper_basic <- function(zzoom = 12, zhoriz = 0, zvert = 0){
   #zzoom = 13
   #zhoriz = 0
   #zvert = 0
   
   first.fooz = filter(fooz, timestamp == min(timestamp))
   last.fooz = filter(fooz, timestamp == max(timestamp))
   
   
    fooz.center <- fooz %>% 
      summarise(center.lat = max(location_lat) - ((max(location_lat) - min(location_lat))/2),
                center.long = max(location_long) - ((max(location_long) - min(location_long))/2))
   
   NTomales.satellite.map <- get_map(location = c(lon =  fooz.center$center.long + zhoriz, lat = fooz.center$center.lat + zvert), maptype = "satellite", zoom = zzoom)
   
   
   a = ggmap(NTomales.satellite.map)+
     #geom_point(aes(x=location_long, y= location_lat, fill = "green", size = 1), data = first.fooz, pch=21, color = "black", inherit.aes = F)+
     #geom_point(aes(x=location_long, y= location_lat, fill = "blue", size = 1), data = last.fooz, pch=21, color = "black", inherit.aes = F)+ 
     #geom_point(aes(x=location_long, y= location_lat, fill = "red"), data = fooz, pch=21, color = "black", inherit.aes = F)+ 
     geom_path(aes(x=location_long, y= location_lat, color = int.time), size = 1, data = fooz) +
     scale_colour_gradientn(colours = heat.colors(10))+
     theme(legend.position="none",
           axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank())
   
   a
 }  
   

 
fooz <- filter(hetp_plus, bird == "GREG_2", date == "2018-06-05")
   
GREG_2_20180605 = birdy_mapper_basic(zzoom = 13)

greg2_stg2 <- grid.arrange(GREG_2_20180605, GREG_2_20180606, ncol = 1)

ggsave(filename = "GREG_2_20180605.png",
       plot = GREG_2_20180605, width = 7, height = 6.7, units = 'cm',
       scale = 2, dpi = 600)


