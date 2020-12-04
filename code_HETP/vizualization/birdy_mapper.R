
#load the required libraries
#if necessary, can use install.packages([package name])
library(tidyverse)
library(lubridate)
library(maptools)
library(ggmap)
library(ggsn)
library(gridExtra)
library(hms)
library(RColorBrewer)
library(scales)

#devtools::install_github("dkahle/ggmap")


## read in a hetp data file with a few additional fields
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201907.csv")




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
  int.time = as.integer(timestamp),
  mo.da=paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep=""),
  hr=hour(timestamp),
  hr.mn=as.character(paste(hour(timestamp), ":", minute(timestamp), sep="")),
  hr.mn.sec=as.hms(paste(hour(timestamp), minute(timestamp), second(timestamp), sep=":")))


##get base map for area covering all points
#bbox.allpoints <- c(left = min(hetp_plus$location_long)-.025, bottom = min(hetp_plus$location_lat)-.025, right = max(hetp_plus$location_long)+.025, top = max(hetp_plus$location_lat)+.025)
#allpoints.basemap <- get_stamenmap(bbox.allpoints, maptype = "terrain-background", zoom = 12, color="bw")

## get base map for just Northern Tomales Bay
bbox.NTomales <- c(left = -122.99, bottom = 38.19, right = -122.9, top = 38.25)
NTomales.terrain.map <- get_stamenmap(bbox.NTomales, maptype = "terrain-background", zoom = 14, color="bw")

NTomales.terrain.map <- get_map(location = c(lon = -122.945, lat = 38.22), maptype = "terrain", zoom = 14)
NTomales.stamenterrain.map <- get_map(location = c(lon = -122.945, lat = 38.22), maptype = "terrain-background", zoom = 14, source = c("stamen"))


## now a function to create a map showing all points for a certain bird on a certain date
## point color indicates predicted tidal water level
## the bottom plot shows the tides and night vs day for the selected date.
birdy_mapper_tides <- function(zbird, zdate){
  

 zbird = "GREG_1"
 zdate = "2017-06-27"
 
 fooz <- filter(hetp_plus, bird == zbird, date == zdate)
 first.fooz = min(fooz$timestamp)
 last.fooz = max(fooz$timestamp)
 
ggmap(NTomales.terrain.map)+
   geom_point(aes(x=location_long, y= location_lat), color = "black", data = filter(fooz, inlight=="FALSE"), size = 3) +
    geom_point(aes(x=location_long, y= location_lat, color = water.level), data = filter(fooz, inlight=="TRUE"), size = 3) +
   geom_path(aes(x=location_long, y= location_lat, color = water.level), data = filter(fooz, inlight=="TRUE"), size = 1) +
   theme(axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank()) +
  scale_colour_gradient(low = muted("blue"),
  high = muted("red"), name = "Tide\nheight")
ggsave(paste("figures_output/", zbird, "_", zdate, "map.jpg", sep = ""), height = 6, width = 6)
 
ggplot(data = fooz)+
    geom_rect(data=fooz, aes(xmax=dawn.time, xmin=first.fooz, ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3)+
    geom_rect(data=fooz, aes(xmax=last.fooz, xmin=dusk.time, ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3) +
    geom_point(aes(x=timestamp, y=water.level, color = water.level), data = filter(fooz, inlight=="TRUE"), size = 6)  +
    geom_point(aes(x=timestamp, y=water.level), color = "black", data = filter(fooz, inlight=="FALSE"), size = 6) + 
   scale_fill_gradient(limits = c(-2, 6.5)) +
   ylim(c(-2, 6.5)) +
   theme(legend.position = "none") +
   ylab("Tide height") +
   xlab("Time") +
   scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M", tz = "America/Los_Angeles")) +
  scale_colour_gradient(low = muted("blue"),
  high = muted("red"))

ggsave(paste("figures_output/", zbird, "_", zdate, "tide.jpg", sep = ""), height = 6, width = 6)
 
 
grid.arrange(a, z, ncol=1)
}
 
###------------------------
devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
NTomales.satellite.map <- get_map(location = c(lon = -122.87, lat = 38.223), maptype = "terrain", zoom = 12)

df <- df %>% 
   rename(long = location.long,
          lat = location.lat)

ggmap(NTomales.satellite.map) +
   geom_path(aes(x=long, y= lat), data = df, size = 1) +
   geom_point(aes(x=long, y= lat, color = num.hours), data = df, size = 2) +
   theme(axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank()) +
  scale_colour_gradient(low = "blue", high ="red", name = "Daylight hours\neelgrass available") +
    scalebar(df, dist = 10, dist_unit = "km",
             transform = TRUE, model = "WGS84", location = "bottomright", st.dist = .05, st.size = 5, height = .06, anchor = c(x = -122.87, y = 38.223))



ggsave(paste("figures_output/", zbird, "_", zdate, "map.jpg", sep = ""), height = 6, width = 6)
 

ggplot(aes(x = date, y = num.hours), data = df) + 
  geom_col()  + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  xlab("") +
  ylab("Number of daylight hours eelgrass available") + 
  scale_x_datetime(labels = date_format("%b%d"))





## and now the call to the function, where you can select the bird and date.


   zbird = "GREG_1"
   zdate = "2017-07-04"
   
   birdy_mapper_tides(zbird = zbird,
              zdate = zdate)
 
 ggsave(paste(zbird, zdate, "tide.jpg", sep = "_"), height = 6, width = 6)
 
#############################################################

  
   #fooz <- filter(hetp_plus, bird == zbird, date >= date1, date <= date2)
   
   
  
 birdy_mapper_basic <- function(zbird, start.date, end.date, zzoom = 12, zhoriz = 0.05, zvert = 0.05){
 
   #zhoriz = 0
   #zvert = 0
   #zbird = "GREG_2"
   #start.date = "2017-06-10"
   #end.date = "2017-09-01"
   #zzoom = 14
   
  fooz <- filter(hetp_plus, bird == zbird, date >= start.date, date <= end.date)
   
   first.fooz = filter(fooz, timestamp == min(timestamp))
   last.fooz = filter(fooz, timestamp == max(timestamp))
   
   fooz.bbox <- c(left = min(fooz$location_long) - zhoriz, 
                  bottom = min(fooz$location_lat) - zvert, 
                  right = max(fooz$location_long) + zhoriz, 
                  top = max(fooz$location_lat) - zvert)
   
  fooz.terrain.map <- get_stamenmap(fooz.bbox, maptype = "terrain-background", zoom = zzoom, color="bw")
   
    #fooz.center <- fooz %>% 
    #  summarise(center.lat = max(location_lat) - ((max(location_lat) - min(location_lat))/2),
    #            center.long = max(location_long) - ((max(location_long) - min(location_long))/2))
   
   #NTomales.satellite.map <- get_map(location = c(lon =  fooz.center$center.long + zhoriz, lat = fooz.center$center.lat + zvert), maptype = "satellite", zoom = zzoom)
   
   
   a = ggmap(fooz.terrain.map)+
     geom_point(aes(x=location_long, y= location_lat, size = 1), data = fooz)+
     theme(legend.position="none",
           axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank())
   
   a
   
 }  
   

 

   
birdy_mapper_basic(zbird == "GREG_2", start.date == "2017-06-10", end.date == "2017-09-01", zzoom = 13)

greg2_stg2 <- grid.arrange(GREG_2_20180605, GREG_2_20180606, ncol = 1)

ggsave(filename = "GREG_2_20180605.png",
       plot = GREG_2_20180605, width = 7, height = 6.7, units = 'cm',
       scale = 2, dpi = 600)


