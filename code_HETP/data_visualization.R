
library(tidyverse)
library(lubridate)
library(move)

library(maptools)
library(ggmap)
library(ggsn)

#devtools::install_github("dkahle/ggmap")

##################################################################################
##### now some plotting


# Get the basemap; selects center of basic Google background based on locations in the subseted location file

#df1 <- subset(hetpDF_use, local_identifier=="GREG_3" & (day(timestamp)>=12 | day(timestamp)<=19))
#df1 <- subset(hetpDF, hour(timestamp)>13 & hour(timestamp)<15)
df1 <- subset(hetpDF_use, local_identifier=="GREG_3" & Date<="2017-06-22")

ggmap(north.Tomales, extent='panel',padding=0)+
  geom_point(aes(x=location_long, y=location_lat, colour=water.level), 
             data=subset(hetpDF_use, local_identifier=="GREG_3" & Date=="2017-06-16"), size=1)+
  theme_void()


## stamen map

tsize=4
scalebar_long= max(hetpDF$location_long)-.1
scalebar_lat= min(hetpDF$location_lat)-.02
arrow_size=4
bbox <- c(left = min(hetpDF$lon)-.025, bottom = min(hetpDF$lat)-.025, right = max(hetpDF$lon)+.025, top = max(hetpDF$lat)+.025)
foo <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 12, color="bw")

labeled_map <- 
  
  ggmap(foo)

+
  
  annotate("text", x=-122.75, y=38.28, label="Two Rock-Fallon", size=tsize)+
  geom_segment( aes(x=-122.75, y=38.275, xend = -122.78, yend = 38.26), size=arrow_size, arrow = arrow(length = unit(0.03, "npc")))+
  
  annotate("text", x=-122.82, y=38.18, label="italic('Chileno Valley')", parse = TRUE, size=tsize)+
  geom_segment( aes(x=-122.82, y=38.185, xend = -122.8, yend = 38.2), size=arrow_size, arrow = arrow(length = unit(0.03, "npc")))+
  
  annotate("text", x=-122.945, y=38.252, label="italic('Toms Point')", parse = TRUE, size=tsize)+ 
  geom_segment( aes(x=-122.945, y=38.248, xend = -122.949, yend = 38.227), size=arrow_size, arrow = arrow(length = unit(0.03, "npc")))+
  
  annotate("text", x=-122.92, y=38.18, label="italic('Tomales Bay')", parse = TRUE, angle=-55, size=tsize) + 
  
  scale_bar(lon = scalebar_long, lat = scalebar_lat, distance_lon = 5, distance_lat = .1, distance_legend = .5, dist_unit = "km", arrow_length=2.5, arrow_distance=1, arrow_north_size = tsize)


df1 <- subset(hetpDF, date(timestamp)=="2017-07-20")
bbox <- c(left = min(df1$lon)-.025, bottom = min(df1$lat)-.025, right = max(df1$lon)+.025, top = max(df1$lat)+.025)
foo <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 12, color="bw")


ggmap(foo)+
  geom_point(aes(x=lon, y=lat), data=df1)+
  #facet_grid(~bird)+
  theme_void()+
  scalebar(data=df1, dist=5)




df1 <- hetpDF

lat.center = max(df1$location_lat)-((max(df1$location_lat)-min(df1$location_lat))/2)
lon.center = max(df1$location_long)-((max(df1$location_long)-min(df1$location_long))/2)

north.Tomales <- get_map(
  #location='california',
  c(lon=lon.center,lat=lat.center),
  zoom=12,crop=T,
  scale="auto",color="bw",source="google", maptype="terrain") # can change to terrain


all3 <- ggmap(north.Tomales, extent='panel',padding=0)+
  geom_point(aes(x=location_long, y=location_lat), data=hetpDF_use, size=1)+
  theme_void()+
  facet_grid(~local_identifier)




library(gridExtra)
df1 <- subset(hetpDF_use, Date<= local_identifier=="GREG_3", num.hours>=3.5)
a=ggmap(north.Tomales, extent='panel',padding=0)+
  geom_point(aes(x=location_long, y=location_lat), data=subset(df1, num.hours>=3.5), size=1)+
  theme_void()+
  annotate("text", x = -122.95, y = 38.3, label = "A", size=9)
b=ggmap(north.Tomales, extent='panel',padding=0)+
  geom_point(aes(x=location_long, y=location_lat), data=subset(df1, num.hours<3.5), size=1)+
  theme_void()+
  annotate("text", x = -122.95, y = 38.3, label = "B", size=9)

ggplot(data=subset(blakes_tides1, Date=="2017-06-13"), aes(x=DateTime, y=water.level))+
         geom_line()+
          geom_vline(xintercept=dawn.time, data=subset(blakes_tides1, Date=="2017-06-13"))



ggsave(file="data_visualization/all3.tiff", all3, dpi = 300)

labz <- data.frame(x="x", 
                   y="y",
                   label=c("eelgrass available 3 or more daylight hours", "eelgrass available less than 3 daylight hours"))

ggplot(dat, aes(x=date, y=value, color=location, group=location)) + 
  geom_line()+
  facet_grid(product ~ ., scale = "free_y")+
  geom_text(data=labz, aes(x=x, y=y, label=label), 
            colour="black", inherit.aes=FALSE, parse=TRUE)



############################################
### looking at time lags in when birds are in different zones
library(gstat)

greg2NTom<- droplevels(filter(hetpDF_use, bird=="GREG_2", zone.name=="NTomales"))


hist(greg2NTom$date, breaks=130)
lines(greg2NTom$water.level)

ggplot(data = greg2NTom, aes(x=date))+
  geom_histogram(binwidth = 1)+
  geom_point(aes(y=100*num.hours), data=greg2NTom)

ggplot(data = greg2NTom, aes(x=date, y=num.hours))+
  geom_point()

zoof <- timeLag(greg2NTom)


Vario1 = variogram(timestamp, greg2NTom)
plot(Vario1)


foof <- data.frame(sec.diffs = as.numeric(diff(greg2NTom$timestamp)))
               foof$hour.diffs = foof$sec.diffs/60/60  
               foof$rnd.hour.diffs = round(foof$hour.diffs, 0)
               
foof <- data.frame(diffs = as.numeric(diff(greg2NTom$date)))
               
hist(data$hour.diffs)

####
ggplot(data = hetpDF_use, aes(x=date))+
  geom_bar(aes(fill=zone.name))+
  geom_line(aes(y=12*num.hours))+
  facet_wrap(~bird, ncol = 1)+
  ylab("number of 5-minute blocks")+ 
  xlab("")+
  labs(caption = "black line indicates eelgrass availability")

 