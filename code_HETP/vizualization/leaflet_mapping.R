##mapping hetp data with leaflet

library(tidyverse)
library(lubridate)
library(leaflet)
options(scipen = 999)

## read in the hetp_use data table created by data_visualization.R
hetp <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201907.csv")


## fix the format for the date fields
hetp <- hetp %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
  timestamp=as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
  mo.da=paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep=""),
  hr.mn=paste(hour(timestamp), ":", minute(timestamp), sep="")) %>% 
  select(date, timestamp, lat = location_lat, lon = location_long, bird, dawn.time, dusk.time, inlight, num.hours, mo.da, hr.mn, water.level)



##########################
### plotting
## tomkat example: https://github.com/pointblue/state-of-the-ranch/blob/master/weather.Rmd
# set color palette
pal <- colorNumeric(
  palette = "Blues",
  domain = hetp$water.level)

leaf <- leaflet(height=500) %>% setView(lng=-122.9, lat=38.22, zoom=14) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(data=filter(hetp, bird=="GREG_1"), radius=1, color = ~pal(inlight), group= "GREG_1", popup=~paste0(mo.da, ", ", hr.mn, '<br>Tide level: ',water.level, '<br>During daylight? ',inlight), options = popupOptions(maxWidth=800)) %>%
  #addCircleMarkers(data=filter(hetp, bird=="GREG_2"), radius=1, color = ~pal(inlight), group= "GREG_2", popup=~paste0(mo.da, ", ", hr.mn, '<br>Tide level: ',water.level, '<br>During daylight? ',inlight), options = popupOptions(maxWidth=800)) %>%
  #addCircleMarkers(data=filter(hetp, bird=="GREG_3"), radius=1, color = ~pal(inlight), group= "GREG_3", popup=~paste0(mo.da, ", ", hr.mn, '<br>Tide level: ',water.level, '<br>During daylight? ',inlight), options = popupOptions(maxWidth=800)) %>%
  addLegend(position='topright', colors=pal(c(FALSE, TRUE)), labels=c('No', 'Yes'), opacity=0.8, title='During daylight') %>%
  addLayersControl(position='bottomleft',baseGroups=c('GREG_1','GREG_2','GREG_3'), 
                   options=layersControlOptions(collapsed=F))%>%
  addMeasure(    primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters")

##########################


greg3_bigmove <-   leaflet(width = 500) %>% setView(lng=-121.98, lat=38.22, zoom=9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(data=filter(hetp, bird=="GREG_3", Date>="2017-08-13"), radius=3, popup=~paste0('Date: ', mo.da, '<br>Time (24-hr format): ', hr.mn, '<br>During daylight? ',inlight), options = popupOptions(maxWidth=800)) %>%
  addMeasure(    primaryLengthUnit = "kilometers",
                 primaryAreaUnit = "sqkilometers")



##########################

#############################



hetp_by_day <- hetp %>% 
  group_by(bird, date) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon)) %>% 
  ungroup() %>% 
  rename(lat = mean.lat, lon = mean.lon) %>% 
  mutate(last.lat = lag(lat),
         last.lon = lag(lon),
         lat.move = abs(lat - last.lat),
         lon.move = abs(lon - last.lon),
         deg.mov = sqrt((lat.move^2) + (lon.move^2))) %>% 
  select(-last.lat, -last.lon, -lat.move, -lon.move) %>% 
  mutate(deg.mov.lag2 = lag(deg.mov, 2),
         deg.mov.lag3 = lag(deg.mov, 3),
         deg.mov.lag4 = lag(deg.mov, 4),
         deg.mov.lag5 = lag(deg.mov, 5),
         deg.mov2day = deg.mov + deg.mov.lag2,
         deg.mov3day = deg.mov2day + deg.mov.lag3,
         deg.mov4day = deg.mov3day + deg.mov.lag4,
         deg.mov5day = deg.mov4day + deg.mov.lag5)



  
leaf_data <- hetp_by_day %>% 
  filter(bird == "GREG_11") %>% 
  ungroup() %>% 
  data.frame()
  
leaflet(height=500) %>% setView(lng=-122.9, lat=38.22, zoom=12) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(data = leaf_data, radius= 5, popup = ~paste(date, 'degrees moved = ', deg.mov, sep = " "), options = popupOptions(maxWidth=800))  %>%
  addCircleMarkers(data = filter(leaf_data, deg.mov5day > 2), radius= 10, popup = ~paste(date, 'degrees moved = ', deg.mov, sep = " "), options = popupOptions(maxWidth=800), color="orangered") %>%
  addPolylines(lng = leaf_data$lon,
               lat = leaf_data$lat, weight = 3)






