library(tidyverse)
library(lubridate)
library(ggmap)
library(maps)
library(mapdata)

library(leaflet)
library(shiny)
library(chron)
library(scales)
library(hms)


######################################
## to extract and save files for a single bird on a single day
## only need to do this once

## read in the hetp_use data table created by data_visualization.R
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201904.csv")


## fix the format for the date fields
hetp_1day <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S", tzone = "America/Los_Angeles"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"), 
         ztime = times(paste(hour(timestamp), minutes(timestamp), second(timestamp), sep = ":"))) %>%
  select(date, timestamp, lat = location_lat, lon = location_long, bird, everything()) %>% 
  data.frame()

tz(hetp_1day$timestamp) <- "America/Los_Angeles"

bird_day_filter <- function(zbird, zday){
foo <- hetp_1day %>% 
  filter(date == zday, bird == zbird) %>% 
  droplevels() %>% 
  arrange(timestamp) %>%
  select(date, timestamp, lat, lon, bird, utm.easting, utm.northing, utm.zone, inlight, dawn.time, dusk.time, water.level, num.hours, ztime) %>% 
  arrange(date) 
return(foo)
}

bird_day_filter("GREG_1", "2017-06-27") %>% 
  write.csv(., "data_files/GPS_with_covariates/greg2_20170627.csv")


######################################################
### once single day single bird files are saved, can start here

#g1_170627 <- read.csv("data_files/GPS_with_covariates/greg1_20170627.csv") 

format_fixer <- function(df){
df <- df %>%
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.character(timestamp),
         timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S", tzone = "America/Los_Angeles"))
return(df)
}
#g1_170627 <- format_fixer(g1_170627)


filt_rounder <- function(df){
df_filt <- df %>% 
  filter(hour(timestamp) >= 5 & hour(timestamp) <= 21 & (minute(timestamp) == 0)) %>% 
  mutate(utm.north.rnd10 = round(utm.northing, -1),
         utm.east.rnd10 = round(utm.easting, -1),
         utm.north.rnd100 = round(utm.northing, -2),
         utm.east.rnd100 = round(utm.easting, -2),
         utm.north.rnd50 = (round((utm.northing/50), 0)*50),
         utm.east.rnd50 = (round((utm.easting/50), 0)*50))
return(df_filt)
}
#g1_170627_filt <- filt_rounder(g1_170627)

test_plotter <- function(df_filt, zmain){
ggplot(data = df_filt) +
  #geom_point(aes(x = utm.east.rnd10, y = utm.north.rnd10), color = "blue") +
  #geom_point(aes(x = utm.east.rnd100, y = utm.north.rnd100), color = "red") +
  geom_jitter(aes(x = utm.east.rnd50, y = utm.north.rnd50), color = "green")+
  geom_path(aes(x = utm.east.rnd50, y = utm.north.rnd50), color = "green") +
  geom_point(aes(x = utm.east.rnd50, y = utm.north.rnd50), color = "red") +
  #xlim(503000, 519000) +
  #ylim(4228000, 4237000) +
  ggtitle(zmain)
}

#test_plotter(g1_170627_filt, "GREG1_170627")
#test_plotter(g1_170704_filt, "GREG1_170704")


greg1_20170704 <- read.csv("data_files/GPS_with_covariates/greg1_20170704.csv") %>% 
  format_fixer() %>% 
  filt_rounder() 



greg1_20170627 <- read.csv("data_files/GPS_with_covariates/greg1_20170627.csv") %>% 
  format_fixer() %>% 
  filt_rounder() 

greg2_20170704 <- read.csv("data_files/GPS_with_covariates/greg2_20170704.csv") %>% 
  format_fixer() %>% 
  filt_rounder() 

greg2_20170627 <- read.csv("data_files/GPS_with_covariates/greg2_20170627.csv") %>% 
  format_fixer() %>% 
  filt_rounder() 

utm_summarizer <- function(bird_day) {
bird_day_utm_summary <- bird_day %>% 
  summarise(min_utmE = min(utm.easting),
            max_utmE = max(utm.easting),
            min_utmN = min(utm.northing),
            max_utmN = max(utm.northing))
}

utm_summaries <- rbind(utm_summarizer(greg1_20170627) %>% 
  mutate(bird_day = "greg1_20170627") %>% 
    select(bird_day, everything()),
utm_summarizer(greg1_20170704) %>% 
  mutate(bird_day = "greg1_20170704") %>% 
    select(bird_day, everything()),
utm_summarizer(greg2_20170627) %>% 
  mutate(bird_day = "greg2_20170627") %>% 
    select(bird_day, everything()),
utm_summarizer(greg2_20170704) %>% 
  mutate(bird_day = "greg2_20170704") %>% 
    select(bird_day, everything()))

lat.buffer <- (max(greg1_20170704$lat) - min(greg1_20170704$lat)) * 0.1
lon.buffer <- (max(greg1_20170704$lon) - min(greg1_20170704$lon)) * 0.1

ztop = max(greg1_20170704$lat) + lat.buffer
zbottom = min(greg1_20170704$lat) - lat.buffer

zright = max(greg1_20170704$lon) + lon.buffer
zleft = min(greg1_20170704$lon) - lon.buffer

nTomales.satellite <- get_googlemap(center = c(lon = -122.92, lat =  38.2), zoom = 13,  maptype = "satellite") 

tomspoint.satellite <- get_googlemap(center = c(lon = -122.939848, lat =   38.215723), zoom = 14,  maptype = "satellite") 

tomspoint.terrain <- get_googlemap(center = c(lon = -122.939848, lat =   38.215723), zoom = 14,  maptype = "terrain") 


nTomales.stamen.terrain <- get_stamenmap(bbox = c(left = -122.975,
                    right = -122.915,
                    top = 38.24,
                    bottom = 38.205),
                    zoom = 11, 
                    maptype ='terrain',
                    color = 'color')


ggmap(tomspoint.terrain) +
  geom_path(aes(x = lon, y = lat), data = greg1_20170704, color = "green") +
  geom_point(aes(x = lon, y = lat), data = greg1_20170704, color = "red") 
  


test_mapper_goog <- function(df_filt, zmain, zmaptype, zzoom){
  center.lon = mean(df_filt$lon)
  center.lat = mean(df_filt$lat)
  
foo.base <- get_googlemap(center = c(lon = center.lon, lat =   center.lat), zoom = zzoom,  maptype = zmaptype) 

ggmap(foo.base) +
  geom_path(aes(x = lon, y = lat), data = df_filt, color = "green") +
  geom_point(aes(x = lon, y = lat), data = df_filt, color = "red") +
  ggtitle(zmain)
}

test_mapper_goog(greg2_20170704, "greg2_20170704", "satellite", 12)
test_mapper_goog(greg2_20170627, "greg2_20170627", "satellite", 12)


greg1_20170627 %>% 
  select(date_time = timestamp, bird, UTM_easting = utm.east.rnd50, UTM_northing = utm.north.rnd50) %>% 
  write.csv(., "data_files/data4education/greg1_20170627.csv", row.names = F)

greg1_20170704 %>% 
  select(date_time = timestamp, bird, UTM_easting = utm.east.rnd50, UTM_northing = utm.north.rnd50) %>% 
  write.csv(., "data_files/data4education/greg1_20170704.csv", row.names = F)

greg2_20170627 %>% 
  select(date_time = timestamp, bird, UTM_easting = utm.east.rnd50, UTM_northing = utm.north.rnd50) %>% 
  write.csv(., "data_files/data4education/greg2_20170627.csv", row.names = F)

greg2_20170704 %>% 
  select(date_time = timestamp, bird, UTM_easting = utm.east.rnd50, UTM_northing = utm.north.rnd50) %>% 
  write.csv(., "data_files/data4education/greg2_20170704.csv", row.names = F)


greg2_20170627 %>% 
  select(date_time = timestamp, tide_height = water.level) %>% 
  mutate(tide_height = round(tide_height, 1)) %>% 
  write.csv(., "data_files/data4education/tide_20170627.csv", row.names = F)

greg2_20170704 %>% 
  select(date_time = timestamp, tide_height = water.level) %>% 
  mutate(tide_height = round(tide_height, 1)) %>% 
  write.csv(., "data_files/data4education/tide_20170704.csv", row.names = F)
