
## functions to create some covariates and merge with hetp data

## all these use hetpDF, a dataframe of hetp data, with at minumum the following fields:
#$ timestamp
#$ location_lat
#$ location_long
#$ local_identifier
#$ event_id


# if coming from interpolate_bird_tides.R, then tidyverse and lubridate will already be loaded
library(tidyverse)
library(lubridate)

library(maptools)
library(chron)


hetp_use <- read.csv("data_files/HETP_GPSonly_201706_201807.csv")
hetp_use <- hetp_use %>% 
  mutate(timestamp = as.POSIXct(as.character(timestamp)))

# if coming from interpolate_bird_tides.R, then reading and fixing timestamp should be skipped,
# in stead just rename the final product from interpolate_bird_tides.R,
# make field date,
# and remove records with no coordinates
hetp_use <- bird_tides %>% 
  mutate(date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  filter(!is.na(location_lat) & !is.na(location_long))
################################
## determin time of dawn and dusk for each day of the study

add.dawn.dusk<-function(df){
pts <- cbind(df$location_long, df$location_lat)
df.sp <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))

dawn = data.frame(crepuscule(df.sp, df$timestamp, solarDep = 6, direction = "dawn", POSIXct.out = TRUE))
dusk = data.frame(crepuscule(df.sp, df$timestamp, solarDep = 6, direction = "dusk", POSIXct.out = TRUE))
  
dawn.dusk <- cbind(select(dawn, dawn.time = time), select(dusk, dusk.time = time))
hetp_dawn.dusk <- cbind(df, dawn.dusk)
}
hetp_dd <- add.dawn.dusk(hetp_use)


## for testing  
test.add.dawn.dusk <- function(zlong, zlat, zdate){
  hels <- matrix(c(zlong, zlat), nrow=1)
  Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
  d <- as.POSIXct(as.character(zdate), tzone = "America/Los_Angeles")
  foo.dawn = data.frame(crepuscule(Hels, d, solarDep=6, direction="dawn", POSIXct.out=TRUE)) 
  foo.dusk = data.frame(crepuscule(Hels, d, solarDep=6, direction="dusk", POSIXct.out=TRUE)) 
  foo.dawn.dusk <- cbind(select(foo.dawn, dawn.time = time), select(foo.dusk, dusk.time = time))
  return(foo.dawn.dusk)
}
##
  
  
#########################################
## assign daylight T or F to each point, based on fields in dawn.dusk
# inlight is a standalone dataframe witha record for each timestamp in hetpDF
assign.inlight <- function(df) {
df <- df %>% 
  mutate(inlight = timestamp	%within%	interval(dawn.time, dusk.time))
  return(df)
}

hetp_dd_inlight <- assign.inlight(hetp_dd)


##########################################
# blake.dawn.dusk is a standalone dataframe with a record for each date in hetpDF
# it is used below in make blake_eelgrass_available

make.blake.dawn.dusk<-function(){
  study.days <- unique(as.Date(hetp_use$timestamp, tz = "America/Los_Angeles"))
  length(study.days)
  hels <- matrix(c(-122.9, 38.2), nrow=1)
  Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
  d <- as.POSIXct(as.character(study.days), tzone = "America/Los_Angeles")
  dawn = data.frame(crepuscule(Hels, d, solarDep=6, direction="dawn", POSIXct.out=TRUE))   
  colnames(dawn) <- c("dawn.frac", "dawn.time")
  dusk = data.frame(crepuscule(Hels, d, solarDep=6, direction="dusk", POSIXct.out=TRUE))   
  colnames(dusk) <- c("dusk.frac", "dusk.time") 
  dawn.dusk <- cbind(dawn, dusk) %>% 
    dplyr::select(dawn.time, dusk.time)
  dawn.dusk$date <- as.Date(dawn.dusk$dawn.time)
  return(dawn.dusk)
}
blake.dawn.dusk<- make.blake.dawn.dusk()



blake4eelavail <- blake5min_tides %>% 
  mutate(date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  full_join(blake.dawn.dusk, by = c("date")) %>% 
  assign.inlight()

## make eelgrass.available
## estimate the number of daylight hours per day that the eelgrass beds are available to foraging egrets
## error on this estimate is up to 10 min with the 5 min tide data
 blake_eelgrass_available <- blake4eelavail %>% 
   group_by(date) %>% 
   filter(inlight=="TRUE" & water.level <= 1) %>%
   summarise(num.hours = (n()*5/60)) %>% 
   full_join(select(blake.dawn.dusk, date), by = c("date")) %>% 
   mutate(num.hours = ifelse(is.na(num.hours), 0, num.hours)) %>% 
   arrange(date)




## add inlight, eelgrass_avail and bird.tides to hetpDF 
## and add 'zones' from assign_zones.R
make.hetpDF_use <- function(df) {
  df <- merge(df, hetp_dd_inlight, by.x = "timestamp", by.y = "timestamp", all=T)
  #df <- merge(df, bird.tides, by.x = "timestamp", by.y = "timestamp", all=T)
  df$date=as.Date(as.character(df$timestamp))
  df <- merge(df, blake_eelgrass_available, by.x = "date", by.y = "date", all=T)
  #df <- merge(df, bird.zones, by.x="event_id", by.y="event_id", all=T)
  df$num.hours[is.na(df$num.hours)] <- 0
  df <- dplyr::arrange(df, timestamp)
  df1 <- unique(df)
  df1 <- df1[complete.cases(df1), ]
  return(df1)
}

hetpDF_use <- make.hetpDF_use(hetp_use)


### now read these covariates back into the @data slot of the hetp movestack
#hetp@data<- merge(hetp@data, dplyr::select(hetpDF_use, event_id, inlight, water.level, num.hours, zone.name), by.x="event_id", by.y="event_id", all=T)


hetpDF_with_covariates <- hetp_dd_inlight %>% 
  full_join(., blake_eelgrass_available, by = "date")


write.csv(hetpDF_with_covariates, "data_files/GPS_with_covariates/hetpGPS_with_covariates.csv", row.names = F)



# make a light data frame for shiny webmap
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201811.csv")

## fix the format for the date fields
hetp <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
         mo.da = paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep = ""),
         #hr=hour(timestamp),
         #hr.mn=as.POSIXct(paste(hour(timestamp), ":", minute(timestamp), ":00", sep=""), format="%H:%M:%S"),
         hr.mn.sec=as.character(paste(hour(timestamp), minute(timestamp), second(timestamp), sep = ":"))) %>% 
  select(date, ztimestamp = timestamp, lat = location_lat, lon = location_long, bird, mo.da, hr.mn.sec, water.level, inlight)


write.csv(hetp, "C:/Users/scott.jennings/Documents/Projects/hetp/hetp_shiny_webmap/data/hetp_use.csv", row.names = F)

