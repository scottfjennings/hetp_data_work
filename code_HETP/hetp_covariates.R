
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


# if coming from interpolate_bird_tides.R, then reading and fixing timestamp should be skipped,
hetp_use <- read.csv("data_files/HETP_GPSonly_201706_201807.csv")
hetp_use <- hetp_use %>% 
  mutate(timestamp = as.POSIXct(as.character(timestamp)))

# in stead just rename the final product from interpolate_bird_tides.R,
# make field date,
# and remove records with no coordinates
hetp_use <- bird_tides %>% 
  mutate(date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  filter(!is.na(location_lat) & !is.na(location_long))
################################
## determin time of dawn and dusk for each day of the study

# dawn.dusk is a standalone dataframe with a record for each date in hetpDF
# it is used below in assign.inlight() and make.eel.avail()

make.dawn.dusk<-function(){
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
dawn.dusk<- make.dawn.dusk()


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
  foo.dawn.dusk <- cbind(select(foo.dusk, dusk.time = time), select(foo.dawn, dawn.time = time))
}
##
  
  
#########################################
## assign daylight T or F to each point, based on fields in dawn.dusk
# inlight is a standalone dataframe witha record for each timestamp in hetpDF
assign.inlight <- function(df) {
df$date=as.Date(as.character(df$timestamp))
  df1 <- merge(df, dawn.dusk, by.x="date", by.y="date", all=T)
  df1$inlight		=   df1$timestamp	%within%	interval(	df1$dawn.time	, df1$dusk.time)	
  df1 <- dplyr::select(df1, timestamp, inlight, dawn.time, dusk.time)
  return(df1)
}

inlight <- assign.inlight(hetp_use)


##########################################
## estimate the number of daylight hours per day that the eelgrass beds are available to foraging egrets
## error on this estimate is up to 10 min with the 5 min tide data
make.eel.avail <- function(threshold.water.level = 1) {
  blake_dawn.dusk <- merge(hetp_use, dawn.dusk, by.x="date", by.y="date", all.y=T)
  blake_dawn.dusk$inlight		=   blake_dawn.dusk$datetime	%within%	interval(	blake_dawn.dusk$dawn.time, blake_dawn.dusk$dusk.time)	
  eelgrass_available <- data.frame(blake_dawn.dusk %>% 
                                     group_by(date) %>% 
                                     filter(inlight=="TRUE" & water.level <= threshold.water.level) %>% 
                                     summarise(num.hours = (n()*5/60)))
  return(eelgrass_available)
}

eelgrass_avail <- make.eel.avail()



## add inlight, eelgrass_avail and bird.tides to hetpDF 
## and add 'zones' from assign_zones.R
make.hetpDF_use <- function(df) {
  df <- merge(df, inlight, by.x = "timestamp", by.y = "timestamp", all=T)
  df <- merge(df, bird.tides, by.x = "timestamp", by.y = "timestamp", all=T)
  df$date=as.Date(as.character(df$timestamp))
  df <- merge(df, eelgrass_avail, by.x = "date", by.y = "date", all=T)
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





write.csv(hetpDF_use, "data_files/hetp_use_temp.csv", row.names = F)



#hetpDF_summ <- hetpDF_use %>% 
#  group_by(bird, date, zone.name) %>% 
#  summarise(num.points = n()) %>% 
#  full_join(eelgrass_avail)

#hetpDF_summ$num.hours[is.na(hetpDF_summ$num.hours)]<- 0






