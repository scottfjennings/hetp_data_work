
## functions to create some covariates and merge with hetp data

## all these use hetpDF, a dataframe of hetp data, with at minumum the following fields:
#$ timestamp
#$ location_lat
#$ location_long
#$ local_identifier
#$ event_id


library(maptools)
library(tidyverse)
library(lubridate)
library(chron)

hetp_use <- read.csv("data_files/HETP_GPSonly_201706_201807.csv")

hetp_use <- hetp_use %>% 
  mutate(timestamp = as.POSIXct(as.character(timestamp)))


################################
## determin time of dawn and dusk for each day of the study

# dawn.dusk is a standalone dataframe with a record for each date in hetpDF
# it is used below in assign.inlight() and make.eel.avail()

make.dawn.dusk<-function(){
  study.days <- unique(as.Date(hetp_use$timestamp))
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


################################
## function for interpolating water level at bird GPS times using approx()

## this uses a tide file created by C:/Users/scott.jennings/Documents/Projects/water_levels/tides/read_tide_files.R


blake <- read.csv("C:/Users/scott.jennings/Dropbox (Audubon Canyon Ranch)/water_levels/tides/BlakesLanding/blake_tides_compiled.csv") %>% 
  mutate(date = as.Date(as.character(date)),
         time = as.character(time),
         datetime = as.POSIXct(as.character(datetime)))

make.bird.tides <- function() {
  bird.tides <- data.frame(approx(blake$datetime, blake$water.level, xout = hetp_use$timestamp, method = "constant", ties = mean))
  colnames(bird.tides) <- c("timestamp", "water.level")
  return(bird.tides)
}

bird.tides <- make.bird.tides()

# !! need to get time formats right- check bird.tides against blake.tides to make sure the times/tides still match up correctly. If not, run this
#bird.tides$timestamp <- with_tz(bird.tides$time, tzone = "America/Los_Angeles")

##########################################
## estimate the number of daylight hours per day that the eelgrass beds are available to foraging egrets
## error on this estimate is up to 10 min with the 5 min tide data
make.eel.avail <- function() {
  blake_dawn.dusk <- merge(blake, dawn.dusk, by.x="date", by.y="date", all.y=T)
  blake_dawn.dusk$inlight		=   blake_dawn.dusk$datetime	%within%	interval(	blake_dawn.dusk$dawn.time, blake_dawn.dusk$dusk.time)	
  eelgrass_available <- data.frame(blake_dawn.dusk %>% 
                                     group_by(date) %>% 
                                     filter(inlight=="TRUE" & water.level<=1) %>% 
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






