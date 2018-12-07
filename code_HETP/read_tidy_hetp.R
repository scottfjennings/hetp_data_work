

## various options for reading in and tidying hetp data

## do not run everything here!!!



library(tidyverse)
library(lubridate)
library(move)


### option 1, read in a saved file
#
setwd("C:/Users/scott.jennings/Documents/projects/HETP/HETP_working") # ACR local
# can read in downloaded data file (currently storing a current-ish version in the find_dups folder)
# can select option to include local timestamp in downlod from Movebank, and if so don't need the date fixing below

# 
hetp <- read_csv("data_files/ACR Heron and Egret telemetry project.csv")

names(hetp) <- (gsub(":", ".", names(hetp)))
names(hetp) <- (gsub("-", ".", names(hetp)))

hetpDF<-  hetp %>% 
  dplyr::select(event.id, 
                gps.timestamp = timestamp,
                lat = location.lat, 
                lon = location.long, 
                bird = individual.local.identifier) %>% 
  mutate(timestamp = with_tz(gps.timestamp, tzone = "America/Los_Angeles")) %>% 
  data.frame()
  


##############################################


### option 2, download all data from movebank, but THIS DOES NOT SEEM TO DOWNLOAD ACC DATA!!!
study = "ACR Heron and Egret telemetry project"
animal = "GREG_3"
login <- movebankLogin(username="scott.jennings", password="3grets0nT#eMove!")

hetp <- getMovebankData(study=study, login=login)

###-------------------------------------------

### option 2.1, download just a single animal  --- see animal assignment above
#greg3 <- getMovebankData(study=study, animalName = animal, login=login)
#hist(timeLag(greg3, units='mins'))




##############################################
## DATE FIX
## for some reason when downloading data from movebank, the timestamp is staying as GPS time
## this changes the 'timestamp' field to local time, and makes another field for GPS timestamp
## could do this in 2 lines (assign $gps.timestamp first, then change $timestamp), but keeping it as 3 lines to allow checking that it worked right
hetp$local.timestamp <- with_tz(timestamps(hetp), tzone = "America/Los_Angeles")
hetp$gps.timestamp <- hetp$timestamp
hetp$timestamp <- hetp$local.timestamp


##############################################
## make a data frame from the move object
## see the move vignete if more detail is needed on the difference between a move object and a dataframe (or tibble)
#hetpDF <- as(hetp, "data.frame") 

## write to
#write.csv(hetpDF, "C:/Users/scott.jennings/Desktop/hetp_temp/hetp.csv")
#hetpDF<- read.csv("C:/Users/scott.jennings/Desktop/hetp_temp/hetp.csv")

## make data frame and reduce to just a few fields in one step
## package raster also has select(), so need to specify dplyr here
hetpDF<-  as(hetp, "data.frame") %>% 
  dplyr::select(timestamp, lat=location_lat, lon=location_long, bird=local_identifier, event_id)


#### double check how many points for each bird each day. ideal max is 288. a few more is ok, many more is a problem
#hoo <- hetpDF %>% 
#  mutate(date = as.Date(ymd_hms(timestamp))) %>% 
#  group_by(bird, date) %>% 
#  summarise(n())




# if you read the csv back in, need  to fix the timestamp field again
#hetpDF$timestamp=as.POSIXct(as.character(hetpDF$timestamp), format="%Y-%m-%d %H:%M:%S")

#hetpDF$date <- as.Date(ymd_hms(hetpDF$timestamp))
