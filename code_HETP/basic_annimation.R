
## make basic animated map of bird movements

library(move)
library(moveVis)


setwd("C:/Users/scott.jennings/Documents/projects/HETP/HETP_working") # ACR local

study = "ACR Heron and Egret telemetry project"
animal = "GREG_3"
login <- movebankLogin(username="scott.jennings", password="3grets0nT#eMove!")

#getMovebankAnimals(study=study, login=login) # all animals

#download all data from movebank
hetp <- getMovebankData(study=study, login=login)

##############################################
## for some reason when downloading data from movebank, the timestamp is staying as GPS time
## this changes the 'timestamp' field to local time, and makes another field for GPS timestamp
## could do this in 2 lines (assign $gps.timestamp first, then change $timestamp), but keeping it as 3 lines to allow checking that it worked right
hetp$local.timestamp <- with_tz(timestamps(hetp), tzone = "America/Los_Angeles")
hetp$gps.timestamp <- hetp$timestamp
hetp$timestamp <- hetp$local.timestamp


# download just a single animal  --- see animal assignment above
greg3 <- getMovebankData(study=study, animalName = animal, login=login)
hist(timeLag(greg3, units='mins'))

##subset to each by = n record
greg3_each5<- greg3[seq(1, NROW(greg3), by = 5),]

##############################################
## make animation using moveVis
conv_dir <- get_imconvert()
out_dir <- "C:/Users/Scott/Dropbox/ACR/projects/HETP/HETP_working/data_visualization/gifs"
animate_move(data_ani=greg3_each5, out_dir=out_dir, conv_dir=conv_dir)
### this works but takes a long time to stitch the gif, and I haven't figured out yet how to add additional info
