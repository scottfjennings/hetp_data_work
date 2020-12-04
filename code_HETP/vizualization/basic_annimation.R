
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









## can't get align_move to work
##greg2_summer17_align <- moveVis::align_move(greg2_summer17_move)



greg2_summer17_merge_timefix <- greg2_summer17_merge %>% 
  mutate(orig.timestamp = timestamp,
         timestamp = round_date(as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"), "8 hours")) %>% 
  dplyr::select(orig.timestamp, timestamp, everything()) %>% 
  distinct(timestamp, .keep_all = T)

table(hour(greg2_summer17_merge_timefix$timestamp))


greg2_summer17_merge_timefix <- greg2_summer17_merge_timefix[2:340,]

greg2_summer17_move <- move(x = greg2_summer17_merge_timefix$location.long, 
                            y = greg2_summer17_merge_timefix$location.lat,
                            time=as.POSIXct(greg2_summer17_merge_timefix$timestamp,
                                            format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
                            data=greg2_summer17_merge_timefix, proj=CRS("+proj=longlat +ellps=WGS84"))

greg2_summer17_align <- moveVis::align_move(greg2_summer17_move)


frames <- frames_spatial(greg2_summer17_align, path_colours = c("red"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)

frames <- add_labels(frames, x = "Longitude", y = "Latitude") # add labels, e.g. axis labels
frames <- add_progress(frames) # add a progress bar
frames <- add_scalebar(frames, height = 0.015) # add a scale bar
frames <- add_northarrow(frames) # add a north arrow
frames <- add_timestamps(frames, greg2_summer17_align, type = "label") # add timestamps
frames <- add_gg(frames, gg = expr(theme(legend.position="none")))
frames <- add_text(frames, labels = paste("Eelgrass available", round(greg2_summer17_align@data$num.hours, 2), "hours per day", sep = " "), x = -122.87, y = 38.29, size = 8, type = "label")

frames[[150]]


animate_frames(frames, out_file = "C:/Users/scott.jennings/Documents/Projects/hetp/data_visualization/gifs/greg2_summer17_eelavail.mov")


