### code for basic data checking

library(tidyverse)
library(lubridate)
library(here)
library(maptools)
library(chron)


# GPS data right form movebank

hetp_gps <- read.csv("data_files/GPSonly/HETP_GPSonly.csv")


# read GPS data downloaded from movebank, add dawn/dusk times and classify each point as in daylight or not, and write to RDS ----

df <- hetp_gps  %>% 
  mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles"),
         date = as.Date(timestamp, tz = "America/Los_Angeles")) %>%
  dplyr::select(event.id, "location_lat" = location.lat, "location_long" = location.long, timestamp, "bird" = individual.local.identifier, utm.easting, utm.northing, utm.zone, ground.speed) %>% 
  filter(!is.na(location_lat) | !is.na(location_long)) %>% 
  filter(location_long < -100) %>% 
  filter(location_lat > 20) 

# need to load these function form add_covariates.R
df2 <- df %>% 
  add_dawn_dusk()

df3 <- df2 %>% 
  assign_inlight()

saveRDS(df3, here("data_files/GPS_with_covariates/GPS_dawn_dusk"))




# make distinct list of bird ID and tag numbers
# hetp_gps %>% distinct(individual.local.identifier, tag.local.identifier) %>% rename(bird = individual.local.identifier, tag = tag.local.identifier) %>% saveRDS("data_files/rds/birdID_tagnum")


acc_data <- read.csv("data_files/ACConly/HETP_ACConly_201809.csv")


axis_summary <- acc_data %>% 
  distinct(individual.local.identifier, eobs.acceleration.axes)




# save a smaller subset of the data for testing ----
hetp_sub <- gps %>% 
  dplyr::select(event.id, bird, timestamp, location_lat, location_long, water.level, utm.easting, utm.northing, inlight) %>% 
  mutate(timestamp = as.POSIXct(timestamp)) %>% 
  filter(year(timestamp) == 2018, month(timestamp) %in% seq(4, 8))

write.csv(hetp_sub, "data_files/GPS_with_covariates/hetpGPS_with_covariates_subset.csv", row.names = F)

# ----
foo <- gps %>% 
  mutate(timestamp = as.POSIXct(timestamp)) %>% 
  mutate(tag.bird = paste("tag", tag, bird, sep = "")) %>% 
  group_by(zdate = date(timestamp), tag.bird) %>% 
  summarise(num.gps.fix = n())  

foo.wide <- foo %>% 
  spread(tag.bird, num.gps.fix) %>% 
  select(zdate, ends_with("G_1"), ends_with("G_2"), ends_with("G_3"), ends_with("G_4"), ends_with("G_5"), ends_with("G_6"), ends_with("G_7"), ends_with("G_8"), ends_with("G_9"), ends_with("G_10"))


tag.fix.summary <- foo %>% 
  group_by(tag.bird) %>% 
  summarise(mean.fix = mean(num.gps.fix),
            sd.fix = sd(num.gps.fix)) %>% 
  arrange(mean.fix)




