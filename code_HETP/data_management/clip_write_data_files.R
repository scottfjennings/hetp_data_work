

library(tidyverse)
library(lubridate)
library(here)
library("data.table")   
  
source("https://raw.githubusercontent.com/scottfjennings/scotts_helper_functions/main/code/spacetime_utility_functions.R")


# read data downloaded from movebank, reduce fields, add dawn, dusk, inlight, and write to RDS ----
# this is the basic first step coming from downloaded Movebank GPS data
# data.table::fread is much faster for these big files than read.csv


all_gps <- fread("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/hetp/hetp_data_work/data_files/GPSonly/HETP_GPSonly.csv", check.names = TRUE)

keep_fields = c("location-long", "location-lat", "eobs:horizontal-accuracy-estimate", "eobs:speed-accuracy-estimate", "ground-speed", "tag-local-identifier", "individual-local-identifier", "utm-easting", "utm-northing", "utm-zone", "study-timezone", "study-local-timestamp")

  all_gps <- fread("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/hetp/hetp_data_work/data_files/GPSonly/HETP_GPSonly.csv", select = keep_fields, check.names = TRUE) %>% 
  mutate(date = as.Date(as.character(study.local.timestamp)),
         study.local.timestamp = as.POSIXct(as.character(study.local.timestamp))) 

  all_gps2 <- fread("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/hetp/hetp_data_work/data_files/GPSonly/HETP_GPSonly_2022onward.csv", select = keep_fields, check.names = TRUE) %>% 
    mutate(date = as.Date(as.character(study.local.timestamp)),
           study.local.timestamp = as.POSIXct(as.character(study.local.timestamp))) 
  
all_gps_out <- bind_rows(all_gps, all_gps2) %>% 
  distinct() %>% 
  data.frame() %>% 
  rename("timestamp" = study.local.timestamp, "latitude" = location.lat, "longitude" = location.long) %>% 
  filter(!is.na(latitude)) %>% 
  add_dawn_dusk_inlight()

saveRDS(all_gps_out, here("data_files/rds/gps_with_covariates"))

  
# ----  

all_bird_month_year <- distinct(all_gps, individual.local.identifier, month(date), year(date)) %>% 
  rename(bird = 1, month = 2, year = 3)

split_gps_bird_month <- function(zbird, zmonth, zyear) {
zbird_month <- all_gps%>% 
  filter(individual.local.identifier == zbird, month(study.local.timestamp) == zmonth, year(study.local.timestamp) == zyear) %>% 
  dplyr::select(event.id, timestamp, location.long, location.lat, eobs.horizontal.accuracy.estimate, gps.dop, gps.satellite.count, ground.speed, individual.local.identifier, study.timezone, study.local.timestamp)
 saveRDS(zbird_month, paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_gps/", zbird, "_GPS_", zmonth, "_", zyear, sep = ""))
}

pmap(list(all_bird_month_year$bird, all_bird_month_year$month, all_bird_month_year$year), split_gps_bird_month)



# ACC
all_acc <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/ACConly/HETP_ACConly_201706_202007.csv") %>% 
  mutate(date = as.Date(as.character(study.local.timestamp)),
         study.local.timestamp = as.POSIXct(as.character(study.local.timestamp))) 

# generate a bird X day summary of ACC operations (axes, sampling rate, bursts per day) --
all_acc2 <- all_acc %>% 
  dplyr::select(-timestamp, -study.timezone) %>% 
  mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles"),
         date = as.Date(timestamp, tz = "America/Los_Angeles"))

acc_checker <- distinct(all_acc2, date, individual.local.identifier, eobs.acceleration.axes, eobs.acceleration.sampling.frequency.per.axis) %>% 
  data.frame()
bursts_per_day <- all_acc2 %>% 
  group_by(date, individual.local.identifier) %>% 
  summarise(bursts.per.day = n())

acc_summary <- full_join(acc_checker, bursts_per_day)
saveRDS(acc_summary, "data_files/rds/acc_summary")



# each birds X year X month combo in the data
# acc_summary made by clip_write_data_files.R
# this used for looping below
bird_month <- readRDS("data_files/rds/acc_summary") %>% 
  distinct(individual.local.identifier, month(date), year(date)) %>% 
  rename(bird = individual.local.identifier, month = 2, year = 3)

saveRDS(bird_month, "data_files/rds/bird_month")

# --
all_bird_month_year <- distinct(all_acc, individual.local.identifier, month(study.local.timestamp), year(study.local.timestamp)) %>% 
  rename(bird = 1, month = 2, year = 3)

split_acc_bird_month <- function(zbird, zmonth, zyear) {
zbird_month <- all_acc%>% 
  filter(individual.local.identifier == zbird, month(date) == zmonth, year(date) == zyear) %>% 
  dplyr::select(event.id, timestamp, eobs.acceleration.axes, eobs.acceleration.sampling.frequency.per.axis, eobs.accelerations.raw, individual.local.identifier, study.timezone, study.local.timestamp)
 saveRDS(zbird_month, paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_acc/", zbird, "_ACC_", zmonth, "_", zyear, sep = ""))
}

pmap(list(all_bird_month_year$bird, all_bird_month_year$month, all_bird_month_year$year), split_acc_bird_month)


split_acc_bird_month("GREG_1", "6", "2017")

