

library(tidyverse)
library(lubridate)

# read data downloaded from movebank, subset to bird X month X year chunks and write to RDS


# GPS ----
all_gps <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/HETP_GPSonly_201706_202007.csv") %>% 
  mutate(date = as.Date(as.character(study.local.timestamp)),
         study.local.timestamp = as.POSIXct(as.character(study.local.timestamp))) 

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

