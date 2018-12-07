
## imports monthly HETP GPS csv's, combines into 1 object, reduces columns, resaves combined csv
library(tidyverse)


hetp_gps_reader <- function(zfile){
zfile_path <- paste("C:/Users/scott.jennings/Dropbox (Audubon Canyon Ranch)/SJ_files/HETP/data_files/GPSonly/", zfile, sep = "")
  tide_table = read.csv(zfile_path) %>% 
  select(gps.timestamp = timestamp, location_lat = location.lat, location_long = location.long, tag = tag.local.identifier, bird = individual.local.identifier, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp = study.local.timestamp)
}

hetp_gps <- map_df(list.files("C:/Users/scott.jennings/Dropbox (Audubon Canyon Ranch)/SJ_files/HETP/data_files/GPSonly/"), hetp_gps_reader)






gps_1706 <- read.csv("data_files/GPSonly/HETP_GPSonly_201706.csv")
gps_1707 <- read.csv("data_files/GPSonly/HETP_GPSonly_201707.csv")
gps_1708 <- read.csv("data_files/GPSonly/HETP_GPSonly_201708.csv")
gps_1709 <- read.csv("data_files/GPSonly/HETP_GPSonly_201709.csv")
gps_1710 <- read.csv("data_files/GPSonly/HETP_GPSonly_201710.csv")
gps_1711 <- read.csv("data_files/GPSonly/HETP_GPSonly_201711.csv")
gps_1712 <- read.csv("data_files/GPSonly/HETP_GPSonly_201712.csv")
gps_1801 <- read.csv("data_files/GPSonly/HETP_GPSonly_201801.csv")
gps_1802 <- read.csv("data_files/GPSonly/HETP_GPSonly_201802.csv")
gps_1803 <- read.csv("data_files/GPSonly/HETP_GPSonly_201803.csv")
gps_1804 <- read.csv("data_files/GPSonly/HETP_GPSonly_201804.csv")
gps_1805 <- read.csv("data_files/GPSonly/HETP_GPSonly_201805.csv")
gps_1806 <- read.csv("data_files/GPSonly/HETP_GPSonly_201806.csv")
gps_1807 <- read.csv("data_files/GPSonly/HETP_GPSonly_201807.csv")



hetp <- bind_rows(gps_1706, gps_1707, gps_1708, gps_1709, gps_1709, gps_1710, gps_1711, gps_1712, gps_1801, gps_1802, gps_1803, gps_1804, gps_1805, gps_1806, gps_1807)


hetp_use <- hetp %>% 
  select(gps.timestamp = timestamp, location_lat = location.lat, location_long = location.long, tag = tag.local.identifier, bird = individual.local.identifier, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp = study.local.timestamp)

write.csv(hetp_use, "data_files/GPSonly/HETP_GPSonly_201706_201807.csv", row.names = F)

#############################

## append new monthly csv

gps_1807 <- read.csv("data_files/GPSonly/HETP_GPSonly_201807.csv")


%>% 
  select(gps.timestamp = timestamp, location.lat, location.long, tag = tag.local.identifier, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp = study.local.timestamp)

HETP_GPSonly_201706_201806 <- read.csv("data_files/GPSonly/HETP_GPSonly_201706_201806.csv")


HETP_GPSonly_201706_201807 <- bind_rows(HETP_GPSonly_201706_201806, gps_1807)


