### code for basic data checking

library(tidyverse)
library(lubridate)


# GPS data right form movebank

hetp_gps <- read.csv("data_files/GPSonly/HETP_GPSonly.csv")


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

,
            max.fix = max(num.gps.fix),
            min.fix = min(num.gps.fix))


###################

foo <- table(bird_month_dist_summ$bird, bird_month_dist_summ$zmonth, bird_month_dist_summ$zyear) %>% data.frame()
> View(foo)
> foo <- arrange(foo, Var1, Var3)
> foo <- arrange(foo, Var1, Var3, Var2)




