

# calculate distance travelled by each bird each day
library(GISTools)
library(tidyverse)
library(dplyr)
library(rgdal)
library(move)
library(lubridate)
library(ggmap)
library(lme4)
options(scipen = 999)
## works best to use move() to load the file from disk, but need to deal with duplicate records first


# 2 read data ----
read.csv("data_files/GPSonly/HETP_GPSonly_201706_202001.csv") %>% 
  dplyr::filter(!is.na(eobs.fix.battery.voltage)) %>% 
  write.csv("data_files/GPSonly/HETP_GPSonly_nodupstamp.csv", row.names = F)

hetp <- move("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/HETP_GPSonly_nodupstamp.csv")

mig_status <- data.frame(bird	= c("GREG_1", "GREG_2", "GREG_3", "GREG_4", "GREG_5", "GREG_6", "GREG_7", "GREG_8", "GREG_9", "GREG_10", "GREG_11"),
                         migratory = c("migratory", "non-migratory", "migratory", "nomad", "nomad", "non-migratory", "unknown", "nomad", "unknown", "migratory", "nomad"))


# 3.1 movement, all fixes ----
hetp$distance <- unlist(lapply(distance(hetp), c, NA))


hetp_covs <- data.frame(hetp) %>%
  dplyr::select(event.id, distance, study.local.timestamp) %>% 
  left_join(., foo <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201912.csv"))



hetp_covs <- hetp_covs %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, tz = "America/Los_Angeles"),
                dawn.time = as.POSIXct(dawn.time, tz = "America/Los_Angeles"),
                date = as.Date(date),
                sunrise_day = ifelse(timestamp < dawn.time, date - 1, date),
                sunrise_day = as.Date(sunrise_day, origin = "1970-01-01")) 


hetp_covs2 <- hetp_covs %>%
  arrange(bird, timestamp) %>% 
  group_by(bird) %>% 
  mutate(step.time = ((timestamp - lag(timestamp))/60)/60,
         step.time = as.numeric(step.time),
         step.speed = (distance/1000)/step.time,
         rnd.step.speed = plyr::round_any(step.speed, 5, floor)) %>% 
  ungroup() %>% 
  filter(step.time > 0) %>% 
  dplyr::select(event.id, distance, location_lat, location_long, bird, utm.easting, utm.northing, timestamp, water.level, dawn.time, dusk.time, inlight, num.hours, sunrise_day, step.time, step.speed, rnd.step.speed) %>% 
  full_join(., mig_status) %>% 
  filter( step.speed <= 55)


ggplot(data = filter(hetp_covs2, inlight == TRUE)) +
  geom_histogram(aes(distance), binwidth = 100) +
  facet_wrap(~migratory) 





bird_day_dist_summ <- hetp_covs2 %>% 
  filter(step.speed <= 55) %>% 
  group_by(bird, sunrise_day, inlight) %>% 
  summarise(max.dist = round(max(distance), 0),
            min.dist = round(min(distance), 1),
            mean.dist = round(mean(distance), 0),
            sd.dist = round(sd(distance), 0),
            total.dist = round(sum(distance), 0)) %>% 
  ungroup() %>% 
  mutate(zmonth = month(sunrise_day))

ggplot(data = bird_day_dist_summ)+
  geom_boxplot(aes(x = zmonth, y = total.dist, group = zmonth)) +
  facet_wrap(~bird)

ggplot(data = filter(bird_day_dist_summ, inlight == "FALSE")) +
  geom_histogram(aes(mean.dist), binwidth = 5) +
  facet_wrap(~bird)
# ----- 

bird_month_dist_summ <- hetp_covs %>% 
  filter(!is.na(distance)) %>% 
  mutate(zyear = year(date),
         zmonth = month(date)) %>% 
  group_by(bird, zyear, zmonth, inlight) %>% 
  summarise(max.dist = round(max(distance), 0),
            min.dist = round(min(distance), 1),
            mean.dist = round(mean(distance), 0),
            sd.dist = round(sd(distance), 0),
            total.dist = round(sum(distance), 0))  %>% 
  ungroup() 

# ----- 

mig_month_dist_summ <- hetp_covs2 %>% 
  filter(!is.na(distance)) %>% 
  mutate(zyear = year(timestamp),
         zmonth = month(timestamp)) %>% 
  group_by(migratory, zyear, zmonth, inlight) %>% 
  summarise(max.dist = round(max(distance), 0),
            min.dist = round(min(distance), 1),
            mean.dist = round(mean(distance), 0),
            sd.dist = round(sd(distance), 0),
            total.dist = round(sum(distance), 0))  %>% 
  ungroup() 
# ----- 

mig_dist_summ <- bird_day_dist_summ %>% 
  full_join(., mig_status) %>% 
  group_by(migratory, inlight) %>% 
  summarise(mean.max.distance = mean(max.dist),
            sd.max.distance = sd(max.dist),
            mean.total.distance = mean(total.dist),
            sd.total.distance = sd(total.dist),
            mean.mean.distance = mean(mean.dist),
            sd.mean.distance = sd(mean.dist))

# ----- 

mig_dist <- bird_day_dist_summ %>% 
  full_join(., mig_status)
mig_dist_day_lm <- lm(max.dist ~ migratory, data = filter(mig_dist, inlight == TRUE))

#dist_summ_all <- dist_summ
dist_summ_all <- rbind(dist_summ_all, dist_summ)
table(dist_summ_all$bird)


write.csv(dist_summ_all, "data_files/distance_summaries.csv")

#####
obs_dates <- dist_summ %>% 
  group_by(bird) %>% 
  distinct(sunrise_day) %>% 
  summarise(first.date = min(sunrise_day),
            last.date = max(sunrise_day),
            days.tracked = n())

write.csv(obs_dates, "data_files/observation_dates.csv")

# 4 movement, 1 fix per night ----

foo <- read.csv("data_files/GPSonly/HETP_GPSonly_201706_202001.csv") %>% 
  dplyr::filter(!is.na(eobs.fix.battery.voltage)) %>% 
  dplyr::filter(hour(timestamp) < 5 | hour(timestamp) > 20)
%>% 
  write.csv("data_files/GPSonly/midnight_locs_nodupstamp.csv", row.names = F)

midnight_locs <- move("data_files/GPSonly/midnight_locs_nodupstamp.csv")


midnight_locs$distance <- unlist(lapply(distance(midnight_locs), c, NA))



distances_1day <- midnight_locs %>%
  as.data.frame() %>% 
  dplyr::select(bird = individual.local.identifier, location.lat, location.long, timestamp, distance) %>% 
  full_join(mig_status) %>% 
  arrange(bird, timestamp) %>% 
  group_by(bird) %>% 
  mutate(loc_gap = lead(timestamp) - timestamp,
         loc_gap = as.numeric(loc_gap),
         loc_gap = round(loc_gap/60/60/24, 0)) %>% 
  ungroup() %>% 
  mutate(distance = ifelse(loc_gap > 1, NA, distance))

foo <- distances_1day %>% 
  filter(!(is.na(distance))) %>% 
  group_by(migratory) %>% 
  summarise(mean.dist = mean(distance),
            max.dist = max(distance),
            min.dist = min(distance))

distances_1day %>% 
  filter(!(is.na(distance))) %>% 
  ggplot() +
  geom_boxplot(aes(x = migratory, y = distance))+
  facet_wrap(~migratory, scales = "free")

dist1day_lmm <- lmer(distance ~ migratory + (1|bird), data = distances_1day)


dist_g10km <- distances_1day %>% 
  filter(distance > 10000 | date == min(date) | date == max(date))


nomads_10km <- read.csv("data_files/nomads_10km.csv") %>% 
  #dplyr::select(bird, location.lat, location.long, date, distance) %>% 
  mutate(date = as.Date(date))

nomads_10km <- rbind(nomads_10km, dist_g10km)

write.csv(nomads_10km, "data_files/nomads_10km.csv", row.names = F)

rm(df, df_nightlocs, nomads_10km, dist_g10km, df_nightlocs_dist, df_nightlocs_move, dist_g30km, distances_1day, migrators_30km, zparms)


################################
migrators_30km_coords <- migrators_30km %>% 
  dplyr::select(location.long, location.lat)

migrators_30km_nocoords <- migrators_30km %>% 
  dplyr::select(-location.lat, -location.long)
migrators_30km_sp <- SpatialPointsDataFrame(migrators_30km_coords, migrators_30km_nocoords, proj4string = CRS("+proj=longlat"))

writeOGR(obj = migrators_30km_sp, dsn = "C:/Users/scott.jennings/Documents/Projects/hetp/esri_maps", layer="migrators_30km", driver="ESRI Shapefile") # this is in geographical projection

