### code for basic data checking

library(tidyverse)
library(lubridate)
library(geosphere)

options(scipen = 999)
# GPS data right form movebank

wild_gregs <- paste("GREG_", seq(1, 11), sep = "")


hetp_gps <- read.csv("data_files/GPSonly/HETP_GPSonly.csv")

cgrc_ll = c(-122.900821, 38.165640)




hetp_night_locs <- hetp_gps %>% 
  select(event.id, accuracy = eobs.horizontal.accuracy.estimate, ground.speed, bird = individual.local.identifier, location.long, location.lat, utm.easting, utm.northing, utm.zone, study.local.timestamp) %>% 
  mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles")) %>% 
  filter((hour(timestamp) < 2 | hour(timestamp) > 22), accuracy <= 10) 

night_mean_locs <- hetp_night_locs %>% 
  mutate(zdate = as.Date(timestamp, tz = "America/Los_Angeles"),
         znight = ifelse(hour(timestamp) > 20, as.character(zdate), as.character(zdate - 1)),
         znight = as.Date(znight, tz = "America/Los_Angeles")) %>% 
  group_by(bird, znight) %>% 
  summarise(mean.long = mean(location.long),
            sd.long = sd(location.long),
            mean.lat = mean(location.lat),
            sd.lat = sd(location.lat),
            num.loc = n()) %>% 
  ungroup()


night_locs_no_move <- night_mean_locs %>% 
  filter(sd.long <= 0.00009 , sd.lat <= 0.00009) %>% 
  mutate(dist.cgrc = distGeo(cbind(mean.long, mean.lat),
                             cgrc_ll),
         dist.cgrc = dist.cgrc/1000) %>% 
  group_by(bird) %>% 
  mutate(daily.disp = distGeo(cbind(mean.long, mean.lat),
                              cbind(lag(mean.long), lag(mean.lat))),
         day.lag = znight - lag(znight),
         day.lag = as.numeric(day.lag),
         av.daily.disp = daily.disp/day.lag)
  
         
night_locs_no_move %>% 
  filter(bird %in% wild_gregs) %>% 
ggplot() +
  geom_histogram(aes(x = dist.cgrc)) +
  facet_wrap(~bird, scales = "free") +
  xlab("km from CGRC") +
  ylab("number of nights")

ggsave("figures_output/nightly_distance_from_CGRC_hists.png", width = 10, height = 10)

filter(night_locs_no_move, bird == "GREG_10", dist.cgrc > 100) %>% summarise(mean.dist = mean(dist.cgrc))

filter(night_locs_no_move, bird == "GREG_1", dist.cgrc > 920, dist.cgrc < 930) %>% nrow()




night_locs_no_move %>% 
  filter(bird %in% wild_gregs) %>% 
ggplot() +
  geom_histogram(aes(x = av.daily.disp)) +
  facet_wrap(~bird) +
  xlab("daily displacement (m)") +
  ylab("number of nights")

ggsave("figures_output/daily_displacement_distance_hists.png", width = 10, height = 10)



night_locs_no_move %>% 
  filter(bird %in% wild_gregs, daily.disp < 5000) %>% 
  ggplot() +
  geom_point(aes(x = mean.long, y = mean.lat)) +
  facet_wrap(~bird)


  
 night_locs_no_move %>% 
  filter(bird %in% wild_gregs)  %>% 
   ggplot() +
   geom_point(aes(x = znight, y = dist.cgrc, color = bird)) +
  #facet_wrap(~bird, scales = "free") +
   ylab("Distance from CGRC (km)") +
   xlab("")
ggsave("figures_output/cgrc_distance_timeseries_spp_by_color.png", width = 14, height = 10)



 night_locs_no_move %>% 
  filter(bird %in% wild_gregs)  %>% 
   group_by(bird, year(znight), month(znight)) %>% 
   summarise(mean.month.dist = mean(dist.cgrc)) %>% 
   rename(year = 2, month = 3) %>% 
   ggplot(group = bird) +
   geom_line(aes(x = month, y = mean.month.dist, linetype = as.factor(year))) +
   scale_x_continuous(breaks = seq(1:12)) +
  facet_wrap(~bird, scales = "free") +
   ylab("Distance from CGRC (km)") +
   xlab("Month")
ggsave("figures_output/cgrc_distance_month_year_mean_spp_by_facet.png", width = 14, height = 10)





 night_locs_no_move %>% 
  filter(bird %in% wild_gregs)  %>% 
   group_by(bird, month(znight)) %>% 
   summarise(mean.month.dist = mean(dist.cgrc)) %>% 
   rename(month = 2) %>% 
   ggplot(group = bird) +
   geom_line(aes(x = month, y = mean.month.dist)) +
   scale_x_continuous(breaks = seq(1:12)) +
  facet_wrap(~bird, scales = "free") +
   ylab("Distance from CGRC (km)") +
   xlab("Month")
ggsave("figures_output/cgrc_distance_month_mean_spp_by_facet.png", width = 14, height = 10)
