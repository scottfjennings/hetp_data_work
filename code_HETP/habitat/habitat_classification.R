

library(tidyverse)
library(lubridate)

birdz = c(paste("GREG_", seq(1:11), sep = ""))
move_status = c("migratory", 
                "nomad", 
                "migratory", 
                "migratory", 
                "nomad", 
                "nomad", 
                "resident", 
                "nomad", 
                "resident", 
                "migratory", 
                "nomad")
tomales.nomad = c("no", 
                "yes", 
                "yes", 
                "migratory", 
                "yes", 
                "yes", 
                "resident", 
                "yes", 
                "resident", 
                "migratory", 
                "yes")
birdz_move <- data.frame(bird = birdz, move.status = move_status)
rm(birdz, move_status)
##########################

## assign cari habita classification

hetp_gps <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201912.csv") %>% 
  select(timestamp, location_lat, location_long, )




buffTom <- read.csv("data_files/habitat/buffTom.csv") %>% 
   rename_all(~ gsub("_", ".", .))

####
hetp_eel <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_cov_eel.csv") %>% 
  select(event_id, in.eelgrass = COUNT) %>% 
   rename_all(~ gsub("_", ".", .))

hetp_cari <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_cov_cari.csv") %>% 
  dplyr::select(event_id, leglabellevel1, leglabellvel2, legend_headings) %>% 
   rename_all(~ gsub("_", ".", .))

hetp_cari_eel <- full_join(hetp_cari, hetp_eel, by = c("event.id")) %>% 
  mutate(in.eelgrass = ifelse(is.na(in.eelgrass), 0, in.eelgrass))

hetp_cari_eel_buff <- full_join(hetp_cari_eel, buffTom, by = c("event.id"))

# hetp_use from hetp_covariate_filter.R
cov_cari_eel_buff <- full_join(hetp_use, hetp_cari_eel_buff, by = c("event.id")) %>%
  rename(bird = individual.local.identifier) %>% 
  full_join(., birdz_move, by = c("bird"))

cov_cari_eel_buff <- cov_cari_eel_buff %>% 
  mutate(inouteel = ifelse(in.eelgrass == 1, "In eelgrass", "Not in eelgrass")) %>% 
  mutate(wetland.type = ifelse(in.eelgrass == 1, "In eelgrass", "Terrestrial"),
         wetland.type = ifelse(legend.headings == "Estuarine and Marine" & in.eelgrass == FALSE, "Other estuarine/marine", eelinout),
         wetland.type = ifelse(legend.headings == "Palustrine and Riverine" & in.eelgrass == FALSE, "Palustrine/riverine wetlands", eelinout))

rm(hetp_eel, hetp_cari, hetp_cari_eel, hetp_cari_eel_buff, hetp_use, birdz_move, buffTom)
##------
ggplot(data = filter(cov_cari_eel_buff, inlight == TRUE, ground.speed < 4, within1Tom == TRUE)) +
  geom_violin(aes(x = inouteel, y = water.level)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ylab("Tidal height") +
  xlab("Daytime egret locations")
ggsave("figures_output/ineel_waterlevel_20190927square4violin.jpg", width = 4, height = 4, units = "in")
##------
ggplot(data = filter(cov_cari_eel_buff, inlight == TRUE, ground.speed < 4, within1Tom == TRUE)) +
  geom_violin(aes(x = wetland.type, y = water.level)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ylab("Tidal height") +
  xlab("Daytime egret locations")
ggsave("figures_output/wetlandtype_waterlevel_20190927violin.jpg", width = 8, height = 4, units = "in")
##------

ggplot(data = filter(cov_cari_eel_buff, inlight == TRUE, ground.speed < 4, within20Tom)) +
  geom_boxplot(aes(x = within5Tom, y = num.hours)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ylab("Number of daylight hours eelgrass available") +
  #xlab("Bird in eelgrass") +
  facet_wrap(~move.status)

rm(hetp_cari_eel, hetp_cari, hetp_cari_sub, hetp, hetp_covs, hetp_eel, hetp_eel_sub)

hetp_bird_day_eelsumm <- cov_cari_eel_buff %>% 
  filter(inlight == TRUE, ground.speed < 4, within5Tom == TRUE) %>% 
  group_by(bird, date) %>% 
  summarise(total.locs = n(),
            eel.locs = sum(in.eelgrass),
            prop.eel.forage = eel.locs/total.locs) %>% 
  ungroup()

day_num_hours <- cov_cari_eel_buff %>% 
  distinct(date, .keep_all = T)  %>% 
  mutate(num.day.hours = as.numeric(as.POSIXct(dusk.time) - as.POSIXct(dawn.time)),
            prop.eel.avail = num.hours/num.day.hours) %>% 
  dplyr::select(date, num.hours, num.day.hours, prop.eel.avail) %>% 
  filter(complete.cases(date)) %>% 
  ungroup() %>% 
  arrange(date)


eelsum_numhours <- full_join(hetp_bird_day_eelsumm, day_num_hours, by = c("date")) %>% 
  filter(!is.na(bird)) %>% 
  full_join(., birdz_move, by = c("bird"))

ggplot(aes(x = prop.eel.avail, y = prop.eel.forage, color = bird), data = eelsum_numhours) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(~move.status)  + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ylab("Proportion of day bird in eelgrass") +
  xlab("Proportion of day eelgrass available")
  
ggsave("figures_output/eelAvail_propEel_20190926.jpg", width = 8, height = 4, units = "in")


################

cov_cari_eel_buff_20km <- cov_cari_eel_buff %>% 
  filter(within20Tom == TRUE) %>% 
  mutate(tom1_5 = ifelse(within5Tom == TRUE & within1Tom == FALSE, TRUE, FALSE),
         tom5_10 = ifelse(within10Tom == TRUE & within5Tom == FALSE, TRUE, FALSE),
         tom5_10 = ifelse(within10Tom == TRUE & within5Tom == FALSE, TRUE, FALSE))







