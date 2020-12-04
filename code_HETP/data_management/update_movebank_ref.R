


library(tidyverse)
library(lubridate)

ibr_greg_nums <- seq(12, 17)
ibr_gregs <- paste("GREG_", ibr_greg_nums, sep = "") %>% 
  data.frame() %>% 
  rename(ibr.gregs = 1)



ref_dat <- read.csv("movebank/ACR Heron and Egret telemetry project-reference-data.csv")

ref_dat <- ref_dat %>%
  filter(!is.na(animal.id))


ref_dat[ref_dat==""] <- NA
ref_dat_edit <- ref_dat %>% 
  mutate(#tag.id, # from ref_dat
         # animal.id # join field  
         # animal.taxon, # from ref_dat
         deploy.on.date = as.POSIXct(deploy.on.date), # from banding
         # deploy.off.date,
         # animal.comments,
         # animal.death.comments, 
         # animal.exact.date.of.birth,
         # animal.latest.date.born,
         # animal.life.stage, # from banding
         # animal.mass, # from banding
         # animal.nickname,
         # animal.reproductive.condition,
         # animal.ring.id, # from banding
         # animal.sex, # from banding
         # animal.taxon.detail,
         attachment.type = ifelse(is.na(attachment.type), "harness", attachment.type),
         # behavior.according.to,
         # data.processing.software,
         # deploy.off.latitude,
         # deploy.off.longitude,
         # deploy.off.person,
         # deploy.on.latitude, # from trap_sites
         # deploy.on.longitude, # from trap_sites
         # deploy.on.person,
         # deployment.comments, # from banding
         # deployment.end.comments, 
         # deployment.end.type,
         deployment.id = ifelse(is.na(deployment.id), paste(animal.id, tag.id, sep = "_"), deployment.id),
         # duty.cycle,
         # geolocator.calibration,
         # geolocator.light.threshold,
         # geolocator.sensor.comments,
         # geolocator.sun.elevation.angle,
         # habitat.according.to,
         # location.accuracy.comments,
         # manipulation.comments,
         # manipulation.type,
         # study.site,
         # tag.beacon.frequency,
         # tag.comments,
         # tag.failure.comments,
         tag.manufacturer.name = ifelse(is.na(tag.manufacturer.name), "e-obs", tag.manufacturer.name),
         tag.mass = ifelse(is.na(tag.mass), ifelse(tag.id < 6000, 48, 25), tag.mass),
         tag.model = ifelse(is.na(tag.model), ifelse(tag.id < 6000, "Bird Solar 48", "Bird UMTS 25g"), tag.model)
         # tag.processing.type,
         # tag.production.date,
         # tag.readout.method,
         #tag.serial.no
         )



# fill info from banding data csv ----

trap_sites <- read.csv("banding/trapping_sites.csv")
banding <- read.csv("banding/HETP_banding.csv") %>% 
  rename(location.id = location.code)


temp_gps <- read.csv("banding/hetp_gps_temp.csv")

ibr_first_loc <- temp_gps %>% 
  filter(individual.local.identifier %in% ibr_gregs$ibr.gregs, !is.na(eobs.fix.battery.voltage)) %>% 
  mutate(study.local.timestamp = as.POSIXct(study.local.timestamp)) %>% 
  group_by(individual.local.identifier) %>% 
  filter(study.local.timestamp == min(study.local.timestamp)) %>% 
  ungroup() %>% 
  select(individual.local.identifier, everything()) %>% 
  arrange(individual.local.identifier) %>% 
  select(Bird.ID = individual.local.identifier, long.dd = location.long, lat.dd = location.lat, Tag.number = tag.local.identifier, study.local.timestamp) %>% 
  mutate(location.id = "IBR")
  

wild_banding <- banding %>% 
  filter(!Bird.ID %in% ibr_gregs$ibr.gregs, !is.na(Bird.ID)) %>%
  mutate(date = as.POSIXct(paste(Date, Time.released), format = "%m/%d/%Y %H:%M")) %>% 
  select(date, Bird.ID, Band.number, Tag.number, Bird.mass, Location, location.id, Notes, Sex, animal.life.stage) %>% 
  left_join(select(trap_sites, location.id, lat.dd, long.dd), by = c("location.id")) %>% 
  mutate(deployment.comments = NA)
                           
                                               
ibr_banding <- banding %>% 
  filter(Bird.ID %in% ibr_gregs$ibr.gregs, !is.na(Bird.ID)) %>% 
  select(Date, Bird.ID, Band.number, Bird.mass, Location, location.id, Notes, Sex, animal.life.stage) %>% 
  left_join(., ibr_first_loc, by = c("Bird.ID", "location.id")) %>% 
  rename(date = study.local.timestamp) %>% 
  mutate(deployment.comments = paste("Bird tagged ", as.character(Date), "; bird released ", as.character(date(date), sep = "")),
         Notes = paste(Notes, Location)) %>% 
  select(-Date)


wild_names <- data.frame(wild.names = names(wild_banding))
ibr_names <- data.frame(ibr.names = names(ibr_banding))

filter(wild_names, !wild.names %in% ibr_names$ibr.names)
filter(ibr_names, !ibr.names %in% wild_names$wild.names)


# combine wild and IBR bird banding/location info and make names match reference data names  
all_banding <- rbind(wild_banding, ibr_banding) %>% 
  rename_all(tolower) %>% 
  rename(animal.id = bird.id,
         animal.comments = notes,
         animal.mass = bird.mass,
         animal.ring.id = band.number,
         animal.sex = sex,
         deploy.on.latitude = lat.dd, 
         deploy.on.longitude = long.dd) %>% 
  select(-tag.number, -location, -location.id, -date)


ref_dat_out <- ref_dat_edit %>% 
    full_join(., all_banding, by = c("animal.id")) %>% 
  select(matches(names(ref_dat))) 

ref_dat_out2 <- ref_dat_out %>% 
  mutate(animal.comments = paste(animal.comments.x, animal.comments.y),
         animal.life.stage = ifelse(is.na(animal.life.stage.x), animal.life.stage.y, animal.life.stage.x),
         animal.mass = ifelse(is.na(animal.mass.x), animal.mass.y, animal.mass.x),
         animal.ring.id = ifelse(is.na(animal.ring.id.x), animal.ring.id.y, animal.ring.id.x),
         animal.sex = ifelse(is.na(animal.sex.x), animal.sex.y, animal.sex.x),
         animal.sex = tolower(animal.sex),
         animal.ring.id = gsub("-", "", animal.ring.id),
         deploy.on.latitude = ifelse(is.na(deploy.on.latitude.x), deploy.on.latitude.y, deploy.on.latitude.x),
         deploy.on.longitude = ifelse(is.na(deploy.on.longitude.x), deploy.on.longitude.y, deploy.on.longitude.x),
         deployment.comments = ifelse(is.na(deployment.comments.x), deployment.comments.y, deployment.comments.x)) %>% 
  select(matches(names(ref_dat))) 


ref_dat_out3 <- ref_dat_out2 %>% 
  select(-ends_with(".x"), -ends_with(".y")) %>% 
  select(matches(names(ref_dat))) 

  

in_names <- data.frame(in.names = names(ref_dat))
out_names <- data.frame(out.names = names(ref_dat_out))
out_names2 <- data.frame(out.names = names(ref_dat_out2))
out_names3 <- data.frame(out.names = names(ref_dat_out3))
out_names_dup <- ref_dat_out %>% 
  select(ends_with(".x")) %>% 
  select(matches(names(ref_dat))) %>% 
  names() %>% 
  data.frame() %>% 
  rename(out.names.dup = 1) 

filter(out_names, grepl("\.x|\.y", out.names))

filter(out_names, !out.names %in% in_names$in.names)
filter(in_names, !in.names %in% out_names$out.names)
filter(in_names, !in.names %in% out_names3$out.names)


out_test = data.frame(ref_dat == ref_dat_out3)

write.csv(ref_dat_out, "movebank/edited_ref_data.csv", row.names = F)
