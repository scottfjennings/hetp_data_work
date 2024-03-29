
# 1 description ----
## functions to create some covariates and merge with hetp data


# add ODBA ----

add_odba <- function(hetp_df) {
  odba <- readRDS("data_files/rds/odba/odba") %>% 
    rename(timestamp = study.local.timestamp,
           bird = bird.id) %>% 
    mutate(timestamp.min = round(timestamp, "mins"),
           timestamp.min = as.POSIXct(timestamp.min),
           timestamp.rnd.err = abs(timestamp - timestamp.min)) %>% 
    group_by(timestamp.min, bird) %>% 
    filter(timestamp.rnd.err == min(timestamp.rnd.err))
  
  hetp_df <- hetp_df %>% 
    mutate(timestamp.min = round(timestamp, "mins"),
           timestamp.min = as.POSIXct(timestamp.min)) %>% 
    full_join(., odba, by = c("bird", "timestamp.min"))
  
}

## make eelgrass.available -- currently not including this for habitat selection analysis ----
## estimate the number of daylight hours per day that the eelgrass beds are available to foraging egrets
## error on this estimate is up to 10 min with the 5 min tide data
# requires input to have columns date, inlight (from assign_inlight())
blake4eelavail <- blake5min_tides %>% 
  mutate(date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  full_join(blake.dawn.dusk, by = c("date")) %>% 
  assign.inlight()


add_eelgrass_hours_day <- function(df, water.level) 
 blake_eelgrass_available <- df %>% 
   group_by(date) %>% 
   filter(inlight=="TRUE" & water.level <= water.level) %>%
   summarise(num.hours = (n()*5/60)) %>% 
   full_join(dplyr::select(blake.dawn.dusk, date), by = c("date")) %>% 
   mutate(num.hours = ifelse(is.na(num.hours), 0, num.hours)) %>% 
   arrange(date)

write.csv(blake_eelgrass_available, paste("data_files/habitat/blake_eelgrass_available", Sys.Date(), ".csv", sep = ""), row.names = FALSE)


# pipe together functions ----


#hetp_for_analysis 
df <- readRDS("data_files/GPSonly/all_gps_20230806") %>%
  filter(!is.na(location.long))
  add_dawn_dusk() %>% 
  assign_inlight()

saveRDS(hetp_for_habitat_sel, "data_files/rds/ready_for_analysis/hetp_for_habitat_sel")



hetp_for_habitat_sel <- readRDS("data_files/rds/bird_tides/bird_tides") %>% 
  mutate(date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  filter(!is.na(location_lat) & !is.na(location_long)) %>% 
  add_dawn_dusk() %>% 
  assign_inlight()

saveRDS(hetp_for_habitat_sel, "data_files/rds/ready_for_analysis/hetp_for_habitat_sel")

hetp_for_odba <- read.csv("data_files/GPSonly/HETP_GPSonly.csv") %>%  
  mutate(timestamp = as.POSIXct(as.character(timestamp))) %>% 
  select(event.id, timestamp = study.local.timestamp, bird = individual.local.identifier, contains("location"), contains("utm")) %>%   
  mutate(timestamp = as.POSIXct(as.character(timestamp))) %>% 
  add_odba()

saveRDS(hetp_for_odba, "data_files/rds/ready_for_analysis/hetp_for_odba")



hetpDF_use <- make.hetpDF_use(hetp_dd_inlight) %>% 
  select(gps.timestamp, location_lat, location_long, tag, bird, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp, water.level, date, dawn.time, dusk.time, inlight, num.hours)


#prev.hetp <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201812.csv")

#new.hetp <- rbind(prev.hetp, hetpDF_use)

write.csv(hetpDF_use, "data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201912.csv", row.names = F)



# make a light data frame for shiny webmap

# starting from hetpDF_use in the environment
hetp <- hetpDF_use %>% 
  select(date, ztimestamp = timestamp, lat = location_lat, lon = location_long, bird, mo.da, hr.mn.sec, water.level, inlight)

# or reloading the saved hetpDF_use, need to fix the format for the date fields
#hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201901.csv") %>% mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"))

## add a couple more covariates
hetp <- hetpDF_use %>% 
  mutate(timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
         mo.da = paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep = ""),
         #hr=hour(timestamp),
         #hr.mn=as.POSIXct(paste(hour(timestamp), ":", minute(timestamp), ":00", sep=""), format="%H:%M:%S"),
         hr.mn.sec=as.character(paste(hour(timestamp), minute(timestamp), second(timestamp), sep = ":"))) %>% 
  select(date, ztimestamp = timestamp, lat = location_lat, lon = location_long, bird, mo.da, hr.mn.sec, water.level, inlight)

write.csv(hetp, "C:/Users/scott.jennings/Documents/Projects/hetp/hetp_shiny_webmap/data/hetp_use.csv", row.names = F)

