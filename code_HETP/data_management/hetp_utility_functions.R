
library(tidyverse)
library(lubridate)
library(raster)
#library(proj4)
library(rgdal)
library(sf)
library(rgeos)
library(rgeos)
library(ggmap)
options(scipen = 999)


# data ----
#hetp <- read.csv("data_files/GPSonly/HETP_GPSonly_201706_201908.csv") %>% 
#  select(event.id, study.local.timestamp, eobs.horizontal.accuracy.estimate, eobs.speed.accuracy.estimate, gps.dop, ground.speed, height.above.ellipsoid, individual.local.identifier)

#hetp_covs <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201912.csv") %>% 
#  filter(!is.na(location_long)) %>% 
#  rename_all(~ gsub("_", ".", .)) %>% 
#  rename(individual.local.identifier = bird) %>% 
#  dplyr::select(-gps.timestamp, -individual.local.identifier)

#hetp_dist <- read.csv("data_files/GPS_with_covariates/distance_eventid.csv")

#hetp_use <- full_join(hetp, hetp_covs, by = c("event.id")) %>% 
#  select(contains("timestamp"), contains("individual"), everything()) %>% 
#  mutate(timestamp = as.POSIXct(timestamp, tz = 'UTC'),
#         date = as.Date(date)) %>% 
#  full_join(., hetp_dist, by = c("event.id"))

#rm(hetp_covs, hetp)

# bird_filter -----
#rm(df, leaf.df, zparms, obj)

# filter hetp_use by specified parms
# filterable variables are:
# bird ID ("zbird")
# day/night ("daytime")
# tidal level ("zwater.level") - this must be a character string of 2 or 3 values. use 2 values with the first being either "G", "L" or "E" to filter for water levels greater, less than or exactly equal to the second value in the string. Use 3 values with "R" as the first value to filter for a range of water levels between the second and third value
# number of daylight hour eelgrass was available ("znum.hours")
# start and end dates ("date.range") - this must be 2 dates entered as a character string
# any of these can be left out
bird_filter <- function(zbird, daytime, zwater.level, znum.hours, date.range) {
  #zbird = "GREG_1"
filt_bird <- hetp_use %>% 
  dplyr::filter(individual.local.identifier == zbird) 
#date.range = c("2017-06-10", "2017-09-01")
if(missing(date.range)) {
filt_bird <- filt_bird 
} else {
filt_bird <- filt_bird %>% 
  dplyr::filter(date >= as.Date(date.range[1]) & date <=as.Date(date.range[2])) 
}
#daytime = TRUE
if(missing(daytime)) {
filt_bird <- filt_bird 
} else {
filt_bird <- filt_bird %>% 
  dplyr::filter(inlight == daytime) 
}
#znum.hours = c("G", 3)
if(missing(znum.hours)) {
filt_bird <- filt_bird 
} else {
    if(znum.hours[1] == "L") {
    filt_bird <- filt_bird %>% 
      dplyr::filter(num.hours < as.numeric(znum.hours[2]))
    } else {
    filt_bird <- filt_bird %>% 
      dplyr::filter(num.hours >= as.numeric(znum.hours[2])) 
    }
}
#zwater.level = c("L", 1)
if(missing(zwater.level)) {
  filt_bird <- filt_bird
} else {
    if(zwater.level[1] == "L") {
    filt_bird <- filt_bird %>% 
      dplyr::filter(water.level <= as.numeric(zwater.level[2]))
    } 
  if(zwater.level[1] == "G"){
    filt_bird <- filt_bird %>% 
      dplyr::filter(water.level >= as.numeric(zwater.level[2]))
  }
    if(zwater.level[1] == "E"){
    filt_bird <- filt_bird %>% 
      dplyr::filter(floor(water.level) == as.numeric(zwater.level[2]))
    }
      if(zwater.level[1] == "R"){
    filt_bird <- filt_bird %>% 
      dplyr::filter(water.level >= zwater.level[2], water.level <= zwater.level[3])
}
}

return(filt_bird)
}


# obj_namer ----
# function to create an object name as concatenation of the specified parms
obj_namer <- function(zbird, daytime, zwater.level, znum.hours, date.range) {
  #zbird = "GREG_1"
zbird = gsub("_", "", zbird)
#date.range = c("2017-06-10", "2017-09-01")
if(missing(date.range)) {
zdates = "allDates" 
} else {
zdates = paste("_", (paste(gsub("-", "", date.range[1]), gsub("-", "", date.range[2]), sep = "_")), sep = "")
}
#daytime = TRUE
if(missing(daytime)) {
zday = "" 
} else {
zday = ifelse(daytime == TRUE, "_day", "_night")
}
#zwater.level = c("G", 1)
if(missing(zwater.level)) {
  zwater = ""
} else {
  if(zwater.level == "R") {
    zwater = paste("_", zwater.level[1], zwater.level[2], "_", zwater.level[3], "ft", sep = "")
  } else {
    zwater = paste("_", zwater.level[1], zwater.level[2], "ft", sep = "")
}}
#znum.hours = c("G", 1)
if(missing(znum.hours)) {
  zhours = ""
} else {
    zhours = paste("_", znum.hours[1], znum.hours[2], "hrs", sep = "")
}
obj.name <- paste(zbird, zdates, zday, zwater, zhours, sep = "")
return(obj.name)
}

# can name a list of parms to feed to both bird_filter and obj_namer
#zparms = list(
#zbird = "GREG_1",
#daytime = TRUE,
#zwater.level = c("G", 1)
#znum.hours = c("L", 3), # "L" is <, "GE" is >=
#date.range = c("2017-06-24", "2017-06-24")
#)
#df <- do.call(bird_filter, zparms)
#summary(df$water.level)

#obj <- do.call(obj_namer, zparms)

#df <- df %>% 
#  filter(minute(as.POSIXct(study.local.timestamp)) < 5) %>% 
#  mutate(month = month(study.local.timestamp))
  

#write.csv(df, paste("data_files/GPS_with_covariates/", zparms$zbird, "_", zparms$date.range[1], "_", zparms$date.range[2], "1hr.csv", sep = ""), row.names = F)


###
# basic map ----
# function to get a background map for the area covered in a df of location data
get_mapper <- function(zdf) {
  
bbox.df <- c(left = min(zdf$location.long) - abs((max(zdf$location.long) - min(zdf$location.long))) * .25, 
                   bottom = min(zdf$location.lat) - abs((max(zdf$location.lat) - min(zdf$location.lat))) * .25, 
                   right = max(zdf$location.long) + abs((max(zdf$location.long) - min(zdf$location.long))) * .25, 
                   top = max(zdf$location.lat) + abs((max(zdf$location.lat) - min(zdf$location.lat))) * .25)
df.terrain.map <- get_stamenmap(bbox.df, maptype = "terrain-background", color="bw")
}
# call to the function (actuall get the background map)
#df_map <- get_mapper(df)
# plot locations on map
#ggmap(df_map)+
#    geom_point(aes(x=location.long, y= location.lat, color = water.level), data = df) +
#  ggtitle(obj)

#df2 <- df %>% 
#  filter(location.lat > 38.198, location.long < -122.919, location.lat < 38.235)


####
# leaflet map with clickable points ----
#library(leaflet)
#leaf.df <- df %>% 
  #dplyr::filter(minute(timestamp) < 10) %>% 
  #dplyr::distinct(date, .keep_all = TRUE) %>% 
#  dplyr::select(study.local.timestamp, event.id, lat = location.lat, lon = location.long, water.level, inlight)

#leaflet(height=500) %>% setView(lng=-122.9, lat=38.22, zoom=12) %>%
#  addProviderTiles("Stamen.Terrain") %>%
#  addCircleMarkers(data = leaf.df, radius= 5, popup = ~paste(study.local.timestamp, water.level, inlight, sep = " "), options = popupOptions(maxWidth=800)) %>%
#  addMeasure(    primaryLengthUnit = "kilometers",
#                 primaryAreaUnit = "sqkilometers")







# start_end_dates ----
# makes a 2 x n matrix with start_date and end_date
# INPUT: start_date and end_date as character strings like this: "20170101"; end_date defaults to Sys.Date(). use by_time to specify time interval to be spanned on each n row. 
hetp_start_end_dates <- function(start_date = "20170101", end_date = Sys.Date(), by_time = "month") {
first_date <- as.Date(start_date, format='%Y%m%d')
if(as.Date(end_date, format='%Y%m%d') == Sys.Date()) {
end_date <- Sys.Date() %m-% months(1) # round down to the last whole month
} else {
  end_date <- as.Date(end_date, format='%Y%m%d')
}

start_dates <- as.Date(seq(first_date, end_date, by = by_time))
end_dates <- (ceiling_date(start_dates, by_time)) - days(1)

start_end_dates <- data.frame(start_date = start_dates, end_date = end_dates) %>% 
  mutate(end_date = ifelse(end_date < Sys.Date(), end_date, Sys.Date()),
         end_date = as.Date(end_date, "1970-01-01"),
         start_date = gsub("-", "", start_date),
         end_date = gsub("-", "", end_date))
return(start_end_dates)
}


#
# filter_hetp_xkm_tomales ----

filter_hetp_xm_tomales <- function(hetp_df, xm) {

  tomales_spine = readOGR("C:/Users/scott.jennings/Documents/Projects/TomalesBay/Tomales_spine.kml")
  tomales_spine_utm <- spTransform(tomales_spine, CRS("+init=epsg:32710"))
 tom_xm_buff <- buffer(tomales_spine_utm, xm)

 pts <- cbind(hetp_df$location.long, hetp_df$location.lat)
 
 hetp_sp <- SpatialPointsDataFrame(pts, hetp_df, proj4string = CRS("+init=epsg:4326"))
 hetp_sp_utm <- spTransform(hetp_sp, CRS("+init=epsg:32710"))
 
 hetp_xm_tom <- over(hetp_sp_utm, tom_xm_buff) 
 
hetp_xm_tom = hetp_sp_utm[!is.na(over(hetp_sp_utm, tom_xm_buff)),]
hetp_xm_tom_ll <- spTransform(hetp_xm_tom, CRS("+init=epsg:4326"))
return(hetp_xm_tom_ll)
}

#hetp_2km_tom <- filter_hetp_xm_tomales(hetp_covs, 2000)



read_bird_month_gps_rds <- function(zbird, zmonth, zyear) {
  zbird_month<- readRDS(paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_gps/", zbird, "_GPS_", zmonth, "_", zyear, sep = ""))
}

read_bird_month_acc_rds <- function(zbird, zmonth, zyear) {
  zbird_month<- readRDS(paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_acc/", zbird, "_ACC_", zmonth, "_", zyear, sep = ""))
}



# convert raw acc values (in mV) to units of gravity and m/s/s  
accel_raw2mss <- function(acc_df) {
  # this is the formula for converting raw ACC values to acceleration in m/s/s
  # from 2017-03-17_e-obs system manual_V1.2.pdf page 63
  # n_axis is any given raw acc value to be converted
  # n_axis_0g is the tag-specific value for this axis when this axis is perpendicular to gravity
  # c_axis is the average difference between this tag's n_axis_0g and the value when the axis is parallel to gravity (averaged across right-side-up and up-side-down); this calculated with function accel_raw2c()
  # 9.81 is gravity constant
  
  acc_eqtn_vals <- readRDS("data_files/R_objects/tag_axis_eqtn_values") %>%
    dplyr::select(tag, axis, zerog, c_axis) 
  
  acc_w_eqtn_vals <- left_join(acc_df, acc_eqtn_vals) %>% 
    mutate(acc_in_g = (acc.val - zerog) * c_axis,
           acc_in_mss = (acc.val - zerog) * c_axis * 9.81)
  
  }
  


