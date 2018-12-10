
# NOAA provided up to 1-minute interval tidal water level data for about 3000 "harmonic" tidal stations around the US. They also provide estimated time and water level offsets for additional "subordinate" stations

# this code allows interpolation of tidal water level values at specific times for subordinate stations, using the offsets from NOAA harmonic tidal stations (https://tidesandcurrents.noaa.gov/tide_predictions.html). This allows/accounts for different offsets for high and low tides.

# This is currently intended for predicting water levels at the exact time that at GPS tag location was recorded, for the purpose of annotating egret location data with tidal information. The code is general enough that it can be adapted to other purposes.


# before using this code, you'll need to download the high-low and 1-hour interval tide data for the harmonic station for the date range of interest, plus at least a 1-day buffer around the actual start and end dates that you really want. Also download the high-low data for the subordinate station for verification purposes. You can only download 1-month chunks of data

# the general process/logic flow is as follows:
# 1. combine the harmonic station high-low and 1-hour interval data
# 2. find the time half-way between each high - low tide pair, then classify points on either side of that mid-time to be offset with the high or low tide offset, as appropriate. For example, if there was a low tide at 0600 am and a high tide at 1200, then another low tide at 1800, then points between 0600 - 0900 would be assigned to be adjusted with the low tide offset, points between 0900-1200 and 1200-1500 would be adjusted with the high tide offset, and points between 1600-1800 would get the low tide offset again.
#   -> the function test_plotter1() below allows visualization of this step and confirmation that it worked as expected
# 3. use the high and low tide offsets for the desired subordinate station to adjust the values from the harmonic station, based on the high-low classifications generated in step 2
# 4. use the function predict_tidem from package oce to interpolate tide levels at specific times, based on the offset values from step 3. 


library(tidyverse)
library(oce)
library(lubridate)
library(purrr)


#--- oce::predict.tidem() doesn't make a very good curve with just H/L values, but does pretty good with 1-hour interval values 
#--- adding H/L values to 1-hour values helps ensure the H and L levels are represented
# data can be downloaded from these sites for:
# Blakes Landing https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9415396&legacy=1
# and 
# San Francisco https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9414290

# all files should be saved in a separate folder for each station and interval, then these functions will read them all in and combine them

# functions to read in tide data for High-Low only
tide_reader_hl_generic <- function(zfile){
zfile_path <- paste(zloc, zfile, sep = "")
  tide_table = read.table(zfile_path, header = F, skip = zskip,  col.names = c("date", "day",  "time", "water.level", "HiLo")) %>% 
  mutate(datetime = paste(date, time, sep = " "),
         datetime = as.POSIXct(datetime),
         date = as.POSIXct(date)) 
}
zloc <- "C:/Users/scott.jennings/Documents/Projects/water_levels/tides/SanFran/SanFran_HL/" # specify the folder location here
zskip = 14
sf_hl <- map_df(list.files(zloc), tide_reader_hl_generic) # map_df, from package purrr, is the function that actually repeats the reading function for each file in the specified folder and combines them into a single data frame

zloc <- "C:/Users/scott.jennings/Documents/Projects/water_levels/tides/BlakesLanding/BlakesLanding_HL/"
zskip = 20
bl_hl <- map_df(list.files(zloc), tide_reader_hl_generic)


# or you can read a single file
#tide_reader_hl <- function(zlocation, zmonthyr, zheader.lines){
#  zfile_path <- paste(zlocation, "/", zlocation, "_", zmonthyr, "_HL.txt", sep = "")
#  tide_table = read.table(zfile_path, header = F, skip = zheader.lines,  col.names = c("date", "day",  "time", "water.level", "HiLo")) #%>% 
#  mutate(datetime = paste(date, time, sep = " "),
#         datetime = as.POSIXct(datetime),
#         date = as.POSIXct(date)) 
#}

#sf_hl_oct18 <- tide_reader_hl(zlocation = "SanFran", zmonthyr = "Oct18", zheader.lines = 14)
#bl_hl <- tide_reader_hl(zlocation = "BlakesLanding", zmonthyr = "Nov18", zheader.lines = 20)

##----- or other interval data. 
# read and combine all files in a location
tide_reader_sf_1hr <- function(zfile){
  zfile_path <- paste("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/SanFran/SanFran_1hr/", zfile, sep = "")
  tide_table = read.table(zfile_path, header = F, skip = 14,  col.names = c("date", "day",  "time", "water.level")) %>% 
  mutate(datetime = paste(date, time, sep = " "),
         datetime = as.POSIXct(datetime),
         date = as.POSIXct(date)) 
}
sf_1h <- map_df(list.files("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/SanFran/SanFran_1hr"), tide_reader_sf_1hr)


# or just read one file at a time
#tide_reader_interval <- function(zlocation, zmonthyr, zinterval, zheader.lines){
#  zfile_path <- paste(zlocation, "/", zlocation, "_", zmonthyr, "_", zinterval, ".txt", sep = "")
#  tide_table = read.table(zfile_path, header = F, skip = zheader.lines,  col.names = c("date", "day",  "time", "water.level")) %>% 
#  mutate(datetime = paste(date, time, sep = " "),
#         datetime = as.POSIXct(datetime),
#         date = as.POSIXct(date)) 
#}
#sf_1hr_oct18 <- tide_reader_interval(zlocation = "SanFran", zmonthyr = "Oct18", zinterval = "1hr", zheader.lines = 14)


# combine 1 hour and H/L water levels for SF
sf_hl_1h <- sf_hl %>% 
  select(datetime, date, water.level) %>% 
  rbind(., select(sf_1h, datetime, date, water.level)) %>% 
  arrange(datetime)


#----
# assign sequential numbers to the 4 (or 3) tides each day
sf_hltidenums <- sf_hl %>% 
  group_by(date) %>% 
  mutate(tide.num = row_number(),
         tide.num = paste("tide", tide.num, sep = ".")) %>% 
  group_by(date, HiLo) %>% 
  ungroup()
# convert to 'wide' data with a row for each day and sets of columns the times, levels, and H/L status of each tide that day
sf_hltidenums_wide <- sf_hltidenums %>% 
  select(date, datetime, water.level, HiLo, tide.num) %>% 
  mutate(zval = paste(datetime, water.level, HiLo, sep = "_")) %>% 
  select(-datetime, -water.level, -HiLo) %>% 
  spread(tide.num, zval) %>% 
  separate(tide.1, c("tide1.time", "tide1.level", "tide1.HiLo"), sep = "_") %>% 
  separate(tide.2, c("tide2.time", "tide2.level", "tide2.HiLo"), sep = "_") %>% 
  separate(tide.3, c("tide3.time", "tide3.level", "tide3.HiLo"), sep = "_") %>% 
  separate(tide.4, c("tide4.time", "tide4.level", "tide4.HiLo"), sep = "_") %>% 
  mutate_at(vars(ends_with("time")), funs(as.POSIXct)) %>% 
  mutate_at(vars(ends_with("level")), funs(as.numeric)) %>% 
  select(-ends_with("level")) %>% 
  mutate(tide4.time = ifelse(is.na(tide4.time), lead(tide1.time), tide4.time),
         tide4.time = as.POSIXct(tide4.time, origin = "1970-01-01 00:00:00"),
         tide4.HiLo = ifelse(is.na(tide4.HiLo), lead(tide1.HiLo), tide4.HiLo)) %>% 
  mutate(prev.day.tide4.time = lag(tide4.time),
         prev.day.tide4.HiLo = lag(tide4.HiLo),
         next.day.tide1.time = lead(tide1.time),
         next.day.tide1.HiLo = lead(tide1.HiLo))

# find the mid times between each tidal extreme pair
sf_hltidenums_wide_mids <- sf_hltidenums_wide %>% 
  mutate(tide1.tide2.mid = tide1.time + ((tide2.time - tide1.time)/2),
         tide2.tide3.mid = tide2.time + ((tide3.time - tide2.time)/2),
         tide3.tide4.mid = tide3.time + ((tide4.time - tide3.time)/2),
         prev.tide4.tide1.mid = tide1.time - ((tide1.time - prev.day.tide4.time)/2),
         tide4.nexttide1.mid = tide4.time + ((next.day.tide1.time - tide4.time)/2),
         prev.tide4.tide1.mid = ifelse(prev.tide4.tide1.mid == tide1.time, lag(tide3.tide4.mid), prev.tide4.tide1.mid),
         prev.tide4.tide1.mid = as.POSIXct(prev.tide4.tide1.mid, origin = "1970-01-01 00:00:00"))

# combine the mid-times to the 1 - hr interval water level data for the harmonic station
sf_1h_tidenums <- sf_1h %>% 
  full_join(., sf_hltidenums_wide_mids, by = c("date"))

# assign each point in the 1-hr interval harmonic data to be offset using either the high or low tide subordinate offset values
sf_1h_offsethl <- sf_1h_tidenums %>% 
   mutate(offsetHL = "L",
          offsetHL = ifelse(datetime < prev.tide4.tide1.mid & tide1.HiLo == "L", "H", offsetHL),
          offsetHL = ifelse(datetime > prev.tide4.tide1.mid & datetime < tide1.tide2.mid & tide1.HiLo == "H", "H", offsetHL),
          offsetHL = ifelse(datetime > tide1.tide2.mid & datetime < tide2.tide3.mid & tide2.HiLo == "H", "H", offsetHL),
          offsetHL = ifelse(datetime > tide2.tide3.mid & datetime < tide3.tide4.mid & tide3.HiLo == "H", "H", offsetHL),
          offsetHL = ifelse(datetime > tide3.tide4.mid & datetime < tide4.nexttide1.mid & tide4.HiLo == "H", "H", offsetHL),
          offsetHL = ifelse(datetime > tide4.nexttide1.mid & tide4.HiLo == "L", "H", offsetHL)) %>% 
   select(date, datetime, water.level, offsetHL, everything())



# test plot to see if the Hi - Low classification worked
test_plotter1 <- function(zyear, zmonth, zdate.range){
ggplot(data = filter(sf_1h_offsethl,  year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2])) +
  geom_point(aes(x = datetime, y = water.level, color = offsetHL, size = 3))
}
test_plotter2(zyear = 2018, 
              zmonth = 11, 
              zdate.range = c(5, 10)) # specify the year, month, and day range you want to look at. spot check a few times throughout the entire range of your dates


##---------------------------
# generate a dataframe with the desired subordinate station offsets by inputting the offset values in the last line here:
subordinate_offsets <- data.frame(subordinate.name = rep("BlakesLanding", 4),
                            which.off = c(rep("time", 2), rep("water.level", 2)),
                            tide = rep(c("high", "low"), 2),
                            value = c(32, 75, 0.86, 0.79))
#write.csv(subordinate_offsets, "subordinate_station_offsets.csv", row.names = F)
#subordinate_offsets <- read.csv("subordinate_station_offsets.csv")

# NOTE - so far I'm just making adjustments based on 1 subordinate station. this can be adapted later to include multiple stations, but this will require adding a column to the bird location data frame specifying which subordinate station to offset each point by, and re-writing the subord_offsetter() function to select offsets based on that column (rather than by a call to the function)

#----------------------------
# now a function to actually offset each point in the 1-hr interval harmonic station data

subord_offsetter <- function(zharmonic.station, zsubordinate.name, zsubordinate.offsets){

offset.getter <- function(zsubordinate.name, zwhich.off, ztide) {
    
  offset <- zsubordinate.offsets %>% 
    filter(subordinate.name == zsubordinate.name,
           which.off == zwhich.off,
           tide == ztide) %>% 
    select(value)
  return(offset[1,1])
  }
  
generated_offsets <- zharmonic.station %>%
    group_by(date) %>% 
    mutate(by.day.mean.level = mean(water.level)) %>% 
    ungroup() %>% 
    mutate(generated.level = ifelse(offsetHL == "H", 
                                  water.level * offset.getter(zsubordinate.name, "water.level", "high"),
                                  water.level * offset.getter(zsubordinate.name, "water.level", "low")),
         generated.time = ifelse(offsetHL == "H", 
                                 datetime + (offset.getter(zsubordinate.name, "time", "high")*60),
                                 datetime + (offset.getter(zsubordinate.name, "time", "low")*60)),
         generated.time = as.POSIXct(generated.time, origin = "1970-01-01 00:00:00"))   
  return(generated_offsets)
}
# call the function, specifying the offset dataframe created above
bl_genhl_1h <- subord_offsetter(zharmonic.station = sf_1h_offsethl,
                                zsubordinate.name = "BlakesLanding",
                                zsubordinate.offsets = subordinate_offsets)


# plot the harmonic station and newly generated, offset subordinate station data together to spot check how it worked
test_plotter2 <- function(zyear, zmonth, zdate.range) {
ggplot() +
  geom_line(data = filter(sf_hl_1h, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level)) +
  geom_point(data = filter(bl_hl, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), color = "blue") +
  geom_line(data = filter(bl_genhl_1h, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = generated.time, y = generated.level), color = "red") 
}
test_plotter2(zyear = 2018, 
              zmonth = 11, 
              zdate.range = c(5, 10))


#---


# if you have several files (e.g. monthly) for the bird GPS data downloaded as .csvs from Movebank, this function will read them in, reduce the fields, and combine them into a single dataframe
hetp_gps_reader <- function(zfile){
zfile_path <- paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/", zfile, sep = "")
  tide_table = read.csv(zfile_path) %>% 
  select(gps.timestamp = timestamp, location_lat = location.lat, location_long = location.long, tag = tag.local.identifier, bird = individual.local.identifier, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp = study.local.timestamp) %>% 
    mutate(timestamp = as.POSIXct(timestamp))
}

hetp_gps <- map_df(list.files("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/"), hetp_gps_reader)

#---





bird_tide_interpolater <- function(zstart.date, zend.date, offset_1hr_df, gps_timeseries) {
# this is adapted from the last example here: https://beckmw.wordpress.com/2017/04/12/predicting-tides-in-r/
# generating offsets in subord_offsetter() above doesn't work on the very first and very last day of the downloaded tide data, so the start adn end date should at least trim off those dates
offset_1hr_df4pred <- filter(offset_1hr_df, date > zstart.date & date < zend.date) 
  
offset_1hr_df_tidem <- as.sealevel(elevation = offset_1hr_df4pred$generated.level, time = offset_1hr_df4pred$generated.time) %>% 
  tidem() # make it into an object that oce can work with

bird_tides <- gps_timeseries %>% 
  mutate(water.level = predict(offset_1hr_df_tidem, newdata = timestamp)) # this is actually running oce::predict_tidem to interpolate the tide level at the subordinate station for each GPS timestamp
}
bird_tides <- bird_tide_interpolater(zstart.date = "2017-06-02", 
                                     zend.date = "2018-12-30", 
                                     offset_1hr_df = bl_genhl_1h, 
                                     gps_timeseries = hetp_gps)



# can plot the interpolated bird_tides along with the original harmonic station and offset subordinate station data to spot check how good teh interpolation did
test_plotter3 <- function(zyear, zmonth, zdate.range) {
ggplot() +
  #geom_line(data = filter(bl_genhl_1h_2_6m, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = Estimated), color = "red", size = 2) +
  geom_line(data = filter(bird_tides, year(timestamp) == zyear, month(timestamp) == zmonth & day(timestamp) >= zdate.range[1] & day(timestamp) <= zdate.range[2]), aes(x = timestamp, y = Estimated), color = "red", size = 2) +
  geom_line(data = filter(sf_1h, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), size = 1) +
  geom_point(data = filter(sf_hl, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), size = 1)  +
  geom_line(data = filter(bl_genhl_1h, year(generated.time) == zyear, month(generated.time) == zmonth & day(generated.time) >= zdate.range[1] & day(generated.time) <= zdate.range[2]), aes(x = generated.time, y = generated.level), color = "blue", size = 1) +
  geom_point(data = filter(bl_hl, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), color = "blue", size = 1)
}
test_plotter3(zyear = 2018, 
              zmonth = 11, 
              zdate.range = c(5, 10))


