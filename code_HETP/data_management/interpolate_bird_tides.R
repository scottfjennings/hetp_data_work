



# 4. use the function predict_tidem from package oce to interpolate tide levels at specific times, based on the offset values from step 3. 

#--- oce::predict.tidem() doesn't make a very good curve with just H/L values, but does pretty good with 1-hour interval values 
#--- adding H/L values to 1-hour values helps ensure the H and L levels are represented
# data can be downloaded from these sites for:
# Blakes Landing https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9415396&legacy=1
# and 
# San Francisco https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9414290



# packages, source ----
library(tidyverse)
library(oce)
library(lubridate)
library(purrr)
library(measurements)
library(sp)
library(maptools)
library(hms)

source("code_HETP/data_management/hetp_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/code/tide_utility_functions.r")
# get tide data ----
# uses blakes_offset_1hr from Projects/water_levels/tides/code/make_subord_station_predictions.r. 

blakes_offset_1hr <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/generated/blakes_offset_1hr")
  



                        
bl_hl <- map2_df("BlakesLanding/BlakesLanding_HL/",
                 list.files("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/downloaded/BlakesLanding/BlakesLanding_HL"),
                 read_tide_month_subord) %>% 
  rename(water.level = pred)



# plot the harmonic station and newly generated, offset subordinate station data together to spot check how it worked ----
test_plotter2 <- function(zyear, zmonth, zdate.range) {
ggplot() +
  geom_line(data = filter(blakes_offset_1hr, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level)) +
  geom_point(data = filter(bl_hl, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), color = "blue") +
  geom_line(data = filter(blakes_offset_1hr, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = offset.datetime, y = offset.water.level), color = "red") 
}
test_plotter2(zyear = 2020, 
              zmonth = 2, 
              zdate.range = c(5, 10))



# read GPS data !! need to change file name for most recent data ----

# if you have several files (e.g. monthly) for the bird GPS data downloaded as .csvs from Movebank, this function will read them in, reduce the fields, and combine them into a single dataframe
hetp_gps_reader <- function(zfile){
zfile_path <- paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/", zfile, sep = "")
  hetp_gps = read.csv(zfile_path) 
}

#hetp_gps <- map_df(list.files("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/GPSonly/"), hetp_gps_reader)

hetp_gps <- hetp_gps_reader("HETP_GPSonly_201706_202007.csv")

hetp_gps2 <- hetp_gps %>% 
  dplyr::select(gps.timestamp = timestamp, location_lat = location.lat, location_long = location.long, tag = tag.local.identifier, bird = individual.local.identifier, event.id, utm.easting, utm.northing, utm.zone, study.timezone, timestamp = study.local.timestamp) %>% 
    mutate(timestamp = as.POSIXct(timestamp))


# assign tide level to each GPS point  ----

bird_tide_interpolater <- function(offset_1hr_df, gps_timeseries) {
# this is adapted from the last example here: https://beckmw.wordpress.com/2017/04/12/predicting-tides-in-r/
# generating offsets in subord_offsetter() above doesn't work on the very first and very last day of the downloaded tide data, so the start adn end date should at least trim off those dates
offset_1hr_df4pred <- filter(offset_1hr_df, date > min(date) & date < max(date)) 
  
offset_1hr_df_sl <- oce::as.sealevel(elevation = offset_1hr_df4pred$offset.water.level, time = offset_1hr_df4pred$offset.datetime) # make it into an object that oce can work with


offset_1hr_df_tidem <- tidem(offset_1hr_df_sl) 

bird_tides <- gps_timeseries %>% 
  mutate(water.level = predict(offset_1hr_df_tidem, newdata = timestamp)) # this is actually running oce::predict.tidem to interpolate the tide level at the subordinate station for each GPS timestamp
}
bird_tides <- bird_tide_interpolater(offset_1hr_df = blakes_offset_1hr, 
                                     gps_timeseries = hetp_gps2)


# can plot the interpolated bird_tides along with the original harmonic station and offset subordinate station data to spot check how good the interpolation did
test_plotter3 <- function(zyear, zmonth, zdate.range) {
ggplot() +
  #geom_line(data = filter(bl_genhl_1h_2_6m, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = Estimated), color = "red", size = 2) +
  geom_line(data = filter(bird_tides, year(timestamp) == zyear, month(timestamp) == zmonth & day(timestamp) >= zdate.range[1] & day(timestamp) <= zdate.range[2]), aes(x = timestamp, y = water.level), color = "red", size = 2) +
  geom_line(data = filter(blakes_offset_1hr, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), size = 1) +
  geom_point(data = filter(blakes_offset_1hr, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), size = 1)  +
  geom_line(data = filter(blakes_offset_1hr, year(offset.datetime) == zyear, month(offset.datetime) == zmonth & day(offset.datetime) >= zdate.range[1] & day(offset.datetime) <= zdate.range[2]), aes(x = offset.datetime, y = offset.water.level), color = "blue", size = 1) +
    ggtitle("red = bird tide; blue = blakes tide; black = SF tide") +
  geom_point(data = filter(bl_hl, year(datetime) == zyear, month(datetime) == zmonth & day(datetime) >= zdate.range[1] & day(datetime) <= zdate.range[2]), aes(x = datetime, y = water.level), color = "blue", size = 2)
}
test_plotter3(zyear = 2020, 
              zmonth = 2, 
              zdate.range = c(15, 20))

# looks good

# write to disk
saveRDS(bird_tides, "data_files/rds/bird_tides/bird_tides")


## handy cleanup if going on to hetp_covariates.R or elsewhere ----
rm("bird_tide_interpolater", "bl_genhl_1h", "bl_hl", "hetp_gps", "hetp_gps_reader", "sf_1h", "sf_1h_offsethl", "sf_1h_tidenums", "sf_hl", "sf_hl_1h", "sf_hltidenums", "sf_hltidenums_wide", "sf_hltidenums_wide2", "sf_hltidenums_wide3", "sf_hltidenums_wide4", "sf_hltidenums_wide5", "sf_hltidenums_wide_mids", "subord_offsetter", "subordinate_offsets", "tide_reader_hl_generic", "tide_reader_sf_1hr", "zloc", "zskip", "StartEndDates")



