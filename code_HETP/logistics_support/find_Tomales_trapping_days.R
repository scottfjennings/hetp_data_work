

library(tidyverse)
library(oce)
library(lubridate)
library(purrr)
library(measurements)
library(sp)
library(maptools)
library(chron)
library(scales)

###############################################

month_seq_char <- c("201903", "201904", "201905", "201906", "201907")


sf_hl_files <- paste("SanFran_", month_seq_char, "_HL.txt", sep = "")
sf_1h_files <- paste("SanFran_", month_seq_char, "_1hr.txt", sep = "")
bl_hl_files <- paste("BlakesLanding_", month_seq_char, "_HL.txt", sep = "")

################################################
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
#


sf_hl <- map_df(sf_hl_files, tide_reader_hl_generic)  # this will do just the 4 months before and 1 month after the desired month 

zloc <- "C:/Users/scott.jennings/Documents/Projects/water_levels/tides/BlakesLanding/BlakesLanding_HL/"
zskip = 20
bl_hl <- map_df(bl_hl_files, tide_reader_hl_generic) # this will do just the 4 months before and 1 month after the desired month 


tide_reader_sf_1hr <- function(zfile){
  zfile_path <- paste("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/SanFran/SanFran_1hr/", zfile, sep = "")
  tide_table = read.table(zfile_path, header = F, skip = 14,  col.names = c("date", "day",  "time", "water.level")) %>% 
  mutate(datetime = paste(date, time, sep = " "),
         datetime = as.POSIXct(datetime),
         date = as.POSIXct(date)) 
}
#
sf_1h <- map_df(sf_1h_files, tide_reader_sf_1hr)



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

# this is the Blakes Landing tide curve
bl_genhl_1h <- subord_offsetter(zharmonic.station = sf_1h_offsethl,
                                zsubordinate.name = "BlakesLanding",
                                zsubordinate.offsets = subordinate_offsets)



ts <- seq(as.POSIXct("2019-03-15 00:00:00", tz = "America/Los_Angeles"),
          as.POSIXct("2019-07-31 23:55:00", tz = "America/Los_Angeles"), by = "5 min")

tide_interpolater <- function(zstart.date, zend.date, offset_1hr_df, gps_timeseries) {
  
  #zstart.date = "2019-03-15"
  #zend.date = "2019-07-31"
  #offset_1hr_df = bl_genhl_1h
  #gps_timeseries = ts
# this is adapted from the last example here: https://beckmw.wordpress.com/2017/04/12/predicting-tides-in-r/
# generating offsets in subord_offsetter() above doesn't work on the very first and very last day of the downloaded tide data, so the start adn end date should at least trim off those dates
offset_1hr_df4pred <- filter(offset_1hr_df, date > zstart.date & date < zend.date) 
  
offset_1hr_df_tidem <- oce::as.sealevel(elevation = offset_1hr_df4pred$generated.level, time = offset_1hr_df4pred$generated.time) %>% 
  tidem() # make it into an object that oce can work with

    
  water.level = predict(offset_1hr_df_tidem, newdata = ts) # this is actually running oce::predict_tidem to interpolate the tide level at the subordinate station for each GPS timestamp
  
  tides <- cbind(ts, water.level) %>% 
    data.frame() %>% 
    mutate(ts = as.POSIXct(ts, tz = "America/Los_Angeles", origin = "1970-01-01"))
  
}
blake5min_tides <- tide_interpolater(zstart.date = "2019-03-15", 
                                     zend.date = "2019-07-31", 
                                     offset_1hr_df = bl_genhl_1h, 
                                     gps_timeseries = ts)





##########################################
# blake.dawn.dusk is a standalone dataframe with a record for each date in hetpDF
# it is used below in make blake_eelgrass_available

make.blake.dawn.dusk<-function(){
  study.days <- unique(as.Date(blake5min_tides$ts, tz = "America/Los_Angeles"))
  #length(study.days)
  hels <- matrix(c(-122.9, 38.2), nrow=1)
  Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
  d <- as.POSIXct(as.character(study.days), tzone = "America/Los_Angeles")
  dawn = data.frame(crepuscule(Hels, d, solarDep=6, direction="dawn", POSIXct.out=TRUE))   
  colnames(dawn) <- c("dawn.frac", "dawn.time")
  dusk = data.frame(crepuscule(Hels, d, solarDep=6, direction="dusk", POSIXct.out=TRUE))   
  colnames(dusk) <- c("dusk.frac", "dusk.time") 
  dawn.dusk <- cbind(dawn, dusk) %>% 
    dplyr::select(dawn.time, dusk.time)
  dawn.dusk$date <- as.Date(dawn.dusk$dawn.time)
  

  return(dawn.dusk)
}
blake.dawn.dusk<- make.blake.dawn.dusk()

blake5min_tides.dawn.dusk <- blake5min_tides %>% 
    mutate(date = as.Date(blake5min_tides$ts, tz = "America/Los_Angeles")) %>% 
    full_join(., blake.dawn.dusk)


tide_viewer1day <- function(zyear, zmonth, zdate.range) {
first.date <- as.Date(as.character(paste(zyear, zmonth, zdate.range[1], sep = "-")))
last.date <- as.Date(as.character(paste(zyear, zmonth, zdate.range[2], sep = "-")))
first.day <- weekdays(first.date)
last.day <- weekdays(last.date)
 
  fooz2 <- blake5min_tides.dawn.dusk %>% 
  filter(year(ts) == zyear & month(ts) == zmonth & (day(ts) >= zdate.range[1] & day(ts) <= zdate.range[2]))
  
  ggplot(data = fooz2)+
    theme(panel.background = element_rect(fill="gray50"),
          panel.grid.major = element_blank(), 
          legend.position="none",
          plot.title = element_text(size = 40, face = "bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_rect(aes(xmin = dawn.time, xmax = dusk.time , ymin = -Inf, ymax = Inf), alpha = .1, fill = "gray90")+
    geom_line(aes(x=ts, y=water.level), size = 2)+
    geom_hline(yintercept = 1, size = 2) +
    ylim(c(-1.5, 6))+
    xlab("")+
    ylab("Tide height") +
   scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M", tz = "America/Los_Angeles")) +
    ggtitle(paste(first.day, "to", last.day, month(zmonth, label = T), zdate.range[1], "to", zdate.range[2], sep = " "))

}

Mar26 <- tide_viewer1day(zyear = 2019, 
              zmonth = 3, 
              zdate.range = c(26, 28))
Apr2 <- tide_viewer1day(zyear = 2019, 
              zmonth = 4, 
              zdate.range = c(2, 4))
Apr9 <- tide_viewer1day(zyear = 2019, 
              zmonth = 4, 
              zdate.range = c(9, 11))
Apr16 <- tide_viewer1day(zyear = 2019, 
              zmonth = 4, 
              zdate.range = c(16, 18))
Apr23 <- tide_viewer1day(zyear = 2019, 
              zmonth = 4, 
              zdate.range = c(23, 25))

Apr30 <- tide_viewer1day(zyear = 2019, 
              zmonth = 4, 
              zdate.range = c(30, 30))
May1 <- tide_viewer1day(zyear = 2019, 
              zmonth = 5, 
              zdate.range = c(1, 2))
May7 <- tide_viewer1day(zyear = 2019, 
              zmonth = 5, 
              zdate.range = c(7, 9))
May14 <- tide_viewer1day(zyear = 2019, 
              zmonth = 5, 
              zdate.range = c(14, 16))
May21 <- tide_viewer1day(zyear = 2019, 
              zmonth = 5, 
              zdate.range = c(21, 23))
May28 <- tide_viewer1day(zyear = 2019, 
              zmonth = 5, 
              zdate.range = c(28, 30))
June4 <- tide_viewer1day(zyear = 2019, 
              zmonth = 6, 
              zdate.range = c(4, 6))
June11 <- tide_viewer1day(zyear = 2019, 
              zmonth = 6, 
              zdate.range = c(11, 13))
June18 <- tide_viewer1day(zyear = 2019, 
              zmonth = 6, 
              zdate.range = c(18, 20))
June25 <- tide_viewer1day(zyear = 2019, 
              zmonth = 6, 
              zdate.range = c(25, 27))
July2 <- tide_viewer1day(zyear = 2019, 
              zmonth = 7, 
              zdate.range = c(2, 4))
July9 <- tide_viewer1day(zyear = 2019, 
              zmonth = 7, 
              zdate.range = c(9, 11))
July16 <- tide_viewer1day(zyear = 2019, 
              zmonth = 7, 
              zdate.range = c(16, 18))
July23 <- tide_viewer1day(zyear = 2019, 
              zmonth = 7, 
              zdate.range = c(23, 25))

#########################################################################
blake_hilo_may18 <- read.csv("C:/Users/Scott/Dropbox (Audubon Canyon Ranch)/water_levels/tides/BlakesLanding/hilo/blake_tides_hilo_may18.csv")

blake_hilo_jun18 <- read.csv("C:/Users/Scott/Dropbox (Audubon Canyon Ranch)/water_levels/tides/BlakesLanding/hilo/blake_tides_hilo_jun18.csv")

blake_hilo <- rbind(blake_hilo_may18, blake_hilo_jun18) %>% 
  mutate(datetime = mdy_hm(Date.Time),
         date = date(datetime)) %>% 
  select(datetime, date, water.level = Prediction, hilo = Type, -Date.Time)

blake_hh <- blake_hilo %>% 
  group_by(date) %>% 
  summarize(max.water = max(water.level))

blake_ll <- blake_hilo %>%  
  group_by(date) %>% 
  summarize(min.water = min(water.level))
  
blake_hh_ll <- full_join(blake_hilo, full_join(blake_hh, blake_ll))



blake_hh_ll <- blake_hh_ll %>% 
mutate(which.tide = ifelse(water.level == max.water, "HH",
                             ifelse(water.level == min.water, "LL",
                                    ifelse(water.level < max.water & hilo == "H", "LH", "HL")))) %>% 
  select(-max.water, -min.water) %>% 
  mutate(time = times(paste(hour(datetime), minute(datetime), second(datetime), sep = ":")))


blake_hh_ll_wide1 <- blake_hh_ll %>%
  select(date, which.tide, water.level) %>% 
  mutate(which.tide = paste(which.tide, "level", sep = ".")) %>% 
  spread(which.tide, water.level)

blake_hh_ll_wide2 <- blake_hh_ll %>%
  select(date, which.tide, time) %>% 
  mutate(which.tide = paste(which.tide, "time", sep = ".")) %>% 
  spread(which.tide, time)


blake_hh_ll_wide <- full_join(blake_hh_ll_wide1, blake_hh_ll_wide2)

#-------------------------------------

#blake10min <- read.csv("C:/Users/scott.jennings/Documents/Projects/water_levels/tides/BlakesLanding/10min/blake_tides10min_compiled.csv")

blake10min <- read.csv("C:/Users/Scott/Dropbox (Audubon Canyon Ranch)/water_levels/tides/BlakesLanding/10min/blake_tides10min_compiled.csv") %>%
  mutate(datetime = ymd_hms(datetime),
         date = ymd(date),
         md = paste(month(date), day(date), sep = "-"),
         time = times(paste(hour(datetime), minute(datetime), second(datetime), sep = ":")),
         day_name = wday(datetime, label = T, abbr = T),
         day_num = wday(datetime),
         mon_day = paste(md, day_name)) %>% 
  filter(minute(datetime) == 00 | minute(datetime) == 30)



blake10min$outgoing <- with(blake10min, c(FALSE, water.level[-1L] < water.level[-length(water.level)]) & water.level!='NULL')
blake10min$incoming <- with(blake10min, c(FALSE, water.level[-1L] > water.level[-length(water.level)]) & water.level!='NULL')


blake10min_which_tides <- full_join(blake10min, blake_hh_ll_wide)





blake10min_which_tides$in_out <- ifelse(blake10min_which_tides$incoming == TRUE, "in", 
                            ifelse(blake10min_which_tides$outgoing == TRUE, "out", ""))



#-------------------------------------------

foo <- blake10min_which_tides %>% 
  filter(time >= "07:00:00", time <= "17:00:00" & water.level >= 0, day_num > 1 & day_num < 7, date > "2018-05-09")

#-------------------------------------------
# test plots -- don't need to run
foo$timez <- hms(foo$datetime)

ggplot(data = foo, aes(x = timez, y = water.level)) +
  geom_line() +
  facet_wrap(~mon_day, nrow = 4)+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = 1)


ggplot(data = blake_day, aes(y = reorder(md, date), x = time.ch, fill=factor(gr2))) +
  geom_tile(show.legend = FALSE)+
  scale_fill_manual(values = c("0" = "white", "1" = "blue"))

ggplot(data=blake_day, aes(x=reorder(point, as.numeric(point)), y=reorder(common.name, -species.number), fill=factor(detected)))+
  geom_tile()+
  scale_fill_manual(values=c("FALSE"="red", "TRUE"="blue"))+
  ylab("")+
  xlab("Point")+ 
  theme(legend.position="none")+
  ggtitle(main) +
  theme(plot.title = element_text(hjust = 0.5))


#-------------------------------------------
start.time.gr1 <- foo %>% 
  filter(water.level >= 1) %>% 
  group_by(date) %>% 
  filter(time == min(time)) %>% 
  select(date, start.time.gr1 = time, start.time.gr1.level = water.level, in_out)

end.time.gr1 <- foo %>% 
  filter(water.level >= 1) %>% 
  group_by(date) %>% 
  filter(time == max(time)) %>% 
  select(date, end.time.gr1 = time, end.time.gr1.level = water.level)



start.time.gr2 <- foo %>% 
  filter(water.level >= 2) %>% 
  group_by(date) %>% 
  filter(time == min(time)) %>% 
  select(date, start.time.gr2 = time, start.time.gr2.level = water.level)


end.time.gr2 <- foo %>% 
  filter(water.level >= 2) %>% 
  group_by(date) %>% 
  filter(time == max(time)) %>% 
  select(date, end.time.gr2 = time, end.time.gr2.level = water.level)

start.time.le1 <- foo %>% 
  filter(water.level <= 1) %>% 
  group_by(date) %>% 
  filter(time == min(time)) %>% 
  select(date, start.time.le1 = time, start.time.le1.level = water.level)

start.time.le2 <- foo %>% 
  filter(water.level <= 2) %>% 
  group_by(date) %>% 
  filter(time == min(time)) %>% 
  select(date, start.time.le2 = time, start.time.le2.level = water.level)


blake_day_times <- full_join(start.time.gr1, 
                             full_join(start.time.gr2, 
                                       full_join(end.time.gr2, 
                                                 full_join(end.time.gr1, by = c("date")), 
                                       by = c("date")), 
                             by = c("date")))

blake_day_times <- full_join(start.time.gr1, 
                             full_join(end.time.gr1, 
                                       full_join(start.time.gr2, 
                                                 full_join(end.time.gr2, 
                                                           full_join(start.time.le1, start.time.le2, by = c("date")),
                                                           by = c("date")),
                                                 by = c("date")),
                                       by = c("date")),
                             by = c("date"))

                                                           
                             

final_blake <- full_join(blake_day_times, blake_hh_ll_wide)
final_blake$test = final_blake$LL.time > final_blake$start.time.gr1 & final_blake$LL.time < final_blake$end.time.gr1

finaler_blake <- final_blake %>% 
  mutate(start_orange = as.character(start.time.gr1),
         start_green = as.character(start.time.gr2),
         end_green = ifelse(test == TRUE, as.character(start.time.le2), as.character(end.time.gr2)),
         end_orange = ifelse(test == TRUE, as.character(start.time.le1), as.character(end.time.gr1))) %>% 
  mutate(day_name = wday(date, label = T, abbr = T)) %>% 
  select(day_name, date, start_orange, start_green, end_green, end_orange, HH.time, HH.level, LH.time, LH.level, HL.time, HL.level, LL.time, LL.level)

write.csv(finaler_blake, "tomales_trapping_times.csv", row.names = F)                             
       