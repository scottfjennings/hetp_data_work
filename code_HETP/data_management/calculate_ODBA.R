


library(tidyverse)
library(lubridate)
source("code_HETP/data_management/hetp_utility_functions.r")



bird_month <- readRDS("data_files/rds/bird_month")

acc_eqtn_vals <- readRDS("data_files/rds/odba/tag_axis_eqtn_values") %>%
    dplyr::select(tag, axis, zerog, c_axis) %>% 
    full_join(., readRDS("data_files/rds/birdID_tagnum") %>% mutate(tag = as.character(tag))) %>% 
    mutate(axis = gsub("acc.", "", axis))

acc_axes = c("x", "y", "z")
 
acc_files <- list.files("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_acc/") %>% 
  data.frame() %>% 
  rename(zfile = 1) %>% 
  filter(!grepl('GREG_6', zfile)) %>% # accidentally only activated 2 axes for greg 6, so no odba
  mutate(acc = "acc")

odba_files <- list.files("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_odba/") %>% 
  data.frame() %>% 
  rename(zfile = 1) %>% 
  mutate(odba = "odba")

zzz <- anti_join(acc_files %>% mutate(zfile = gsub("ACC_", "", zfile)),
                 odba_files %>% mutate(zfile = gsub("ODBA_", "", zfile)))


# read_bird_month_acc_rds() is in hetp_utility_functions.R
# monthly rds files created by clip_write_data_files.R
zbird = "GREG_2"
zmonth = 8
zyear = 2017


# define functions ----
raw_acc_to_long_df <- function(zacc) {


samples_per_axis <- ((str_count(as.character(zacc$eobs.accelerations.raw), " ") %>% data.frame() %>% max() + 1)/length(acc_axes))[[1]]
 
bird_acc_sep <- zacc %>% 
  dplyr::select(event.id, bird = individual.local.identifier, study.local.timestamp, eobs.accelerations.raw, -eobs.acceleration.axes, -eobs.acceleration.sampling.frequency.per.axis) %>% 
  separate(eobs.accelerations.raw, into = paste(rep(acc_axes, samples_per_axis), rep(1:samples_per_axis, each = length(acc_axes)), sep = "_"))

bird_acc_sep_long <- bird_acc_sep %>% 
  pivot_longer(contains(c("x_", "y_", "z_")), names_to = "axis.num", values_to = "acc.raw") %>% 
  mutate(acc.raw = as.numeric(acc.raw))


bird_acc_sep_long_sep <- bird_acc_sep_long %>% 
  separate(axis.num, into = c("axis", "sample.num")) %>% 
  mutate(sample.num = as.numeric(sample.num),
         acc.raw = as.numeric(acc.raw)) %>% 
  filter(!is.na(acc.raw))
}

# acc_long <- raw_acc_to_long_df(bird_acc)



accel_raw2mss <- function(acc_df) {
  # this is the formula for converting raw ACC values to acceleration in m/s/s
  # from 2017-03-17_e-obs system manual_V1.2.pdf page 63
  # n_axis is any given raw acc value to be converted
  # n_axis_0g is the tag-specific value for this axis when this axis is perpendicular to gravity
  # c_axis is the average difference between this tag's n_axis_0g and the value when the axis is parallel to gravity (averaged across right-side-up and up-side-down); this calculated with function accel_raw2c()
  # 9.81 is gravity constant

  acc_w_eqtn_vals <- left_join(acc_df, acc_eqtn_vals, by = c("bird", "axis")) %>% 
    mutate(acc_in_g = (acc.raw - zerog) * c_axis,
           acc_in_mss = (acc.raw - zerog) * c_axis * 9.81)
  
  }
  


# calculate ODBA using difference between each value and a moving average
# calc_odba <- function(acc_long) {
ma.window = 30
      ma <- function(x, n = ma.window){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average function, from: https://stackoverflow.com/questions/743812/calculating-moving-average


acc_ma <- acc_long %>% 
  arrange(study.local.timestamp, axis, sample.num) %>% 
  group_by(study.local.timestamp, axis) %>% 
  mutate(axis.ma = ma(acc.raw, ma.window),
         moving.av.window = ma.window) %>% 
  filter(!is.na(axis.ma)) %>% 
  mutate(accel = acc.raw - axis.ma) %>% 
  ungroup()

odba_timestamp <- acc_ma %>% 
  group_by(study.local.timestamp) %>% 
  summarise(odba.timestamp = sum(abs(accel)),
            num.samples = n()) %>% 
  ungroup()

return(odba_timestamp)
}


#

# calculate ODBA using difference between each values and the average across the entire ACC pulse (no moving average)
calc_odba_whole_pulse <- function(acc_long) {
 

acc_ma <- acc_long %>% 
  arrange(bird, study.local.timestamp, axis, sample.num) %>% 
  group_by(bird, study.local.timestamp, axis) %>% 
  mutate(axis.ma = mean(acc_in_mss),
         moving.av.window = "whole.pulse") %>% 
  filter(!is.na(axis.ma)) %>% 
  mutate(accel = acc_in_mss - axis.ma) %>% 
  ungroup()

odba_timestamp <- acc_ma %>% 
  group_by(bird, study.local.timestamp) %>% 
  summarise(odba.timestamp = sum(abs(accel))) %>% 
  ungroup()

return(odba_timestamp)
}

# function of piped functions to loop through multiple files ----


full_odba_maker <- function(zfile) {
  
  outfile <- gsub("ACC", "ODBA", zfile)
  
readRDS(paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_acc/", zfile, sep = "")) %>% # 8.75 sec
  filter(eobs.acceleration.axes == "XYZ", eobs.acceleration.sampling.frequency.per.axis == 10) %>% # 8.75 sec
  #dplyr::select(-timestamp, -study.timezone) %>% # 12.03 sec, 11.09
  #mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles"), date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% # 11.16, 11.01
  raw_acc_to_long_df() %>% # 27.86
  accel_raw2mss() %>% 
  calc_odba_whole_pulse() %>% 
  mutate(bird.month.year = zfile) %>% 
    saveRDS(paste("data_files/rds/bird_month_odba/", outfile, sep = ""))

}


#
# checking sensitivity of ODBA to moving average window size ----
# this only needed for testing purposes, once best MA value is selected don't need to keep running this
# currently calculating ODBA using the entire pulse, since the pulses are only ~ 6 sec.
system.time(odba_ma5 <- calc_odba(acc_long, 5))
odba_ma10 <- calc_odba(acc_long, 10)
odba_ma20 <- calc_odba(acc_long, 20)
odba_ma30 <- calc_odba(acc_long, 30)
odba_ma40 <- calc_odba(acc_long, 40)

odba_whole_pulse <- calc_odba_whole_pulse(acc_long)


comp_odba <- full_join(odba_ma5 %>% dplyr::select(study.local.timestamp, odba05 = odba.timestamp),
                       odba_ma10 %>% dplyr::select(study.local.timestamp, odba10 = odba.timestamp)) %>% 
  full_join(., odba_ma20 %>% dplyr::select(study.local.timestamp, odba20 = odba.timestamp)) %>% 
  full_join(., odba_ma30 %>% dplyr::select(study.local.timestamp, odba30 = odba.timestamp)) %>% 
  full_join(., odba_ma40 %>% dplyr::select(study.local.timestamp, odba40 = odba.timestamp)) %>% 
  full_join(., odba_whole_pulse %>% dplyr::select(study.local.timestamp, odba.pulse = odba.timestamp))


comp_odba_summary <- comp_odba %>% 
  pivot_longer(cols = contains("odba"), names_to = "moving.av.window", values_to = "odba") %>% 
  group_by(moving.av.window) %>% 
  summarise(min.odba = min(odba),
            mean.odba = mean(odba),
            max.odba = max(odba))


ggplot(comp_odba_summary, aes(x = moving.av.window, y = mean.odba)) +
  geom_point()



# 

# calculate ODBA for a single bird X month X year
system.time(odba <- full_odba_maker("GREG_2", "1", "2018"))

# calculate ODBA for all data
system.time(odba <- map(acc_files[13:20], full_odba_maker))


# some acc files are bigger and need extra splitting (GREG_1)
# processing by day 

zfile = "GREG_1_ACC_9_2017"
outfile <- gsub("ACC", "ODBA", zfile)
  
zacc <- readRDS(paste("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/rds/bird_month_acc/", zfile, sep = "")) %>% # 8.75 sec
  filter(eobs.acceleration.axes == "XYZ", eobs.acceleration.sampling.frequency.per.axis == 10) %>% 
  mutate(date = date(study.local.timestamp))

zacc_distinct_days <- distinct(zacc, date) 


odba_maker_by_day <- function(zday) {
zacc_day <- zacc %>% 
  filter(date == zday)

samples_per_axis <- ((str_count(as.character(zacc_day$eobs.accelerations.raw), " ") %>% data.frame() %>% max() + 1)/length(acc_axes))[[1]]
 
bird_acc_sep <- zacc_day %>% 
  dplyr::select(event.id, bird = individual.local.identifier, study.local.timestamp, eobs.accelerations.raw, -eobs.acceleration.axes, -eobs.acceleration.sampling.frequency.per.axis) %>% 
  separate(eobs.accelerations.raw, into = paste(rep(acc_axes, samples_per_axis), rep(1:samples_per_axis, each = length(acc_axes)), sep = "_"))

bird_acc_sep_long <- bird_acc_sep %>% 
  pivot_longer(contains(c("x_", "y_", "z_")), names_to = "axis.num", values_to = "acc.raw") %>% 
  mutate(acc.raw = as.numeric(acc.raw))

bird_acc_sep_long_sep <- bird_acc_sep_long %>% 
  separate(axis.num, into = c("axis", "sample.num"))

odba1 <- bird_acc_sep_long_sep %>% 
  mutate(sample.num = as.numeric(sample.num),
         acc.raw = as.numeric(acc.raw)) %>% 
  filter(!is.na(acc.raw)) %>% 
  accel_raw2mss() %>% 
  calc_odba_whole_pulse() %>% 
  mutate(bird.month.year = zfile)
}

system.time(map_df(zacc_distinct_days$date, odba_maker_by_day)%>% 
    saveRDS(paste("data_files/rds/bird_month_odba/", outfile, sep = "")))


# some individual files may cause errors 
# using safely() helps ID bad files
safe_full_odba_maker <- safely(full_odba_maker)

system.time(odba <- pmap(list(bird_month$bird, bird_month$month, bird_month$year), safe_full_odba_maker))

odba_df <- map_df(odba, 1)
odba_error <- map(odba, 2)

saveRDS(odba_df, paste("data_files/rds/odba/odba", Sys.Date(), sep = "_"))
saveRDS(odba_error, paste("data_files/rds/odba/odba_error", Sys.Date(), sep = "_"))

# check which files caused problems

odba_error <- readRDS("data_files/rds/odba/odba_error_2020-08-27")




## combine all bird month odba files

read_bird_month_odba <- function(odbafile) {
odba <- readRDS(paste("data_files/rds/bird_month_odba/", odbafile, sep = ""))
}

all_odba <- map_df(odba_files$zfile, read_bird_month_odba)


