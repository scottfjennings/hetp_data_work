


library(tidyverse)
library(lubridate)
source("code_HETP/data_management/hetp_utility_functions.r")



bird_month <- readRDS("data_files/rds/bird_month")


# read_bird_month_acc_rds() is in hetp_utility_functions.R
# monthly rds files created by clip_write_data_files.R
zbird = "GREG_2"
zmonth = 8
zyear = 2017
bird_acc <- read_bird_month_acc_rds(zbird, zmonth, zyear) %>% 
  filter(eobs.acceleration.axes == "XYZ", eobs.acceleration.sampling.frequency.per.axis == 10) %>% 
  dplyr::select(-timestamp, -study.timezone) %>% 
  mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles"),
         date = as.Date(timestamp, tz = "America/Los_Angeles"),
         samples.per.axis = (str_count(as.character(eobs.accelerations.raw), " ") + 1)/3) %>% 
  separate(eobs.accelerations.raw, into = paste("acc", rep(acc_axes, samples_per_axis), rep(1:samples_per_axis, each = length(acc_axes)), sep = "_"))

 samples_per_axis <- ((str_count(as.character(bird_acc$eobs.accelerations.raw), " ") %>% data.frame() %>% distinct() + 1)/3)[[1]]
 
# bird_acc <- bird_acc %>% mutate(samples = (str_count(as.character(bird_acc$eobs.accelerations.raw), " ")), samples_per_axis = (samples + 1)/3)


raw_acc_to_long_df <- function(zacc) {
  
 acc_axes = c("x", "y", "z")

 samples_per_axis <- ((str_count(as.character(zacc$eobs.accelerations.raw), " ") %>% data.frame() %>% distinct() + 1)/length(acc_axes))[[1]]
 
  bird_acc_sep <- zacc %>% 
  dplyr::select(event.id, individual.local.identifier, study.local.timestamp, eobs.accelerations.raw, -eobs.acceleration.axes, -eobs.acceleration.sampling.frequency.per.axis) %>% 
  separate(eobs.accelerations.raw, into = paste("acc", rep(acc_axes, samples_per_axis), rep(1:samples_per_axis, each = length(acc_axes)), sep = "_"))

bird_acc_sep_long <- bird_acc_sep %>% 
  pivot_longer(contains("acc"), names_to = "axis.num", values_to = "acc.raw") %>% 
  separate(axis.num, into = c("trash", "axis", "sample.num")) %>% 
  mutate(sample.num = as.numeric(sample.num),
         acc.raw = as.numeric(acc.raw)) %>% 
  filter(!is.na(acc.raw)) %>% 
  dplyr::select(-trash)

}

# acc_long <- raw_acc_to_long_df(bird_acc)


# calculate ODBA using difference between each value and a moving average
calc_odba <- function(acc_long) {
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
  arrange(individual.local.identifier, study.local.timestamp, axis, sample.num) %>% 
  group_by(individual.local.identifier, study.local.timestamp, axis) %>% 
  mutate(axis.ma = mean(acc.raw),
         moving.av.window = "whole.pulse") %>% 
  filter(!is.na(axis.ma)) %>% 
  mutate(accel = acc.raw - axis.ma) %>% 
  ungroup()

odba_timestamp <- acc_ma %>% 
  group_by(individual.local.identifier, study.local.timestamp) %>% 
  summarise(odba.timestamp = sum(abs(accel))) %>% 
  ungroup()

return(odba_timestamp)
}

# function of piped functions to loop through multiple files ----


full_odba_maker <- function(zbird, zmonth, zyear) {
  
odba <- read_bird_month_acc_rds(zbird, zmonth, zyear) %>% 
  filter(eobs.acceleration.axes == "XYZ", eobs.acceleration.sampling.frequency.per.axis == 10) %>% 
  dplyr::select(-timestamp, -study.timezone) %>% 
  mutate(timestamp = as.POSIXct(study.local.timestamp, tz = "America/Los_Angeles"),
         date = as.Date(timestamp, tz = "America/Los_Angeles")) %>% 
  raw_acc_to_long_df() %>% 
  calc_odba() %>% 
  mutate(bird.id = zbird)

return(odba)
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





# calculate ODBA for a single bird X month X year
system.time(odba <- full_odba_maker("GREG_1", "9", "2017"))

# calculate ODBA for all data
system.time(odba <- pmap_df(list(bird_month$bird, bird_month$month, bird_month$year), full_odba_maker))


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




