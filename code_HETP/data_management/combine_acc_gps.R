


library(tidyverse)
source("code_HETP/data_management/hetp_utility_functions.r")


zbird = "GREG_1"
zmonth = 6
zyear = 2018
combine_acc_gps <- function(zbird, zmonth, zyear) {
bird_acc <- read_bird_month_acc_rds(zbird, zmonth, zyear) 
bird_acc2 <- bird_acc %>% 
  dplyr::select(-timestamp, -study.timezone) %>% 
  mutate(date.min = round_date(study.local.timestamp,unit="1 minutes")) %>% 
  rename(event.id.acc = event.id, study.local.timestamp.acc = study.local.timestamp)


bird_gps <- read_bird_month_gps_rds(zbird, zmonth, zyear)
bird_gps2 <- bird_gps %>% 
  dplyr::select(-timestamp, -study.timezone, -contains("gps")) %>% 
  mutate(date.min = round_date(study.local.timestamp,unit="1 minutes"))  %>% 
  rename(event.id.gps = event.id, study.local.timestamp.gps= study.local.timestamp)


bird_acc_gps <- full_join(bird_acc2, bird_gps2, by = c("individual.local.identifier", "date.min")) %>% 
  dplyr::select(contains("local.timestamp"), date.min, everything())

saveRDS(bird_acc_gps, paste("data_files/rds/bird_month_acc_gps/", zbird, "_acc_gps_", zmonth, "_", zyear, sep = ""))

}

combine_acc_gps("GREG_1", "6", "2018")

