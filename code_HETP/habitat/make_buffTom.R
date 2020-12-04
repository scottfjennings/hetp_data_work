library(tidyverse)




buffTom20 <- read.csv("data_files/GPS_with_covariates/hetp_buffTom20km.csv") %>% 
  mutate(within20Tom = ifelse(is.na(ORIG_FID), FALSE, TRUE )) %>% 
  dplyr::select(event_id, within20Tom)
buffTom10 <- read.csv("data_files/GPS_with_covariates/hetp_buffTom10km.csv") %>% 
  mutate(within10Tom = ifelse(is.na(ORIG_FID), FALSE, TRUE )) %>% 
  dplyr::select(event_id, within10Tom)
buffTom5 <- read.csv("data_files/GPS_with_covariates/hetp_buffTom5km.csv") %>% 
  mutate(within5Tom = ifelse(is.na(ORIG_FID), FALSE, TRUE )) %>% 
  dplyr::select(event_id, within5Tom)
buffTom1 <- read.csv("data_files/GPS_with_covariates/hetp_buffTom1km.csv") %>% 
  mutate(within1Tom = ifelse(is.na(ORIG_FID), FALSE, TRUE )) %>% 
  dplyr::select(event_id, within1Tom)

buffTom <- full_join(buffTom20, buffTom10) %>% 
  full_join(., buffTom5) %>% 
  full_join(., buffTom1)
write.csv(buffTom, "data_files/habitat/buffTom.csv", row.names = F)
