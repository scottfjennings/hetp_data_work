

library(tidyverse)
library(lubridate)
library(move)




## figuring out how to parse acc data
hetp_acc <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/ACConly/HETP_ACConly_201706_202002.csv")

mb_log <- movebankLogin(username = "scott.jennings", password = "3grets0nT#eMove!")
greg2_acc <- read.csv("C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/data_files/ACConly/GREG_2_ACConly_201706_202006.csv")



axis120 <- data.frame(acc = c(paste("acc", seq(1:120), sep = "_")),
                      axis = rep(c("x", "y", "z"), 120/3))


greg2_sub <- greg2_acc %>% 
  dplyr::select(event.id, eobs.accelerations.raw) %>% 
  separate(eobs.accelerations.raw, into = c(paste("acc", seq(1:120), sep = "_")))

greg2_sub_longer <- greg2_sub %>% 
  pivot_longer(cols = contains("acc"), names_to = c("acc"), values_to = c("acc_val")) %>% 
  full_join(., axis120) 

ev_id <- distinct(greg2_acc, event.id)


