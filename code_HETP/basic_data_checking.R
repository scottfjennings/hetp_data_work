### code for basic data checking

library(tidyverse)



acc_data <- read.csv("data_files/ACConly/HETP_ACConly_201809.csv")


axis_summary <- acc_data %>% 
  distinct(individual.local.identifier, eobs.acceleration.axes)
