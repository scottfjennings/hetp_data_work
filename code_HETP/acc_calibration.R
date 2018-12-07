## code for checking GPRS tag accel sensor calibration


library(tidyverse)
library(stringr)
library(lubridate)

t5660 <- read.table("C:/Users/scott.jennings/Desktop/tags - local/tag5660_acc.txt")



t5660_split <- data.frame(do.call('rbind', strsplit(as.character(t5660$V1),',',fixed=TRUE))) %>% 
  rename(rec = X1, date = X2, day = X3, time = X4, acc_x = X5, acc_y = X6, acc_z = X7) %>% 
  mutate(datetime1 = paste(date, time),
         datetime = as.POSIXct(datetime1, format = "%d.%m.%Y %H:%M:%OS"))
op <- options(digits.secs=3)

ggplot(data = t5660_split, aes(x = datetime)) +
  geom_point(aes(y = acc_x, color = "red")) +
  geom_point(aes(y = acc_y, color = "green")) +
  geom_point(aes(y = acc_z, color = "blue"))
