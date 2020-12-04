

library(tidyverse)
library(lubridate)
library(scales)
####################### 404
blake5min_tides <- read.csv("data_files/habitat/blake5min_tides2019-09-26.csv")
blake5min_tides$date = as.Date(blake5min_tides$timestamp)
blake_tide_plot_data <- filter(blake5min_tides, date >= as.Date("2018-05-01") & date <= as.Date( "2018-10-05"))
ggplot(aes(x = timestamp, y = water.level), data = blake_tide_plot_data) +
  geom_line()  + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(aes(yintercept = 1), size = 1, color = "green") +
  xlab("") +
  ylab("Tidal height")# neato

ggsave("figures_output/blake_tide_may_1ftline_20190926.jpg", width = 8, height = 4, units = "in")

####################### 404
blake_eelgrass_available <- read.csv("data_files/habitat/blake_eelgrass_available2019-09-26.csv") %>% 
  mutate(date = as.POSIXct(date),
         month = month(date),
         year = year(date),
         md = paste(month(date), day(date), sep = "-"))
blake_eelgrass_available_filt <- blake_eelgrass_available %>% 
  filter(year(date) == 2017, month(date) == 7)

ggplot(aes(x = date, y = num.hours), data = blake_eelgrass_available_filt) +
  geom_col() + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  xlab("") +
  ylab("Number of daylight hours eelgrass available")+
  scale_x_datetime(labels = date_format("%b%d")) +
  geom_vline(xintercept=as.numeric(blake_eelgrass_available_filt$date[10]), color = "red", size = 2) +
  geom_vline(xintercept=as.numeric(blake_eelgrass_available_filt$date[25]), color = "red", size = 2)


ggsave("figures_output/eelAvail_byDayJul17_10_24lines.jpg", width = 8, height = 4, units = "in")






ggplot(aes(x = date, y = num.hours), data = blake_eelgrass_available_filt) +
  geom_col() + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  xlab("") +
  ylab("Number of daylight hours eelgrass available")+
  #scale_x_datetime(labels = date_format("%b%d")) +
    geom_rect(aes(xmax=blake_eelgrass_available_filt$date[10], 
                  xmin=blake_eelgrass_available_filt$date[25], 
                             ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3) 
              +
    geom_rect(aes(xmax=as.numeric(blake_eelgrass_available_filt$date[25]), 
                             xmin=as.numeric(blake_eelgrass_available_filt$date[31]), 
                             ymin=-Inf, ymax=Inf), 
              fill='gray80', alpha=0.3)
