library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(chron)
library(scales)
library(hms)

## read in the hetp_use data table created by data_visualization.R
hetp_start <- read.csv("data_files/hetp_use_temp.csv")


## fix the format for the date fields
hetp_1day <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S", tzone = "America/Los_Angeles"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"), 
         ztime = times(paste(hour(timestamp), minutes(timestamp), second(timestamp), sep = ":"))) %>%
  select(date, timestamp, lat = location_lat, lon = location_long, bird, everything()) %>% 
  data.frame()

tz(hetp_1day$timestamp) <- "America/Los_Angeles"


greg1_20170627 <- hetp_1day %>% 
  filter(date == "2017-06-27", bird == "GREG_1") %>% 
  droplevels() %>% 
  arrange(timestamp) %>%
  select(date, timestamp, lat, lon, bird, utm.easting, utm.northing, utm.zone, inlight, dawn.time, dusk.time, water.level, num.hours, ztime) %>% 
  arrange(date) 

greg1_20170627high1 <- greg1_20170627 %>% 
  filter(hour(timestamp) < 10) %>% 
  summarise(highest = max(water.level))

greg1_20170627high2 <- greg1_20170627 %>% 
  filter(hour(timestamp) > 10) %>% 
  summarise(highest = max(water.level))

greg1_20170627low1 <- greg1_20170627 %>% 
  filter(hour(timestamp) < 16) %>% 
  summarise(highest = min(water.level))

greg1_20170627low2 <- greg1_20170627 %>% 
  filter(hour(timestamp) > 16) %>% 
  summarise(highest = min(water.level))


greg1_20170627plot <- greg1_20170627 %>%
  mutate(diff = water.level - lag(water.level, default = first(water.level))) %>% 
  mutate(remove = ifelse(diff == 0 & (water.level != greg1_20170627high1[1,] |
                                     water.level != greg1_20170627high2[1,] |
                                     water.level != greg1_20170627low1[1,] |
                                     water.level != greg1_20170627low2[1,]), 1, 0)) %>% 
  filter(remove == 0)

write.csv(greg1_20170627, "greg1_20170627.csv", row.names = F)

greg1_20170627plot = greg1_20170627[seq(1, nrow(greg1_20170627), 3), ]

tiff("tide20170627_noReps_color6x6.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data = greg1_20170627plot, aes(x= timestamp, y=water.level))+
  theme(panel.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_line()+
  geom_line(data = filter(greg1_20170627plot, water.level <= 1), color = "#1b9e77", size = 1.5)+
  geom_line(data = filter(greg1_20170627plot, water.level >= 2, hour(timestamp) < 12), color = "#d95f02", size = 1.5)+
  geom_line(data = filter(greg1_20170627plot, water.level >= 2, hour(timestamp) > 12, hour(timestamp) < 22), color = "#d95f02", size = 1.5)+
  geom_line(data = filter(greg1_20170627plot, water.level >= 2, hour(timestamp) > 22), color = "#d95f02", size = 1.5)+
  ylim(c(-1.5, 6))+
  xlab("Time")+
  ylab("Tide height") +
  scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M", tz = "America/Los_Angeles"))

dev.off()

###------------------------------------------------------------------

  
  
greg1_20170704 <- hetp_1day %>% 
  filter(date == "2017-07-04", bird == "GREG_1") %>% 
  droplevels() %>% 
  arrange(timestamp) %>%
  select(date, timestamp, lat, lon, bird, utm.easting, utm.northing, utm.zone, inlight, dawn.time, dusk.time, water.level, num.hours, ztime) %>% 
  arrange(date) 

greg1_20170704high1 <- greg1_20170704 %>% 
  filter(hour(timestamp) < 16) %>% 
  summarise(highest = max(water.level))

greg1_20170704high2 <- greg1_20170704 %>% 
  filter(hour(timestamp) > 16) %>% 
  summarise(highest = max(water.level))

greg1_20170704low1 <- greg1_20170704 %>% 
  filter(hour(timestamp) < 12) %>% 
  summarise(highest = min(water.level))

greg1_20170704low2 <- greg1_20170704 %>% 
  filter(hour(timestamp) > 12) %>% 
  summarise(highest = min(water.level))


greg1_20170704plot <- greg1_20170704 %>%
  mutate(diff = water.level - lag(water.level, default = first(water.level))) %>% 
  mutate(remove = ifelse(diff == 0 & (water.level != greg1_20170704high1[1,] |
                                     water.level != greg1_20170704high2[1,] |
                                     water.level != greg1_20170704low1[1,] |
                                     water.level != greg1_20170704low2[1,]), 1, 0)) %>% 
  filter(remove == 0)

write.csv(greg1_20170704, "greg1_20170704.csv", row.names = F)

greg1_20170704plot = greg1_20170704[seq(1, nrow(greg1_20170704), 3), ]

tiff("tide20170704_noReps_color6x6.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data = greg1_20170704plot, aes(x= timestamp, y=water.level))+
  theme(panel.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  #geom_rect(aes(xmin = dawn.time, xmax = dusk.time , ymin = -Inf, ymax = Inf),
   #         alpha = .1, fill = "gray90")+
  geom_line()+
  geom_line(data = filter(greg1_20170704plot, water.level <= 1), color = "#1b9e77", size = 1.5)+
  geom_line(data = filter(greg1_20170704plot, water.level >= 2, hour(timestamp) < 4), color = "#d95f02", size = 1.5)+
  geom_line(data = filter(greg1_20170704plot, water.level >= 2, hour(timestamp) > 4, hour(timestamp) < 16), color = "#d95f02", size = 1.5)+
  geom_line(data = filter(greg1_20170704plot, water.level >= 2, hour(timestamp) > 16), color = "#d95f02", size = 1.5)+
  ylim(c(-1.5, 6))+
  xlab("Time")+
  ylab("Tide height") +
  scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M", tz = "America/Los_Angeles"))

dev.off()
