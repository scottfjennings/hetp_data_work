

library(tidyverse)
library(lubridate)
library(chron)


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
       