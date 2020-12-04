####

## interactive plot for hetp data with shiny


library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(chron)

## read in the hetp_use data table created by data_visualization.R
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201901.csv")


## fix the format for the date fields
hetp_1day <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
         ztime = times(paste(hour(timestamp), minutes(timestamp), second(timestamp), sep = ":")),
         dec.hr = as.numeric(ztime),
         dec.hr = round(dec.hr*24, 2)) %>% 
  select(date, timestamp, dec.hr, lat = location_lat, lon = location_long, bird, everything()) %>% 
  group_by(bird, date) %>% 
  distinct(dec.hr, .keep_all = TRUE) %>% 
  arrange(bird, date, dec.hr) %>% 
  data.frame()



##############################################################


## the user interface part
ui_1day = fluidPage(div(style = "background-color: skyblue;",
                   
                   fluidRow(column(8, 
                                   offset = 1, 
                                   h1("Where do Great Egrets go during different times and tides?", 
                                                     style = "font-family: 'Source Sans Pro';"))),
                   
                   fluidRow(column(3, 
                                   offset = 1, 
                                   div(style = "height:70px", 
                                       selectInput(inputId = "bird", 
                                                   label = "Select a bird", 
                                                   choices = sort(unique(hetp_1day$bird)))),
                                   
                                   div(style = "height:80px", 
                                       dateInput(inputId = "date", 
                                                 label = "Select a date", 
                                                 min = min(hetp_1day$date), max = max(hetp_1day$date),
                                                 value = min(hetp_1day$date)+19))),
                   
                            column(7, 
                                   offset = 1,
                                   div(style = "height:80px", 
                                       sliderInput(inputId = "time", 
                                                   label = "Select a time, or press the play button", 
                                                   min = min(hetp_1day$dec.hr), max = max(hetp_1day$dec.hr),
                                                   value = c(min(hetp_1day$dec.hr), min(hetp_1day$dec.hr)+0.5),
                                                   step = 0.5, animate = animationOptions(interval = 1000, loop = TRUE))))),
                   
                   fluidRow(column(3, 
                                   offset = 1, 
                                   plotOutput("eel.avail")),
                            column(7, 
                                   #offset = -1, 
                                   leafletOutput("MapPlot1")))))



## the server part  
server_1day = function(input, output) {
  
  output$MapPlot1 <- renderLeaflet({
    leaflet()  %>% 
      addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>% 
      addProviderTiles("Stamen.Terrain", group = "Basic terrain") %>%
      setView(lng=median(hetp_1day$lon), 
              lat=median(hetp_1day$lat), zoom=10)
  })
  

  observe({
    
    zdate <- input$date 
    zbird <- input$bird
    lwr_ztime <- input$time[1]
    upr_ztime <- input$time[2]
    

    bird_date <- hetp_1day %>% 
      filter(date == zdate,
             bird == zbird)
    
    bird_date_time <- hetp_1day %>% 
      filter(date == zdate,
             bird == zbird,
             dec.hr >= lwr_ztime,
             dec.hr <= upr_ztime)
    
    
    leafletProxy("MapPlot1") %>% clearShapes() %>% clearMarkers() %>% 
      addPolylines(lng = bird_date_time$lon,
                   lat = bird_date_time$lat, weight = 3, color = "orangered")%>% 
      addCircleMarkers(lng = bird_date_time$lon,
                   lat = bird_date_time$lat, color = "orangered", radius = 1) %>%
      addLayersControl(baseGroups = c("Basic terrain", "Satellite image"))
    
    output$eel.avail <- renderPlot(ggplot(data = bird_date)+
                                     theme(panel.background = element_rect(fill="gray50"),
                                           panel.grid.minor.y = element_blank(),
                                           panel.grid.major = element_blank(),
                                           plot.background = element_rect(fill = 'skyblue', colour = 'skyblue'),
                                           legend.position="none") +
                                     geom_rect(aes(xmin = dawn.time, xmax = dusk.time , ymin = -Inf, ymax = Inf),
                                               alpha = .1, fill = "gray90")+
                                     geom_line(aes(x=timestamp, y=water.level))+ 
                                     geom_line(data = bird_date_time, aes(x=timestamp, y=water.level, color = "red", size = 3))+
                                     geom_hline(yintercept = 1) +
                                     ylim(c(-1.5, 6))+
                                     xlab("")+
                                     ylab("Tide height")
    )
  })
  
}




shinyApp(ui_1day, server_1day)



##################################

