####

## interactive plot for hetp data with shiny

## shows multiple days


library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(hms)

## read in the hetp_use data table created by data_visualization.R
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201901.csv")

#hetp_start <- read.csv("data_files/GPS_with_covariates/greg2_summer17.csv")

## fix the format for the date fields
hetp <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp=as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
         dusk.time=as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time=as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
         mo.da=paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep=""),
         hr=hour(timestamp),
         hr.mn=as.POSIXct(paste(hour(timestamp), ":", minute(timestamp), ":00", sep=""), format="%H:%M:%S"),
         hr.mn.sec=as.hms(paste(hour(timestamp), minute(timestamp), second(timestamp), sep=":"))) %>% 
  select(date, ztimestamp=timestamp, lat = location_lat, lon = location_long, bird, everything())

rm(hetp_start)
hetp <- hetp %>% 
  filter(bird == "GREG_2",
         month(date) > 3 & month(date) < 7 & 
           year(date) == 2017) %>% 
  select(date, ztimestamp, lat, lon, bird, water.level, dawn.time, dusk.time, inlight, num.hours, mo.da, hr, hr.mn, hr.mn.sec)

write.csv(hetp, "data_files/GPS_with_covariates/greg2_summer18.csv", row.names = F)

hetp <- read.csv("data_files/GPS_with_covariates/greg2_summer17.csv") %>% 
    mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         ztimestamp=as.POSIXct(as.character(ztimestamp), format="%Y-%m-%d %H:%M:%S"))



################################################
#foof <- hetp %>% 
#  group_by(bird, month(date)) %>% 
#  summarise(num_points = n(),
#            min_tide = min(water.level),
#            med_tide = median(water.level),
#            max_tide = max(water.level))


 
################################################

################################################
##  THIS IS THE WORKING COPY OF THE MOST RECENT MAP, WHERE I FIDDLE WITH NEW STUFF


## the user interface part
ui_EelPerDay = fluidPage(div(style = "background-color: skyblue;",
                   
                   fluidRow(column(7, offset = 1, h1("Explore movement patterns of Great Egrets.", 
                                                     style = "font-family: 'Source Sans Pro';"))),
                   
                   
                   
                   fluidRow(column(3, offset = 1, 
                                   div(style = "height:70px", 
                                       selectInput(inputId = "bird", 
                                                   label = "Select a bird", 
                                                   choices = sort(unique(hetp$bird))))),
                                                        
                            column(5, offset = 1, 
                                   div(style = "height:80px", 
                                       sliderInput(inputId = "dateStart", 
                                                   label = "Select start date", 
                                                   min = min(hetp$date), max = max(hetp$date),
                                                   value = min(hetp$date) + 30, width = "95%", 
                                                   step = 1, animate = animationOptions(loop = TRUE)))))
                            ,
                           # column(7, 
                            #       #offset = 1, 
                            #       div(style = "height:80px", 
                            #         dateInput('dateStart',
                            #                          label = 'Select start date',
                            #                          value = min(hetp$date))
                            #           ))),
                   
                   
                   fluidRow(column(3, offset = 1, h5("Availability of low tide foraging each day"), plotOutput("eel.avail")),
                            column(7, offset = -1,
                                   leafletOutput("MapPlot1"), h5("The green dot shows the bird's first location in the date range, the blue dot shows the last location, and the pink line shows everywhere the bird when during the date range.")))))





## the server part  
server_EelPerDay = function(input, output) {
  
  output$MapPlot1 <- renderLeaflet({
    leaflet()  %>% 
      addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>% 
      addProviderTiles("Stamen.Terrain", group = "Basic terrain") %>%
      setView(lng=median(hetp$lon), 
              lat=median(hetp$lat), zoom=10)
  })
  
  

  observe({
    
    zdate <- input$dateStart 
    zbird <- input$bird
    #lwr.zdate <- min(input$dateRange)
    #upr.zdate <- max(input$dateRange)
    
    birdies <- hetp %>% 
      filter(date >= zdate, date <= zdate + 4, bird == zbird)
    
    foo <- hetp %>% 
      filter(date >= zdate, date <= zdate + 4, bird == zbird) %>% 
      distinct(date, num.hours, .keep_all = TRUE)
    
    
    firstbirdies <- birdies %>% 
      filter(ztimestamp == min(ztimestamp))
    laststbirdies <- birdies %>% 
      filter(ztimestamp == max(ztimestamp))
    
    
    leafletProxy("MapPlot1") %>% clearShapes() %>% clearMarkers() %>% 
      # add line connecting all locations
      addPolylines(lng = birdies$lon, lat = birdies$lat, 
                   weight = 3, color = "orangered") %>%
      # add point for each location
      addCircleMarkers(lng = birdies$lon, lat = birdies$lat, radius=3, 
                       popup=paste0("<b/>", birdies$bird,"</b>", "<br>Date: ", 
                                    firstbirdies$mo.da, "-", year(birdies$date), 
                                    "<br> Time (24-hr format): ", birdies$hr.mn.sec), 
                       color="orangered")%>%
      # add different point for first location
      addCircleMarkers(lng = firstbirdies$lon, lat = firstbirdies$lat, radius=3, 
                       popup=paste0("<b>", "First point: ","<b/>", firstbirdies$bird,"</b>", "<br>Date: ", 
                                    firstbirdies$mo.da, "-", year(firstbirdies$date), "<br> Time (24-hr format): ",
                                    firstbirdies$hr.mn.sec), 
                       color="green")%>%
      # add different point for last location
      addCircleMarkers(lng = laststbirdies$lon, lat = laststbirdies$lat, radius=3, 
                       popup=paste0("<b>", "Last point: ","<b/>", laststbirdies$bird,"</b>", "<br>Date: ", 
                                    laststbirdies$mo.da, "-", year(laststbirdies$date), "<br> Time (24-hr format): ",
                                    laststbirdies$hr.mn.sec), 
                       color="blue")%>%
      addLayersControl(
        baseGroups = c("Basic terrain", "Satellite image")
      )
    # add the plot
    output$eel.avail <- renderPlot(    ggplot(data = foo, aes(x = date, y = num.hours))+
                                         geom_col(width = 0.5, fill = "red")+
                                         theme(panel.background = element_blank(),
                                               panel.grid.minor.y = element_blank(),
                                               panel.grid.major = element_blank(),
                                               legend.position="none")+
                                         ylim(0, 6)+
                                         ylab("Number of hours")+
                                         xlab("")
    )
  })
  
}



shinyApp(ui_EelPerDay, server_EelPerDay)









################################################
## THIS IS THE MAIN MAP THAT WORKS AND IS MY MOST CURRENT REPRESENTATION OF THE DATA
## includes ability to animate with date range slider and select just a single bird, and shows a line instead of points

## the user interface part
ui = fluidPage(div(style = "background-color: skyblue;",
                   
                   fluidRow(column(7, offset = 1, h1("Explore movement patterns of Great Egrets.", 
                                                     style = "font-family: 'Source Sans Pro';"))),
                   
                   
                   
                   fluidRow(column(3, offset = 1, 
                                   div(style = "height:70px", 
                                       selectInput(inputId = "bird", 
                                                   label = "Select a bird", 
                                                   choices = sort(unique(hetp$bird))))),
                            
                            column(7, offset = 1, 
                                   div(style = "height:80px", 
                                       sliderInput(inputId = "date", 
                                                   label = "Select a date range", 
                                                   min = min(hetp$date), max = max(hetp$date),
                                                   value = c(min(hetp$date), min(hetp$date)+1), width = "95%", 
                                                   step = 1, animate = animationOptions(loop = TRUE))))),
                   
                   
                   fluidRow(column(3, offset = 1,  h5("The number of daylight hours the eelgrass was available for foraging, each day."), plotOutput("eel.avail")),
                            column(7, offset = -1, h5("The green dot shows the bird's first location in the date range, the red dot shows the last location, and the pink line shows everywhere the bird went during the date range."),
                                   leafletOutput("MapPlot1")))))



## the server part  
server = function(input, output) {
  
  output$MapPlot1 <- renderLeaflet({
    leaflet()  %>% 
      addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>% 
      addProviderTiles("Stamen.Terrain", group = "Basic terrain") %>%
      setView(lng=median(hetp$lon), 
              lat=median(hetp$lat), zoom=10)
  })
  
  
  output$eel.avail <- renderPlot(ggplot(data = subset(hetp, date=date))+
                                   geom_line(aes(x=ztimestamp, y=water.level)))
  
  observe({
    
    zdate <- input$date 
    zbird <- input$bird
    lwr.zdate <- min(input$date)
    upr.zdate <- max(input$date)
    
    birdies <- hetp %>% 
      filter(findInterval(hetp$date, zdate, rightmost.closed=TRUE) == 1,
             hetp$bird %in% zbird)
    
    foo <- hetp %>% 
      filter(date >= lwr.zdate - 1, date <= upr.zdate + 1, bird == zbird) %>% 
      distinct(date, num.hours, .keep_all = TRUE)
    
    foo.date <- foo %>% 
      filter(findInterval(foo$date, zdate, rightmost.closed=TRUE) == 1)
    
    
    firstbirdies <- birdies %>% 
      filter(ztimestamp == min(ztimestamp))
    laststbirdies <- birdies %>% 
      filter(ztimestamp == max(ztimestamp))
    
    leafletProxy("MapPlot1") %>% clearShapes() %>% clearMarkers() %>% 
      addPolylines(lng = birdies$lon,
                   lat = birdies$lat, weight = 3, color = "orangered") %>%
      addCircleMarkers(lng = birdies$lon,
                       lat = birdies$lat, radius=3, popup=paste0("<b/>", birdies$bird,"</b>", "<br>Date: ", firstbirdies$mo.da, "-", year(birdies$date), "<br> Time (24-hr format): ", birdies$hr.mn.sec), color="orangered") %>%
      addCircleMarkers(lng = firstbirdies$lon,
                       lat = firstbirdies$lat, radius=3, popup=paste0("<b>", "First point: ","<b/>", firstbirdies$bird,"</b>", "<br>Date: ", firstbirdies$mo.da, "-", year(firstbirdies$date), "<br> Time (24-hr format): ", firstbirdies$hr.mn.sec), color="green")%>%
      addCircleMarkers(lng = laststbirdies$lon,
                       lat = laststbirdies$lat, radius=3, popup=paste0("<b>", "Last point: ","<b/>", laststbirdies$bird,"</b>", "<br>Date: ", laststbirdies$mo.da, "-", year(laststbirdies$date), "<br> Time (24-hr format): ", laststbirdies$hr.mn.sec), color="red")%>%
      addLayersControl(
        baseGroups = c("Basic terrain", "Satellite image")
      )
    output$eel.avail <- renderPlot(    ggplot(data = foo, aes(x = date, y = num.hours))+
                                         geom_col(width = 0.5, fill = "white", colour = "white")+
                                         geom_col(data = foo.date, aes(x = date, y = num.hours, fill = "red"), width = 0.5) +
                                         theme(panel.background = element_blank(),
                                               panel.grid.minor.y = element_blank(),
                                               panel.grid.major = element_blank(),
                                               legend.position="none")+
                                         ylim(0, 6)
    )
  })
  
}



shinyApp(ui, server)



#########################################################################

## a test server chunk to double check what the inputs come out as
ui = fluidPage(
  selectInput(inputId = "bird", 
              label = "Select a bird", 
              choices = sort(unique(hetp$bird))),
  
  verbatimTextOutput("bird"),
  
  dateInput(inputId = "date", 
            label = "Select a date", 
            min = min(hetp$date), max = max(hetp$date),
            value = min(hetp$date)+5),
  
  verbatimTextOutput("date"),
  
  numericInput(inputId = "plusxdays",
               label = "Show data for this many days following selected date",
               value = "5",
               max = 8,
               min = 1),
  
  verbatimTextOutput("plusxdays"),
  
  verbatimTextOutput("upr.zdate"),
  
  sliderInput(inputId = "dateSlide", 
              label = "Select start date", 
              min = min(hetp$date), max = max(hetp$date),
              value = min(hetp$date) + 30, width = "95%",
              step = 1, animate = animationOptions(loop = TRUE)))


server = function(input, output) {
  
  output$bird <- renderPrint({ input$bird })
  output$date <- renderPrint({ input$date })
  output$plusxdays <- renderPrint({ input$plusxdays })
  output$upr.zdate <- renderPrint({ input$date + input$plusxdays })
  output$dateSlide <- renderPrint({ input$dateSlide})
  
}
shinyApp(ui, server)


