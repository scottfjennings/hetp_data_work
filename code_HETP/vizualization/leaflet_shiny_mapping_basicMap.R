####

## interactive plot for hetp data with shiny


library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(hms)

## read in the hetp_use data table created by data_visualization.R
hetp_start <- read.csv("data_files/GPS_with_covariates/hetpGPS_with_covariates201706_201901.csv")


## fix the format for the date fields
hetp <- hetp_start %>% 
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         timestamp = as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S"),
         dusk.time = as.POSIXct(as.character(dusk.time), format="%Y-%m-%d %H:%M:%S"),
         dawn.time = as.POSIXct(as.character(dawn.time), format="%Y-%m-%d %H:%M:%S"),
         mo.da = paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep = ""),
         #hr=hour(timestamp),
         #hr.mn=as.POSIXct(paste(hour(timestamp), ":", minute(timestamp), ":00", sep=""), format="%H:%M:%S"),
         hr.mn.sec=as.character(paste(hour(timestamp), minute(timestamp), second(timestamp), sep = ":"))) %>% 
  select(date, ztimestamp = timestamp, lat = location_lat, lon = location_long, bird, mo.da, hr.mn.sec, water.level, inlight)



##############################################################################################33
## good working set of code
## the user interface part
ui = fluidPage(div(style = "background-color: skyblue;",
                   
                   fluidRow(column(7, offset = 1, h1("Explore movement patterns of Great Egrets.", 
                                                     style = "font-family: 'Source Sans Pro';"))),
                   
                   
                   
                   fluidRow(column(3, offset = 1, 
                                   div(style = "height:70px", 
                                       selectInput(inputId = "bird", 
                                                   label = "Select a bird", 
                                                   choices = sort(unique(hetp$bird))))),
                            
                            column(7, 
                                   #offset = 1, 
                                   div(style = "height:80px", 
                                       dateRangeInput('dateRange',
                                                      label = 'Select start and end dates',
                                                      start = max(hetp$date)-60, end = max(hetp$date)-30)
                                   ))),
                   
                   
                   fluidRow(column(12,
                                   leafletOutput("MapPlot1"), h5("The green dot shows the bird's first location in the date range, the blue dot shows the last location, and the pink line shows everywhere the bird went during the date range.")))))





## the server part  
server = function(input, output) {
  
  output$MapPlot1 <- renderLeaflet({
    leaflet()  %>% 
      addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>% 
      addProviderTiles("Stamen.Terrain", group = "Basic terrain") %>%
      setView(lng=median(hetp$lon), 
              lat=median(hetp$lat), zoom=10)
  })
  
  
  
  observe({
    
    zdate <- input$dateRange 
    zbird <- input$bird
    lwr.zdate <- min(input$dateRange)
    upr.zdate <- max(input$dateRange)
    
    
    #zdate <- c(as.Date("2018-05-01"), as.Date("2018-10-15"))
    #zbird <- "GREG_4"
    
    birdies <- hetp %>% 
      filter(findInterval(hetp$date, zdate, rightmost.closed=TRUE) == 1,
             hetp$bird %in% zbird)
    
    #interval.divider <- round(nrow(birdies)/1000, 0)
    
    if(nrow(birdies) > 500){
      birdies <- birdies[seq(1, NROW(birdies), by = round(nrow(birdies)/500, 0)),]
    } else {
      birdies <- birdies
    }
    
    
    
    
    firstbirdies <- birdies %>% 
      filter(ztimestamp == min(ztimestamp))
    lastbirdies <- birdies %>% 
      filter(ztimestamp == max(ztimestamp))
    
    
    leafletProxy("MapPlot1") %>% clearShapes() %>% clearMarkers() %>% 
      # add line connecting all locations
      addPolylines(lng = birdies$lon, lat = birdies$lat, 
                   weight = 3, color = "orangered") %>%
      # add point for each location
      addCircleMarkers(lng = birdies$lon, lat = birdies$lat, radius=3, 
                       popup=paste0("<b/>", birdies$bird,"</b>", 
                                    "<br>Date: ", birdies$mo.da, "-", year(birdies$date), 
                                    "<br> Time (24-hr format): ", birdies$hr.mn.sec,
                                    "<br> Tomales tidal level: ", round(birdies$water.level, 1),
                                    "<br> During daylight? ", birdies$inlight), 
                       color="orangered")%>%
      # add different point for first location
      addCircleMarkers(lng = firstbirdies$lon, lat = firstbirdies$lat, radius=3, 
                       popup=paste0("<b>", "First point: ","<b/>", firstbirdies$bird, 
                                    "</b>", "<br>Date: ", firstbirdies$mo.da, "-", year(firstbirdies$date), 
                                    "<br> Time (24-hr format): ", firstbirdies$hr.mn.sec,
                                    "<br> Tomales tidal level: ", round(firstbirdies$water.level, 1),
                                    "<br> During daylight? ", firstbirdies$inlight), 
                       color="green")%>%
      # add different point for last location
      addCircleMarkers(lng = lastbirdies$lon, lat = lastbirdies$lat, radius=3, 
                       popup=paste0("<b>", "Last point: ","<b/>", lastbirdies$bird,"</b>", 
                                    "<br>Date: ", lastbirdies$mo.da, "-", year(lastbirdies$date), 
                                    "<br> Time (24-hr format): ", lastbirdies$hr.mn.sec,
                                    "<br> Tomales tidal level: ", round(lastbirdies$water.level, 1),
                                    "<br> During daylight? ", lastbirdies$inlight), 
                       color="blue")%>%
      addLayersControl(
        baseGroups = c("Basic terrain", "Satellite image")
      )

  })
  
}



shinyApp(ui, server)
