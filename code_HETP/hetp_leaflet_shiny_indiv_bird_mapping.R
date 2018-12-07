####

## interactive plot for hetp data with packages leaflet and shiny

# data management packages
library(tidyverse)
library(lubridate)

# mapping packages
library(leaflet)
library(shiny)
library(lubridate)
library(tidyverse)

## be sure to set the correct pathway for whereever you stored the hetp_plus.csv file
hetp <- read.csv("data_files/hetp_use_temp.csv")


## fix the format for the date fields
hetp <- within(hetp, {
  date = as.Date(as.character(date), format = "%Y-%m-%d")
  timestamp=as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S")
  mo.da=paste(month(timestamp, label = TRUE, abbr = TRUE), "-", day(timestamp), sep="")
  hr.mn=paste(hour(timestamp), ":", minute(timestamp), sep="")
}) %>% 
  select(date, ztimestamp=timestamp, lat = location_lat, lon = location_long, bird, everything())

z#############################################################################################
### make a separate map for each bird

# first filter to one bird then subsample each 5th record (can adjust this by changing the second number in rep(1:5)
## GREG_1
greg1 <- hetp %>% 
  filter(bird == "GREG_1") 
## then make the shiny app
greg1_shiner <- shinyApp(
  ui = fluidPage(
    titlePanel("Bird ID: GREG_1"),
    
    sliderInput(inputId = "date", 
                label = "Select a date range (no points will show if start and end dates are the same day)", 
                min = min(greg1$Date), max = max(greg1$Date), value = greg1$Date, step = 1),
    leafletOutput("MapPlot1")
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Stamen.Terrain") %>% 
        setView(lng=-122.9, lat=38.22, zoom=13)
    })
    
    observe({
      
      date <- input$date
      bird <- input$bird
      
      birdies <- greg1 %>% 
        filter(findInterval(greg1$Date, date) == 1)
      
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = birdies$long,
                         lat = birdies$lat,
                         opacity = 0,
                         radius = 3, 
                         fillOpacity = 1)
    })
  },
  options = list(height = 600)
)


###############################################
## GREG_2
greg2 <- hetp %>% 
  filter(bird == "GREG_2") 

## then make the shiny app
greg2_shiner <- shinyApp(
  ui = fluidPage(
    titlePanel("Bird ID: GREG_2"),
    
    sliderInput(inputId = "date", 
                label = "Select a date range (no points will show if start and end dates are the same day)", 
                min = min(greg2$Date), max = max(greg2$Date), value = greg2$Date, step = 1),
    leafletOutput("MapPlot1")
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Stamen.Terrain") %>% 
        setView(lng=-122.9, lat=38.22, zoom=13)
    })
    
    observe({
      
      date <- input$date
      bird <- input$bird
      
      birdies <- greg2 %>% 
        filter(findInterval(greg2$Date, date) == 1)
      
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = birdies$long,
                         lat = birdies$lat,
                         opacity = 0,
                         radius = 3, 
                         fillOpacity = 1)
    })
  },
  options = list(height = 600)
)


###############################################
## GREG_3
greg3 <- hetp %>% 
  filter(bird == "GREG_3") 

## then make the shiny app
greg3_shiner <- shinyApp(
  ui = fluidPage(
    titlePanel("Bird ID: GREG_3"),
    
    sliderInput(inputId = "date", 
                label = "Select a date range (no points will show if start and end dates are the same day)", 
                min = min(greg3$Date), max = max(greg3$Date), value = greg3$Date, step = 1),
    leafletOutput("MapPlot1")
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Stamen.Terrain") %>% 
        setView(lng=-122.9, lat=38.22, zoom=13)
    })
    
    observe({
      
      date <- input$date
      bird <- input$bird
      
      birdies <- greg3 %>% 
        filter(findInterval(greg3$Date, date) == 1)
      
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = birdies$long,
                         lat = birdies$lat,
                         opacity = 0,
                         radius = 3, 
                         fillOpacity = 1)
    })
  },
  options = list(height = 600)
)



