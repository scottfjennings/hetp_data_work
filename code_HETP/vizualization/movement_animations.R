####

## interactive plot for hetp data with shiny

## shows multiple days


library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(hms)


hetp <- read.csv("data_files/GPS_with_covariates/greg2_summer18.csv") %>% 
    mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         ztimestamp=as.POSIXct(as.character(ztimestamp), format="%Y-%m-%d %H:%M:%S"))







################################################
## THIS IS THE MAIN MAP THAT WORKS AND IS MY MOST CURRENT REPRESENTATION OF THE DATA
## includes ability to animate with date range slider and select just a single bird, and shows a line instead of points

## the user interface part
ui = fluidPage(div(style = "background-color: skyblue;",
                   
                   
                   fluidRow(column(12, div(style = "height:80px", 
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
    lwr.zdate <- min(input$date)
    upr.zdate <- max(input$date)
    
    birdies <- hetp %>% 
      filter(findInterval(hetp$date, zdate, rightmost.closed=TRUE) == 1)
    
    foo <- hetp %>% 
      filter(date >= lwr.zdate - 1, date <= upr.zdate + 1) %>% 
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
                       lat = birdies$lat, radius=3, popup=paste0("<b/>", birdies$bird,"</b>", "<br>Date: ", firstbirdies$mo.da, "-", year(birdies$date), "<br> Time (24-hr format): ", birdies$hr.mn.sec), color="orangered")%>%
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

