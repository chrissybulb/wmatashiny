#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(leaflet)
library(httr)
library(shinydashboard)
library(DT)

########################## define variables and datsets

## define api key
myApiKey <- "place_your_API_key_here"

## call the api and define the set of Active buses - ActiveBusesResultDataset
ActiveBuses <- GET(paste0("https://api.wmata.com/Bus.svc/json/jBusPositions?RouteID=",
                          "&api_key=",
                          myApiKey))
ActiveBusesResults <- content(ActiveBuses)
ActiveBusesResultDataset <- do.call(rbind.data.frame,ActiveBusesResults$BusPositions) 

## call the api and define the set of Bus Routes - ActiveBusesResultDataset
BusRoutes <- GET(paste0("https://api.wmata.com/Bus.svc/json/jRoutes?",
                        "&api_key=",
                        myApiKey))
BusRoutesResults <- content(BusRoutes)
BusRoutesResultsDataset <- do.call(rbind.data.frame,BusRoutesResults$Routes)

## join up the Active Buses and Bus Routes data
BusRoutesResultsDatasetSummary <- data.frame(LineDesc = BusRoutesResultsDataset$LineDescription)
UniqueBusRoutesResultsDatasetSummary <- unique.data.frame(BusRoutesResultsDatasetSummary, incomparables = FALSE, fromLast = FALSE)
ActiveBusesandRouteInfo <- merge(BusRoutesResultsDataset,ActiveBusesResultDataset,by="RouteID")

## define the average deviation per route
AvgDeviationPerRoute <-aggregate(ActiveBusesandRouteInfo[, 7], list(RouteID=ActiveBusesandRouteInfo$RouteID), mean)
colnames(AvgDeviationPerRoute)[colnames(AvgDeviationPerRoute)=="x"] <- "AverageDeviation"
ActiveBusesandRouteInfo$Count <- NA
ActiveBusesandRouteInfo$Count <- 1
CountVehiclesPerRoute <-aggregate(ActiveBusesandRouteInfo[, 15], list(RouteID=ActiveBusesandRouteInfo$RouteID), sum)
AvgDeviationCountVehiclePerRoute <- merge(AvgDeviationPerRoute,CountVehiclesPerRoute,by="RouteID")
colnames(AvgDeviationCountVehiclePerRoute)[colnames(AvgDeviationCountVehiclePerRoute)=="x"] <- "CountofVehicles"

## define one dataset for Viz1 and one dataset for Viz2
BusResultDatasetViz1 <- subset(ActiveBusesResultDataset, DirectionText=="EAST" | DirectionText=="WEST" | DirectionText=="SOUTH" | DirectionText=="NORTH")
BusResultDatasetViz2 <- subset(ActiveBusesResultDataset, DirectionText=="EAST" | DirectionText=="WEST" | DirectionText=="SOUTH" | DirectionText=="NORTH" | DirectionText=="ANTICLKW" | DirectionText=="CLOCKWIS")

## define top 25 most delayed
Top25AvgDeviation <- top_n(AvgDeviationCountVehiclePerRoute, 25, AverageDeviation)
Top25AvgDeviation$CountofVehiclesGroup <- NA
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles == 1] <- "1"
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles == 2] <- "2"
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles == 3] <- "3"
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles == 4] <- "4"
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles == 5] <- "5"
Top25AvgDeviation$CountofVehiclesGroup[Top25AvgDeviation$CountofVehicles >= 6] <- "6+"

## define top 50 longest trips
ActiveBusesandRouteInfo$TrimStartTime <- NA
ActiveBusesandRouteInfo$TrimStartTime <- substr(ActiveBusesandRouteInfo$TripStartTime, 12, 19)

ActiveBusesandRouteInfo$TrimEndTime <- NA
ActiveBusesandRouteInfo$TrimEndTime <- substr(ActiveBusesandRouteInfo$TripEndTime, 12, 19)

ActiveBusesandRouteInfo$TrimStartTimeStampForm <- NA
ActiveBusesandRouteInfo$TrimStartTimeStampForm <- strptime(ActiveBusesandRouteInfo$TrimStartTime, "%H:%M:%S")

ActiveBusesandRouteInfo$TrimEndTimeStampForm <- NA
ActiveBusesandRouteInfo$TrimEndTimeStampForm <- strptime(ActiveBusesandRouteInfo$TrimEndTime, "%H:%M:%S")

ActiveBusesandRouteInfo$TripDuration <- NA
ActiveBusesandRouteInfo$TripDuration <- difftime(ActiveBusesandRouteInfo$TrimEndTimeStampForm, ActiveBusesandRouteInfo$TrimStartTimeStampForm, units = c("mins"))

ActiveBusesandRouteInfo$TripDuration <- as.numeric(ActiveBusesandRouteInfo$TripDuration)
Duration <- ActiveBusesandRouteInfo[, c(1:7,9,11,12,20)]
Top50Duration <- top_n(Duration, 50, TripDuration)

Top50Duration$Group <- NA
Top50Duration$Group[Top50Duration$Deviation < 0] <- "Early"
Top50Duration$Group[Top50Duration$Deviation == 0] <- "On Time"
Top50Duration$Group[Top50Duration$Deviation > 0 & Top50Duration$Deviation <= 10] <- "Up to 10 Minutes Late"
Top50Duration$Group[Top50Duration$Deviation >= 11 & Top50Duration$Deviation <= 19] <- "Between 10 and 20 Minutes Late"
Top50Duration$Group[Top50Duration$Deviation >= 20] <- "20+ Minutes Late"

Top50Duration$Group <- reorder(Top50Duration$Group, Top50Duration$Deviation)

########################## define the UI part

ui <- basicPage(
  
##  div(img(src='metrologo.png', height = 40, width = 30),
  
  titlePanel(title= "DC Metro Area Active Buses", 
             windowTitle = "DC Metro Area Active Buses"),
  
  
  navbarPage(

  tabsetPanel(
      tabPanel("Active Buses", 
               br(),
                    sidebarPanel(    
                     
                      radioButtons("directionInput", 
                                      "Direction:",
                               c("All","NORTH","SOUTH","EAST","WEST","CLOCKWIS","ANTICLKW"),
                              selected = "All")
        
                    
                    ),
               
               mainPanel(
                 br(),
                 plotOutput('tab1', width = "70%"),
                 br(),
                  "Use the map below to zoom in on specific areas:",
                 leafletOutput('tab1b', width = "100%"),
                 br(),br(),
                 "Trip Details:", br(),
                 DT::dataTableOutput('routes1')
               )),
       
          
      tabPanel("Deviation", 
               br(),
               sidebarPanel(    
                 sliderInput("deviationInput2", "Deviation", min = -20, max = 100,
                             value = c(-10, 15))
                 ),
               
               mainPanel(
                 br(),
               plotOutput('tab2', width = "70%"),
               br(), br(),
               plotOutput('tab3', width = "70%")
               )),
      
      
      tabPanel("Long Trips & Deviation",
                    br(),
                 sidebarPanel(
                      sliderInput("deviationInput4", "Deviation", min = -20, max = 100,
                                  value = c(-10, 15)),
                      
                      sliderInput("durationInput4", "Duration", min = -20, max = 100,
                                  value = c(0, 90))),
                      
      
                  mainPanel(
                      br(),
                      plotOutput('tab4a', width = "70%"),
                      br(),
                      plotOutput('tab4b', width = "70%")
                      )),
      
      tabPanel("References",
               br(),
               mainPanel(
                 br(),
                 "The data used in this Shiny App is called from the Washington Metropolitan Area Transit Authority's API that is available to developers here: https://developer.wmata.com")
               )
  )))
      


########################## define the server part

server <- function(input, output) {
 
   ########################### create tab 1 viz - Map of all active buses colored by direction
  
filter1a <- reactive({
if (input$directionInput == "All"){
filter1a1 <- BusResultDatasetViz2 
} else {
filter1a2 <- BusResultDatasetViz2 %>% 
filter(DirectionText == input$directionInput)
   }})
  
  
  ## filter1a <- reactive({
  ##   if (input$directionInput == "All"){
  ##     filter1 <- BusResultDatasetViz2
  ##   } else {
  ##     filtered <- BusResultDatasetViz2 %>% 
  ##       filter(DirectionText == input$directionInput)
  ##   }})
  
  ##  BusResultDatasetViz2 %>%
  ##    filter(
  ##      DirectionText == input$directionInput)
  ## })
  
   output$tab1 <- renderPlot({
       
     
     state_data <- data.frame(state=tolower(rownames(state.x77)),state.x77)
     county_map <- map_data("county")
     county_map
     metro_county <- subset(county_map, subregion=="arlington" & region=="virginia" | subregion=="montgomery" & region=="maryland" | subregion=="prince georges" | region=="district of columbia" | subregion=="fairfax" & region=="virginia")
     
     
     metro_county_map <-  geom_map(data=metro_county,
                                   map = metro_county,
                                   aes(long, lat, map_id = region),
                                   fill = NA, colour="darkgrey")
     
     ggplot(data = filter1a(), aes(x=Lon, y=Lat)) + metro_county_map  + 
       geom_point(aes(x=Lon, y=Lat, color = DirectionText), size = 1.5) + scale_shape_identity() + labs(color='Direction') + 
       labs(x = "", y= "", title = "Buses by Direction") +
       scale_color_manual(values=c("#BE223F", "#F3D335", "#DA8834", "#2994CF","#30AA4D","#BE223F")) +
       theme_bw() + theme(
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         plot.title = element_text(color="#366797", size=18, face="bold"),
         axis.title.x = element_text(color="#366797", size=12, face="bold"),
         axis.title.y = element_text(color="#366797", size=12, face="bold"))
     
  
      }, height = 400, width = 500)
  
   
   filter1b <- reactive({
     if (input$directionInput == "All"){
       filter1 <- BusResultDatasetViz2
     } else {
       filtered <- BusResultDatasetViz2 %>% 
         filter(DirectionText == input$directionInput)
     }})
   
   output$tab1b <- renderLeaflet({
 ##   filtered1 <-
 ##     BusResultDatasetViz2 %>%
## filter(
  ## BusResultDatasetViz2$DirectionText == input$directionInput)
     
leaflet(data = filter1b()) %>% 
addTiles() %>%
addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = 4, popup = ~RouteID, color = 'green')
     })
     
   
   output$routes1  = DT::renderDataTable({
     Routes1 <- filter1b()[, c(6,7,9,10)] 
    
   })
   
 
  
  ########################### create tab 2 viz - All Active Buses Colored by Direction
   
   filter2 <- reactive({
     BusResultDatasetViz1 %>%
       filter(
         Deviation >= input$deviationInput2[1],
         Deviation <= input$deviationInput2[2]
       )
   })
   
  output$tab2 <- renderPlot({
    BusesDirectionDeviationPlot <-ggplot(data = filter2(), aes(DirectionText, Deviation))
    BusesDirectionDeviationPlot + 
      geom_violin(fill='#2994CF') + scale_shape_identity() + ggtitle("Bus Direction and Frequency of Deviation from Schedule") + scale_y_continuous("Deviation from Schedule (minutes)") + scale_x_discrete("Direction of Bus") +
      theme_bw() + theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color="#366797", size=18, face="bold"),
        axis.title.x = element_text(color="#366797", size=12, face="bold"),
        axis.title.y = element_text(color="#366797", size=12, face="bold")
      )
  }, height = 400, width = 600)
  
  
  ########################### create tab 3 viz - 25 Most Delayed Routes and Counts of Buses in those Routes
  
  filter3 <- reactive({
    Top25AvgDeviation %>%
      filter(
        AverageDeviation >= input$deviationInput2[1],
        AverageDeviation <= input$deviationInput2[2]
      )
  })
  
  output$tab3 <- renderPlot({
    AverageDeviation <-ggplot(data = filter3(), aes(x= reorder(RouteID,-AverageDeviation), y = AverageDeviation, color = CountofVehiclesGroup))
    AverageDeviation + geom_point(size = 3) + 
      labs(x = "RouteID", y= "Average Delay (minutes)", title = "25 Most Delayed Routes and Counts of Buses in those Routes") +
      labs(color='Active Buses') + 
      scale_color_manual(values=c("#BE223F", "#2994CF", "#DA8834", "#F3D335","#30AA4D","#A1A3A1")) +
      theme_bw() + theme(
        axis.text.x=element_text(color="#366797", size=7),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(color="#366797", size=10),
        axis.ticks.y=element_blank(),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color="#366797", size=18, face="bold"),
        axis.title.x = element_text(color="#366797", size=12, face="bold"),
        axis.title.y = element_text(color="#366797", size=12, face="bold"))
    
  }, height = 400, width = 600)
  
  ########################### create tab 4 viz - 50 Longest Trips and Deviation from Schedule in minutes
  
  
  filter4 <- reactive({
    Top50Duration %>%
      filter(
        Deviation >= input$deviationInput4[1],
        Deviation <= input$deviationInput4[2],
        TripDuration >= input$durationInput4[1],
        TripDuration <= input$durationInput4[2]
      )
   })
  
  output$tab4a <- renderPlot({
    
    state_data <- data.frame(state=tolower(rownames(state.x77)),state.x77)
    county_map <- map_data("county")
    county_map
    metro_county <- subset(county_map, subregion=="arlington" & region=="virginia" | subregion=="montgomery" & region=="maryland" | subregion=="prince georges" | region=="district of columbia" | subregion=="fairfax" & region=="virginia")
    
    
    metro_county_map <-  geom_map(data=metro_county,
                                  map = metro_county,
                                  aes(long, lat, map_id = region),
                                  fill = NA, colour="darkgrey")
    
    ggplot(data = filter4(), aes(x=Lon, y=Lat)) + metro_county_map  + 
      geom_point(aes(x=Lon, y=Lat, color = Group), size = 2.5) + scale_shape_identity() +
      labs(x = "", y= "", title = "50 Longest Trips' Deviation from Schedule") +
      scale_color_manual(values=c("#2994CF", "#30AA4D", "#F3D335", "#DA8834","#BE223F","#A1A3A1")) +
      labs(color='Deviation') + 
      theme_bw() + theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color="#366797", size=18, face="bold"),
        axis.title.x = element_text(color="#366797", size=12, face="bold"),
        axis.title.y = element_text(color="#366797", size=12, face="bold"))
  }, height = 400, width = 500)
        

  
  ########################### create viz 4b - Longest Routes and Deviation
  output$tab4b <- renderPlot({
    Top50Duration$Group <- reorder(Top50Duration$Group, Top50Duration$Deviation)
    LongRoutesDeviation <- ggplot(data=filter4(), aes(x=RouteID, y=TripDuration, color = Group)) +
      geom_point(aes(x=RouteID, y=TripDuration), size = 2.5) +
      scale_color_manual(values=c("#2994CF", "#30AA4D", "#F3D335", "#DA8834","#BE223F","#A1A3A1")) +
      labs(x = "RouteID", y= "Duration", color = "Deviation", title = "Deviation of Trips in the Longest Routes") +
      theme_bw() + theme(
        axis.text.x=element_text(color="#366797", size=7),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(color="#366797", size=10),
        axis.ticks.y=element_blank(),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color="#366797", size=18, face="bold"),
        axis.title.x = element_text(color="#366797", size=12, face="bold"),
        axis.title.y = element_text(color="#366797", size=12, face="bold"))
    LongRoutesDeviation
  }, height = 400, width = 500)
  

  
 
}

shinyApp(ui, server)