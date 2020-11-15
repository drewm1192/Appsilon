#Marine Track Dashboard
library(shiny.semantic)
library(shinyBS)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)
library(dplyr)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(pracma)
library(dplyr)
library(shinycustomloader)
library(formattable)
library(tidyr)
library(waiter)
library(corazon)
library(magrittr)
library(highlighter) 
library(formatR)
library(httr)
library(rjson)
library(leaflet)
library(geosphere)
source("helperFunctions.R")
source("uiElements.R")
vesselData <- read.csv("ships.csv")

#####################
#######THE UI########    
#####################
ui <- semanticPage(
  htmlOutput(outputId = "back"),
  tags$style("body {
                    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
                    zoom: 0.9; /* Other non-webkit browsers */
                    zoom: 90%; /* Webkit browsers */
                    }"),
  div(align = "center",style = "height: 100%; width: 100%;background-color:rgba(255,255,255,0.9); border-radius: 15px;",
      div(align = "left", style = "background-color: #d9e5ff ; margin-bottom: 10px; border-radius: 10px 10px 0px 0px",
          div(style = "text-align: left; padding-left: 15px; width: 20%; padding-top: 10px;",
            h2("Marine Track", style = "color: #72a2b8;font-family: Arial; font-weight: 10; font-size: 34px")
          )
      ),
      div(align = "center", style = "padding-top: 10px; padding-bottom: 0px;",
          div(style = "display: inline-block;vertical-align:top; width: 350px; padding-right: 100px;",
              vesselTypeUI("vesselType",vesselData)
          ),
          div(style = "display: inline-block;vertical-align:top; width: 350px; padding-left: 100px;",
              vesselNameUI("vesselName",vesselData)
          )
      ),
      div(style = "width:100%; padding-top:10px; height: 600px; margin: 0px",
          div(class = "ui raised segment", style = "margin-left: 50px; width: 80%; border-radius: 10px;",
              withSpinner(leaflet::leafletOutput(outputId = "vesselMap", height = 550, width = "100%")),
              htmlOutput(outputId = "wasParked")
          )
      ),
      div(style = "width:100%; padding-top:10px;padding-left:50px",
          div(class = "ui raised segment", style = "margin-top: 12px; margin-left: 60px; margin-right: 60px; width: 400px; height: 220px; padding-top: 20px; display: inline-block; vertical-align:top;",
              div(class = "content",
                  uiOutput(outputId = "flagImg"),
                  h1("Vessel Name: ",style = 'font-family: Arial Black; font-size: 25px; padding-top: -30'),
                  htmlOutput(outputId = "selectedVessel"),
                  htmlOutput(outputId = "selectedVesselID")
              )
          ),
          div(class = "ui raised segment", style = "margin-left: 60px; margin-right: 60px; width: 400px; height: 220px; display: inline-block; vertical-align:top;",
              div(class = "content",
                  tags$img(src = "icons/Ship-icon.png", height = "25%",width = "25%"),
                  h1("Vessel Type: ",style = 'font-family: Arial Black; font-size: 25px'),
                  htmlOutput(outputId = "selectedVesselType")
              )
          ),
          div(class = "ui raised segment", style = "margin-left: 60px; margin-right: 60px; width: 400px; height: 220px; display: inline-block; vertical-align:top;",
              div(class = "content",
                  tags$img(src = "icons/distance.png", height = "25%",width = "25%"),
                  h1("Distance Travelled: ",style = 'font-family: Arial Black; font-size: 25px'),
                  htmlOutput(outputId = "vesselDistance")
              )
          ),
          div(style = "align:left;",
              tags$img(src = "logos/logo-appsilon.png", height = "5%", width = "5%")
          ),
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          )
      )
  )              
)


####################
#######SERVER#######
####################
server <- function(input, output,session) {  
  
  #Set a dark blue gradient background
  output$back <- renderUI({
    setBackgroundImage(src ="cool-background.png")
  })
  
  #Handle updating the vessel name dropdown so it only shows 
  #names of vessels of the selected type
  observe({
    x <- vesselData %>% filter(ship_type == input$vesselType)
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "vesselName",
                      label = "Vessel Name",
                      choices = unique(x$SHIPNAME),
                      selected = head(x$SHIPNAME, 1)
    )
  })
  
  #Create the map
  output$vesselMap <- renderLeaflet({
    #Get info on the max distance observations
    listOfInfo <- findMaxDistance(vesselData,input$vesselName)
    #Save out the values from findMaxDistance to readable variable names
    maxDistance <- listOfInfo[[1]]
    startingRecord <- listOfInfo[[2]]
    endingRecord <- listOfInfo[[3]]
    destinationPort <- listOfInfo[[4]]
    if(is.na(destinationPort) || destinationPort == ""){
      destinationPort <- "Not Provided"
    }
    #Only display map if distance is greater than 0
    if(maxDistance != 0){
      #Put long/lat in format required for visual
      lat1 <- c(startingRecord[2])
      lon1 <- c(startingRecord[1])
      lat2 <- c(endingRecord[2])
      lon2 <- c(endingRecord[1])
      point1 <- data.frame(lat1,lon1)
      point2 <- data.frame(lat2,lon2)
      
      #Generate a setView location based on the starting and ending points of journey
      baseLon <- mean(point1[1,1], point2[1,1])
      baseLat <- mean(point1[1,2], point2[1,2])
      #Create the visual with a distane line
      gcIntermediate(point1, point2, 200, 
                     breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE) %>%
        leaflet() %>% addTiles() %>% addPolylines(popup = paste0("<div style = 'text-align: center; font-size: 15px;'>","<strong>","Ship Info:","</strong>","</div>","<br>",
                                                                 "<strong>","Ship Name: ","</strong>",input$vesselName, "<br>",
                                                                 "<strong>","Distance Travelled: ","</strong>", maxDistance," M", "<br>",
                                                                 "<strong>","Destination Port: ","</strong>",destinationPort
        )) %>% 
        setView(lng = baseLon, lat = baseLat, zoom = 11)
    }
  })
  
  #Renders an image of the ship's flag.
  output$flagImg <- renderUI({
    flagData <- vesselData %>% filter(SHIPNAME == input$vesselName) 
    flag <- flagData$FLAG[1]
    tags$img(src = paste0("country_flags/",flag,".jpg"), height = "30%", width = "30%")
  })
  
  #Generates the selected vessel's name in the bottom left box
  output$selectedVessel <- renderUI({
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 20px;'>",
                        input$vesselName,"</div>"))
  })
  
  #Generates the vessel ID in the bottom left box
  output$selectedVesselID <- renderUI({
    idData <- vesselData %>% filter(SHIPNAME == input$vesselName) 
    id <- idData$SHIP_ID[1]
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 13px; padding-top: 5px;'>","ID: ",
                        id,"</div>"))
  })
  
  #Generates the vessel type text in the bottom center box
  output$selectedVesselType <- renderUI({
    type <- toupper(input$vesselType)
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 20px'>",
                        type,"</div>"))
  })
  
  output$vesselDistance <- renderUI({
    listOfInfo <- findMaxDistance(vesselData,input$vesselName)
    distance <- listOfInfo[[1]]
    HTML(paste0("<div style = 'font-family: Arial; font-size: 20px'>",distance,
                "M","</div>"))
  })
  
  #Displays message if there are no records for the vessel where it was not parked
  #i.e. is_parked is never 0.
  output$wasParked <- renderUI({
    isParkedData <- vesselData %>% filter(SHIPNAME == input$vesselName) %>% filter(is_parked == 0)
    activeRecords <- nrow(isParkedData)
    if(activeRecords == 0){
      text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 20px;'>",
                          "No active records for this vessel. This vessel was parked for the during of the observation period.","</div>"))
    }
    else{
      text <- ""
    }
    text
  })
  
}



shinyApp(ui, server)

