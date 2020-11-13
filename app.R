#Appsilon Recruitment Task Dashboard
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
library(dplyr)
library(shinycustomloader)
library(formattable)
library(tidyr)
library(waiter)
library(corazon)
library(magrittr)
library(highlighter) # devtools::install_github("Appsilon/highlighter")
library(formatR)
library(httr)
library(rjson)
library(leaflet)
library(geosphere)
source("helperFunctions.R")

vesselData <- read.csv("sample.csv")
# vesselData <- head(vesselData,200)
#setwd("/Users/andrewmcmanus/Documents/Programming/JavaScript/Jonas JavaScript Course /6-budgety/starter/Appsilon")
#vesselsFil <- distinct(vesselData,ship_type,.keep_all = T)
#VESSEL_TYPES <- c(1,2,3,4)#vesselsFil$ship_type
VESSEL_NAMES <- c(1,2,3,4)#distinct(vesselsFil, SHIPNAME,.keep_all=F)
#####################
#######THE UI########    
#####################
ui <- semanticPage(
  htmlOutput(outputId = "back"),
  div(align = "center",style = "height: 98%; width: 100%;background-color:rgba(255,255,255,0.9); border-radius: 15px;",
    div(align = "left", style = "background-color: #d9e5ff ; margin-bottom: 10px; border-radius: 10px 10px 0px 0px",
        tags$img(src = "logos/logo-appsilon.png", height = "10%", width = "10%")
    ),
    div(align = "center", style = "padding-top: 10px; padding-bottom: 0px;",
      div(style = "display: inline-block;vertical-align:top; width: 350px; padding-right: 100px;",
          selectInput(inputId = "vesselType", label = "Vessel Type", choices = unique(vesselData$ship_type), selected = "Tug")
      ),
      div(style = "display: inline-block;vertical-align:top; width: 350px; padding-left: 100px;",
          selectInput(inputId = "vesselName", label = "Vessel Name", choices = unique(vesselData$SHIPNAME))
      )
    ),
    div(style = "width:100%; padding-top:10px; height: 600px; margin: 0px",
      div(class = "ui raised segment", style = "margin-right: 50px; display: inline-block; vertical-align:top; width: 20%; border-radius: 10px; margin-top: 15px",
              withSpinner(highchartOutput(outputId = "metrics",height = 480, width = "100%")),
              sliderInput(inputId = "test",label="Test",min = 1, max = 100, value = 50)
      ),
      div(class = "ui raised segment", style = "margin-left: 50px; display: inline-block; vertical-align:top; width: 70%; border-radius: 10px;",
              withSpinner(leaflet::leafletOutput(outputId = "vesselMap", height = 550, width = "100%"))
      )
    ),
    div(style = "width:100%; padding-top:15px;",
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
                h1("Distance travelled: ",style = 'font-family: Arial Black; font-size: 25px'),
                htmlOutput(outputId = "vesselDistance")
            )
        )
    )
  )              
)



####################
#######SERVER#######
####################
server <- function(input, output,session) {  
  
  output$back <- renderUI({
    setBackgroundImage(src ="cool-background.png")
  })
  
  observe({
    x <- vesselData %>% filter(ship_type == input$vesselType)
    print("---")
    print(x)
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "vesselName",
                      label = "Vessel Name",
                      choices = x$SHIPNAME,
                      selected = head(x$SHIPNAME, 1)
    )
  })
  
  output$vesselMap <- renderLeaflet({
   
    listOfInfo <- findMaxDistance(vesselData,input$vesselName)
    print("HERE")
    startingRecord <- listOfInfo[[2]]
    endingRecord <- listOfInfo[[3]]
    
    vesselData$X <- NULL
    # point1 <- data.frame(vesselData[startingRecord,1],vesselData[startingRecord,2])
    # point2 <- data.frame(vesselData[endingRecord,1],vesselData[endingRecord,2])
    point1 <- data.frame(vesselData[1,1],vesselData[1,2])
    point2 <- data.frame(vesselData[2,1],vesselData[2,2])
    baseLon <- mean(point1[1,1], point2[1,1])
    baseLat <- mean(point1[1,2], point2[1,2])
    
    gcIntermediate(point1, point2, 200, 
                   breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE) %>%
      leaflet() %>% addTiles() %>% addPolylines() %>% 
      setView(lng = baseLon, lat = baseLat, zoom = 3)
    
      # addCircles(data = l,
      #            lng= ~Longitude, lat = ~Latitude, color = pal(l$Division),
      #            radius = ~Years.of.Experience,
      #            popup = paste("City:",l$City,"<br>",
      #                          "Division:",l$Division,"<br>",
      #                          "Years of Experience",l$Years.of.Experience)
      # ) 
  })
  
  output$flagImg <- renderUI({
    flagData <- vesselData %>% filter(SHIPNAME == input$vesselName) 
    flag <- flagData$FLAG[1]
    tags$img(src = paste0("country_flags/",flag,".jpg"), height = "30%", width = "30%")
  })
  
  output$selectedVessel <- renderUI({
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 20px;'>",
                        input$vesselName,"</div>"))
  })
  
  output$selectedVesselID <- renderUI({
    idData <- vesselData %>% filter(SHIPNAME == input$vesselName) 
    id <- idData$SHIP_ID[1]
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 13px; padding-top: 5px;'>","ID: ",
                        id,"</div>"))
  })
  
  output$selectedVesselType <- renderUI({
    type <- toupper(input$vesselType)
    text <- HTML(paste0("<div style = 'font-family: Arial; font-size: 20px'>",
                        type,"</div>"))
  })
  
  output$vesselDistance <- renderUI({
    
    listOfInfo <- findMaxDistance(vesselData,input$vesselName)
    distance <- round(listOfInfo[[1]],2)
    
    
    HTML(paste0("<div style = 'font-family: Arial; font-size: 20px'>",distance,
                "M","</div>"))
  })
  
  output$metrics <- renderHighchart({
    
    test1 <- c(10,40,50,20,30)
    test2 <- c("October","November","December","January","February")
    dat <- data.frame(test1,test2)
    
    highchart() %>%
      hc_add_series(data = dat, "line", color = "#4287f5", hcaes(x = test2, y = test1), 
                    name = "Series Name") %>%
      hc_plotOptions(
        series = list(
          showInLegend = T,
          pointFormat = "{point.y}%"
        ), column = list(colorByPoint = T)
      ) %>%
      hc_yAxis(title = list(text = "Percent"),
               #lables = list(format("{value}%"),
               max = 70) %>%
      hc_xAxis(title = list(text = "Week"), categories = dat$test2) %>%
      hc_title(text = "") %>%
      hc_tooltip(pointFormat = "{point.y}%")
    
  })
  
}



shinyApp(ui, server)


