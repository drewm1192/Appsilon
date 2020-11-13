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
  div(align = "center",style = "padding:20px; height: 100%; width: 100%; background-color:rgba(255,255,255,0.9); border-radius: 10px",
    fluidRow(align = "center", style = "padding-top: 20px; padding-bottom: 25px;",
      div(style = "display: inline-block;vertical-align:top; width: 350px; padding-right: 100px;",
          selectInput(inputId = "vesselType", label = "Vessel Type", choices = unique(vesselData$ship_type), selected = "Tug")
      ),
      div(style = "display: inline-block;vertical-align:top; width: 350px; padding-left: 100px;",
          selectInput(inputId = "vesselName", label = "Vessel Name", choices = unique(vesselData$SHIPNAME))
      )
    ),
    fluidRow(align= "center",
            withSpinner(leaflet::leafletOutput(outputId = "vesselMap", height = 600, width = "90%"))
    ),
    div(style = "width:100%; padding-top:70px;",
        div(class = "ui centered blue card", style = "margin-left: 60px; margin-right: 60px; width: 400px; padding-top: 20px; display: inline-block; vertical-align:top;",
            div(class = "content",
                htmlOutput(outputId = "selectedVessel"),
                br(),
                uiOutput(outputId = "flagImg")
            )
        ),
        div(class = "ui centered blue card", style = "margin-left: 60px; margin-right: 60px; width: 400px; display: inline-block; vertical-align:top;",
            div(class = "content",
                h2("Cargo"),
                br(),
                br(),
                p("This is example test to see if things are working"),
            )
        ),
        div(class = "ui centered blue card", style = "margin-left: 60px; margin-right: 60px; width: 400px; display: inline-block; vertical-align:top;",
            div(class = "content",
                h2("Distance Sailed"),
                br(),
                br(),
                p("This is example test to see if things are working"),
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
  
  # observe({
  #   # get all character or factor columns
  #   filtered <- vesselData %>% filter(ship_type == input$Type)
  #   print(filtered)
  #   options <-  distinct(filtered,SHIPNAME,.keep_all = T)
  #   updateSelect(session, "vesselName",
  #                choices = options, # update choices
  #                selected = NULL) # remove selection
  # })
  # observeEvent(
  #   input$vesselType,
  #   updateSelectInput(session,"vesselName","Vessel Name",
  #                     choices = unique(vesselData$SHIPNAME[vesselData$ship_type] == input$vesselType))
  # )
  
  output$vesselMap <- renderLeaflet({
    
    # pal <-colorFactor(
    #   palette = c('red', 'blue', 'green', 'pink'),
    #   domain = l$Division
    # )
    
    
    
    
    leaflet() %>%
      addTiles() %>% #addTiles puts the actualy map graphic down. You can get tiles from Google Maps and more, but Openstreet map is the default
      setView(lng = -98.877098, lat = 40.311139, zoom = 4)# %>% # Add default OpenStreetMap map tiles
      # addCircles(data = l,
      #            lng= ~Longitude, lat = ~Latitude, color = pal(l$Division),
      #            radius = ~Years.of.Experience,
      #            popup = paste("City:",l$City,"<br>",
      #                          "Division:",l$Division,"<br>",
      #                          "Years of Experience",l$Years.of.Experience)
      # ) %>%
      # leaflet::addLegend("bottomright", pal = pal, values = map.data$Division)
  })
  
  output$flagImg <- renderUI({
    flagData <- vesselData %>% filter(SHIPNAME == input$vesselName) 
    flag <- flagData$FLAG[1]
    tags$img(src = paste0("country_flags/",flag,".jpg"), height = "40%", width = "40%")
  })
  
  output$selectedVessel <- renderUI({
    
    text <- HTML(paste0("<div style = 'font-family: Arial Black; font-size: 20px'>",
                        input$vesselName,"</div>"))
    
  })
  
}



shinyApp(ui, server)


