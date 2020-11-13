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
library(leaflet)


#####################
#######THE UI########    
#####################
ui <- semanticPage(
  fluidRow(align = "center", style = "padding-top: 100px; padding-bottom: 25px;",
    div(style = "display: inline-block;vertical-align:top; width: 350px; padding-right: 100px;",
        dropdown_input("vesselType", LETTERS, value = "A")
    ),
    div(style = "display: inline-block;vertical-align:top; width: 350px; padding-left: 100px;",
        dropdown_input("vesselName", LETTERS, value = "A")
    )
  ),
  fluidRow(align= "center",
          withSpinner(leaflet::leafletOutput(outputId = "vesselMap", height = 600, width = "90%"))
  )
                 
)



####################
#######SERVER#######
####################
server <- function(input, output,session) {  
  
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
  
  
}



shinyApp(ui, server)


