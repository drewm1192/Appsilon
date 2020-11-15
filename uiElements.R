#Holds Modules for Dropdown Inputs

#' Vessel Type Input UI Element
#' 
#' @param vesselType The vessel type input ID
#'
#' @return A UI element
#' @export
#'
#' @examples
vesselTypeUI <- function(vesselType){
  selectInput(inputId = vesselType, label = "Vessel Type", choices = unique(vesselData$ship_type), selected = "Tug")
}

#' Vessel Name Input UI Element
#'
#' @param vesselName The vessel name input ID
#'
#' @return
#' @export
#'
#' @examples
vesselNameUI <- function(vesselName){
  selectInput(inputId = vesselName, label = "Vessel Name", choices = unique(vesselData$SHIPNAME))
}

