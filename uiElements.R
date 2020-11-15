#Holds Modules for Dropdown Inputs

#' Vessel Type Input UI Element
#' 
#' @param vesselType The vessel type input ID
#' @param vesselData The vessel data set
#' @return A UI element
#' @export
#'
#' @examples
vesselTypeUI <- function(vesselType,vesselData){
  selectInput(inputId = vesselType, label = "Vessel Type", choices = unique(vesselData$ship_type), selected = "Cargo")
}

#' Vessel Name Input UI Element
#'
#' @param vesselName The vessel name input ID
#' @param vesselName The vessel data set
#' @return
#' @export
#'
#' @examples
vesselNameUI <- function(vesselName,vesselData){
  selectInput(inputId = vesselName, label = "Vessel Name", choices = unique(vesselData$SHIPNAME))
}

