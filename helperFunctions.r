#Helper functions to assist with heavy lifting of applicaiton
library(pracma)

#' Long/Lat Distance Calculator.
#' 
#' @param lat1 Latitude of location 1
#' @param long1 Longitude of location 1
#' @param lat2 Latitude of location 2
#' @param long2 Longitude of location 2
#'
#' @return Distance between the two locations in meters
#' @export
#'
#' @examples
distanceCalculator <- function(lat1, long1, lat2, long2){
  
  #Set earth radius constant
  EARTH_RADIUS <- 6371.01
  #Convert degrees to radians
  latitude1 <- deg2rad(lat1)
  longitude1 <- deg2rad(long1)
  latitude2 <- deg2rad(lat2)
  longitude2 <- deg2rad(long2)
  #Calculate distance using Great Circle Distance Equation * 1000 for meters
  distance <-  sin(latitude1) * sin(latitude2) + 
    cos(latitude1) * cos(latitude2) * 
    cos(longitude1 - longitude2) 
  
  distance <- EARTH_RADIUS  * acos(pmin(pmax(distance,-1.0), 1.0)) * 1000
  return(distance)
}


#' Get Maximum Distance
#'
#' @param dataFrame A data frame
#'
#' @return A list with the top distance, and the indexes for which
#' two rows of data account for that distance.
#' @export
#'
#' @examples
findMaxDistance <- function(dataFrame,name){
  datSubset <- dataFrame
  datSubset <- datSubset %>% filter(datSubset$SHIPNAME == name)
  maxDistStartRow <- 0
  maxDistEndRow <- 0
  maxDistTime <- 0
  maxDist <- 0
  maxDistDestination <- ""
  i <- 1
  j <- 2
  while(j < nrow(datSubset) + 1){
    
    dist <- distanceCalculator(datSubset[i,1],datSubset[i,2],datSubset[j,1],datSubset[j,2])
    if(datSubset$is_parked[i] != 1 ){
      print(dist)
      
    }

    if(is.na(dist) || is.nan(dist)){
      i <- i + 1
      j <- j + 1
      next;
    }
    if(datSubset$is_parked[i] == 1 || 
       is.na(datSubset$DESTINATION[i]) || 
       is.na(datSubset$DESTINATION[j])){
      i <- i + 1
      j <- j + 1
      next;
    }
    if(dist > maxDist && datSubset$DESTINATION[i] == datSubset$DESTINATION[j]){
      maxDist <- prettyNum(round(dist,2),big.mark=",",scientific=FALSE)
      maxDistStartRow <- c(datSubset[i,1],datSubset[i,2])
      maxDistEndRow <- c(datSubset[j,1],datSubset[j,2])
      maxDistDestination <- datSubset[i,6]
    }
    i <- i + 1
    j <- j + 1
  }
  allInfo <- list(maxDist,maxDistStartRow,maxDistEndRow,maxDistDestination)
  return(allInfo)
}
