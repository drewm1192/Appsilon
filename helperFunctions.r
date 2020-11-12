#Helper functions to assist with heavy lifting of applicaiton


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
  distance <- EARTH_RADIUS * acos(sin(latitude1) * sin(latitude2) + 
                                    cos(latitude1) * cos(latitude2) * 
                                    cos(longitude1 - longitude2)) * 1000
  return(distance)
}


dat <- read.csv(file.choose())
nrow(dat)
datSubset <- dat %>% filter(SHIPNAME == "KAROLI")

distanceCalculator(datSubset[1,1],datSubset[1,2],datSubset[2,1],datSubset[2,2])



#' Get Maximum Distance
#'
#' @param dataFrame A data frame
#'
#' @return A list with the top distance, and the indexes for which
#' two rows of data account for that distance.
#' @export
#'
#' @examples
findMaxDistance <- function(dataFrame){
  
  maxDistStartRow <- 0
  maxDistEndRow <- 0
  maxDistTime <- 0
  maxDist <- 0
  
  i <- 1
  j <- 2
  while(j < nrow(datSubset) + 1){
    
    dist <- distanceCalculator(datSubset[i,1],datSubset[i,2],datSubset[j,1],datSubset[j,2])
    if(is.nan(dist)){
      i <- i + 1
      j <- j + 1
      next;
    }
    if(dist >= maxDist){
       maxDist <- dist
       maxDistStartRow <- i
       maxDistEndRow <- j
    }
    i <- i + 1
    j <- j + 1
  }
  allInfo <- list(maxDist,maxDistStartRow,maxDistEndRow)
  return(allInfo)
}






