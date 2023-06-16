#' Calculate day length
#'
#' \code{daylength} calculates day length in hours based on latitude and Julian day
#' 
#' @param day Julian day
#' @param lat latitude in decimal degrees
#'
#' @return daylength, the number of hours between sunrise and sunset.
#'  
#' @details 
#' Given Julian day and latitude, this function calculates day length in hours following GenVeg (CITE, plus their sources).
#' 
#' 
#' 
#' 
#' @references
#' GenVeg
#' Astro?
#' 
#' @examples
#' #Result: Stem length of 20.63898 centimeters
#' biomass.to.stemlength.pod(0.1, type=1)
#' 
#' #Result: Stem length of 43.11088 centimeters
#' biomass.to.stemlength.pod(0.1, type=2)
#' 
#' #Result: Stem length of 14.82514 centimeters
#' biomass.to.stemlength.pod(0.1, type=3)
#' 
#' #Result: Error message indicating incorrect model specification
#' biomass.to.stemlength.pod(0.1, type=7)
#' 
#' #Result: Warning message indicating unrealistic biomass
#' #biomass.to.stemlength.pod(1, type=1)
#' 
#' @export

daylength <- function(day, lat){
  
  degree.to.rad <- pi/180 #LD addition
  xgauss <- c(0.1127, 0.5, 0.8873) #LD addition, from xgauss in 20220816 GenVeg code - check with Todd
  declination <- (-asin ((sin(23.45 * degree.to.rad)) * (cos(2 * pi * (day + 10) / 365))))
  
  #intermediate variables
  sinld <- ((sin(lat * degree.to.rad)) * (sin(declination))) #radians
  cosld <- cos(lat * degree.to.rad) * cos(declination) #radians
  
  aob <- (sinld / cosld)  #radians
  
  temp1 <- asin(aob)
  
  daylength <- 12 * (1 + 2 * temp1 / pi) #calculates daylength based on declination and latitude
  
  return(daylength) #returns day length in hours
} #end fxn call
