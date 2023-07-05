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
#' #Result: day length of 12.90703 hours
#' daylength(120,25)
#' 
#' #Result: day length of 11.74682 hours
#' daylength(200,-5)
#' 
#' @export

daylength <- function(day, lat){
  #Error handling for logical inputs
  if(is.logical(day) || is.logical(lat)){stop("Invalid input")}
  #Error handling for nonexistent latitudes
  if(abs(lat)>90){stop("Latitude must be between -90 and 90 degrees")}
  
  degree.to.rad <- pi/180 #LD addition
  xgauss <- c(0.1127, 0.5, 0.8873) #LD addition, from xgauss in 20220816 GenVeg code - check with Todd
  declination <- (-asin ((sin(23.45 * degree.to.rad)) * (cos(2 * pi * (day + 10) / 365))))
  
  #intermediate variables
  sinld <- ((sin(lat * degree.to.rad)) * (sin(declination))) #radians
  cosld <- cos(lat * degree.to.rad) * cos(declination) #radians
  
  aob <- (sinld / cosld)  #radians
  
  temp1 <- asin(aob)
  
  daylength <- 12 * (1 + 2 * temp1 / pi) #calculates daylength based on declination and latitude
  
  if((day %in% seq(1,365))==FALSE){warning("day is not a whole number between 1 and 365")} #produces a warning for values of day outside of 1:365.
  
  return(daylength) #returns day length in hours
} #end fxn call

