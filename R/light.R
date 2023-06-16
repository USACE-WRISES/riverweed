#' Calculate photosynthetically active radiation
#'
#' \code{light} calculates photosynthetically active radiation (PAR) at the water surface 
#' at three times of day based on latitude and Julian day
#' 
#' @param day Julian day
#' @param lat latitude in decimal degrees
#'
#' @return tmpvec a vector of three PAR values in microEinsteins
#'  
#' @details 
#' Given Julian day and latitude, this function calculates photosynthetically active radiation (PAR) in microEinsteins at the water surface following GenVeg (CITE, plus their sources).  This function returns a vector of PAR values corresponding to three times of day, roughly midday, morning/afternoon, and dawn/dusk.
#' 
#' 
#' 

#' 
#' @references
#' GenVeg
#' Astro?
#' 
#' @examples
#' #Result: Vector of c(2879.4155, 1882.2661, 382.6393) microEinsteins of PAR
#' light(120, 25)
#' 
#' #Result: Vector of c(2940.5729, 1942.6421, 403.0639) microEinsteins of PAR
#' light(250, 5)
#' 
#' @export

light <- function(day, lat){
  
  tmpvec <- c()
  degree.to.rad <- pi/180 #LD addition
  xgauss <- c(0.1127, 0.5, 0.8873) #LD addition, from xgauss in 20220816 GenVeg code - check with Todd. Confirmed.
  declination <- (-asin ((sin(23.45 * degree.to.rad)) * (cos(2 * pi * (day + 10) / 365))))
  
  #intermediate variables
  sinld <- ((sin(lat * degree.to.rad)) * (sin(declination))) #radians
  cosld <- cos(lat * degree.to.rad) * cos(declination) #radians
  
  aob <- (sinld / cosld)  #radians
  
  temp1 <- asin(aob)
  
  daylength <- 12 * (1 + 2 * temp1 / pi) #calculates daylength based on declination and latitude
  
  
  dsinB <- 3600 * (daylength * sinld + 24 * cosld * sqrt(1 - aob * aob) / pi)
  dsinBE <- 3600 * (daylength *(sinld + 0.4 * (sinld * sinld + cosld * cosld * 0.5)) + 12 * cosld * (2 + 3 * 0.4 * sinld) * sqrt (1 - aob * aob) / pi)  
  
  sc <- 1370 * (1 + 0.033 * cos(2 * pi * day / 365)) #Solar constant
  
  
  dso <- sc * dsinB     #Daily solar radiation
  
  
  for(hr in 1:3){
    
    hour1 <- 12 + (daylength * 0.5 * xgauss[hr]) #calculates hour in which photosynthesis is applied #LD: does this make sense? Seems to start everything at noon.
    #print (paste("hour1r: ", hour1)) 
    
    sinb.tmp <-  sinld + cosld * cos(2 * pi * (hour1 + 12) / 24)
    #print (paste("sinb.tmp", sinb.tmp))  
    #sinb.tmp <-c(o,sinb.tmp1)
    
    sinB <- max(c(0,sinb.tmp)) #calculates sin of solar elevation, max functions prevents values less than 0
    #print (paste("sinB", sinB))
    
    PAR1 <- 0.5 * dso * sinB * (1 + 0.4 * sinB) / dsinBE #NB: dso can be replaced with values from the FAO chart
    #print (paste("PAR1", PAR1))
    
    PAR1 <- PAR1 * (868/208.32) #Convert to correct units, which are microEinsteins
    #print (paste("hr: ", hr, " PAR1: ", PAR1, " tmpvec ", tmpvec))
    
    tmpvec[hr] <- PAR1 #output of function is vector of 3 values that represents time of day
    #print (tmpvec)
    
  }
  
  return(tmpvec) #returns a vector of PAR values in MicroEinsteins
  
} #end fxn call

