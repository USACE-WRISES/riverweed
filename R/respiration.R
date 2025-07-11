#' Calculate maintenance respiration
#'
#' \code{respiration} calculates the amount of biomass lost by a macrophyte on a given day due to maintenance respiration.
#' 
#' @param kmprime base maintenance respiration coefficient at 25 °C. GenVeg, citing Teh (2006) table 7.1, give a value of 0.03 for leaves and 0.015 for stems and roots.  GenVeg's NetLogo code calls this figure q10 instead, treats stems and leaves together, and uses the average value of 0.0225.
#' @param temp average daily temperature in °C.
#' @param liveweight live biomass of macrophyte or its relevant organ or tissue.
#' @param totalweight total (live plus dead) biomass of macrophyte or its relevant organ or tissue.
#' @param glucosereq glucose requirement for growth. GenVeg, citing Teh (2006) Table 7.4, give values of 1.436 for leaves, 1.513 for stems, and 1.444 for roots.
#'
#' @return daily biomass loss due to maintenance respiration, a positive number.
#'  
#' @details 
#' This function calculates macrophyte biomass losses due to maintenance respiration, and is intended to be used alongside the photosynthesis function to convert gross photosynthetic assimilation to net photosynthetic assimilation.  This function applies to a single plant organ or tissue, but can be looped over different organs or tissues with different metabolic properties.  Positive values of respiration represent biomass lost to respiration, so net photosynthesis = gross photosynthesis - respiration.  
#' Inclusion of standing dead biomass via the totalweight argument is carried over from GenVeg, where standing dead biomass was used as a proxy of plant age, and maintenance respiration assumed to decrease in older plants.  The effects of standing dead biomass in other circumstances should be considered with caution.
#' 
#' 
#' @references
#' GenVeg
#' Teh, Christopher B. S. 2006. Introduction to mathematical modeling of crop growth: How the equations are derived and assembled into a computer model. Boca Raton, FL: BrownWalker Press.
#' 
#' 
#' @examples
#' #Result: macrophyte biomass loss of 0.015 g
#' respiration(kmprime=0.0225, temp=15, liveweight=2, totalweight=2, glucosereq=1.5)
#' 
#' #Result: macrophyte biomass loss of 0.01 g
#' respiration(kmprime=0.0225, temp=15, liveweight=2, totalweight=3, glucosereq=1.5)
#' 
#' #Result: macrophyte biomass loss of 0.02222222 g
#' respiration(kmprime=0.05, temp=15, liveweight=2, totalweight=3, glucosereq=1.5)
#' 
#' @export
respiration <- function(kmprime, temp, liveweight, totalweight, glucosereq){
  
  #Error handling for invalid inputs
  if(totalweight < liveweight){stop("totalweight must be greater than or equal to liveweight")}
  if(totalweight < 0 | liveweight < 0){stop("Biomass cannot be negative")}
  if(glucosereq < 0){stop("Glucose requirement cannot be negative")}
  if(kmprime < 0){stop("kmprime cannot be negative")}
  if(temp < -90 || temp > 60){warning("Specified temperature has not been recorded in nature on Earth")}
  if(is.logical(kmprime) || is.logical(temp) || is.logical(liveweight) || is.logical(totalweight) || is.logical(glucosereq)){stop("Invalid input")}
  
  else{
  km <- kmprime*2^((temp-25)/10) #Respiration coefficient for a generic tissue after GenVeg, who cite temperature dependence from Teh (2006) p. 134.
  rmprime <- km*liveweight #Maintenance respiration per day after GenVeg, citing Teh (2006)
  proplive <- liveweight/totalweight #Proportion of tissue biomass that is alive, used to adjust maintenance respiration based on plant age, after GenVeg, on the logic that older plants have more dead biomass and require less maintenance respiration.
  respmaint <- rmprime * proplive #Maintenance respiration in g glucose, after GenVeg, citing Teh (2006) p. 145. 
  respbm <- respmaint/glucosereq #Convert respiration losses from g glucose to g biomass, after GenVeg. 
  
  #Return biomass loss due to respiration (a positive number).
  return(respbm)
  }
}
