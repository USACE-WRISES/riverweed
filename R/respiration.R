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
#' @return 
#'  
#' @details 
#' This function calculates macrophyte biomass losses due to maintenance respiration, and is intended to be used alongside the photosynthesis function to convert gross photosynthetic assimilation to net photosynthetic assimilation.  This function applies to a single plant organ or tissue, but can be looped over different organs or tissues with different metabolic properties.  Positive values of respiration represent biomass lost to respiration, so net photosynthesis = gross photosynthesis - respiration.
#' 
#' 
#' 

#' 
#' @references
#' GenVeg
#' Teh, Christopher B. S. 2006. Introduction to mathematical modeling of crop growth: How the equations are derived and assembled into a computer model. Boca Raton, FL: BrownWalker Press.
#' 
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
respiration <- function(kmprime, temp, liveweight, totalweight, glucosereq){
  if(totalweight < liveweight){stop("totalweight must be greater than or equal to liveweight")}
  else if(totalweight < 0 | liveweight < 0){stop("biomass cannot be negative")}
  else if(glucosereq < 0){stop("glucose requirement cannot be negative")}
  else if(kmprime < 0){stop("kmprime cannot be negative")}
  else{
  km <- kmprime*2^((temp-25)/10) #Respiration coefficient for a generic tissue after GenVeg, who cite temperature dependence from Teh (2006) p. 134.
  rmprime <- km*liveweight #Maintenance respiration per day after GenVeg, citing Teh (2006)
  proplive <- liveweight/totalweight #Proportion of tissue biomass that is alive, used to adjust maintenance respiration based on plant age, after GenVeg, on the logic that older plants have more dead biomass and require less maintenance respiration.
  respmaint <- rmprime * proplive #Maintenance respiration in g glucose, after GenVeg, citing Teh (2006) p. 145. 
  respbm <- respmaint/glucosereq #Convert respiration losses from g glucose to g biomass, after GenVeg. 
  return(respbm)
  }
}
