#' Calculate macrophyte mortality
#'
#' \code{mortality} calculates macrophyte biomass loss due to mortality
#' 
#' @param old.size initial biomass of macrophyte
#' @param mort.rate mortality rate of macrophyte, in g/g/day
#'
#' @return 
#'  
#' @details 
#' Calculates plant mortality, following NetLogo version of mortality rather than R GenVeg version. Note GenVeg uses mortality rate of 0.021 /d (that is, 2.1% loss per day), and the GenVeg NetLogo mortality file contains some interesting simple respiration functions.
#' 
#' 
#' 
#' 
#' @references
#' Rack, Laura. 2022. River Basin Center, University of Georgia. Unpublished data. 
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
mortality <- function(old.size, mort.rate){
  new.size <- old.size * (1-mort.rate)
  return(new.size)
}
