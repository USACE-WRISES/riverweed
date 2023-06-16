#' Calculate macrophyte mortality
#'
#' \code{mortality} calculates macrophyte biomass loss due to mortality
#' 
#' @param old.size initial biomass of macrophyte
#' @param mort.rate mortality rate of macrophyte, in g/g/day
#'
#' @return biomass of macrophyte after mortality
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
#' #Result: Biomass of 1.96 g
#' mortality(old.size=2, mort.rate=0.02)
#' 
#' @export
mortality <- function(old.size, mort.rate){
  new.size <- old.size * (1-mort.rate)
  return(new.size)
}
