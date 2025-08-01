#' Calculate macrophyte mortality
#'
#' \code{mortality} calculates macrophyte size loss due to mortality
#' 
#' @param old.size initial size of macrophyte
#' @param mort.rate mortality rate of macrophyte, in /day
#'
#' @return daily biomass loss due to mortality, as a positive number
#'  
#' @details 
#' Calculates plant mortality, following NetLogo version of mortality rather than R 
#' GenVeg version. Note GenVeg uses mortality rate of 0.021 /d (that is, 2.1% loss per 
#' day), and the GenVeg NetLogo mortality file contains some interesting simple 
#' respiration functions.
#' 
#' @references
#' GenVeg
#' 
#' @examples
#' #Result: macrophyte biomass loss of 3 g
#' mortality(10, 0.3)
#' 
#' #Result: macrophyte biomass loss of 5 g
#' mortality(25, 0.2)
#' 
#' @export
mortality <- function(old.size, mort.rate){
  
  #Error handling for invalid inputs
  if(old.size < 0){stop("Macrophyte size cannot be negative")}
  if(mort.rate < 0 || mort.rate > 1){stop("Mortality rate must be between 0 and 1")}
  if(is.logical(old.size) || is.logical(mort.rate)){stop("Invalid input")}
  
  #Calculate and return size increment (a positive number).
  change.size <- old.size * mort.rate
  return(change.size)
}
