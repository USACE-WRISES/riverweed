#' Calculate macrophyte losses to desiccation
#'
#' \code{desiccation} calculates macrophyte losses due to desiccation.
#' 
#' @param old.size size at prior time step (e.g., biomass or stem length).
#' @param D desiccation rate in proportion lost per day.
#' @param depth a vector of water depths at the last N time steps, where N = dry.time.limit 
#' @param depth.limit shallowest water depth the macrophyte can tolerate without experiencing desiccation.
#' @param dry.time.limit number of time steps the macrophyte can tolerate at water depths shallower than depth.limit without experiencing desiccation.
#' 
#' @return daily macrophyte loss due to desiccation, a positive number.
#' 
#' @details 
#' This function calculates macrophyte size (e.g., stem length or biomass) lost due to desiccation in a 
#' given time step. Desiccation is assumed to reduce macrophyte size at rate D whenever water is shallower 
#' than a given depth (depth.limit) for at least a set number of time steps (the length of the depth input). 
#' All measures of macrophyte size must use the same units. Podostemum, like many macrophytes, has been 
#' shown to be sensitive to desiccation (Wood and Freeman 2017).
#' 
#' @references
#'  Wood, James, and Mary Freeman. “Ecology of the Macrophyte Podostemum Ceratophyllum Michx. (Hornleaf 
#'  Riverweed), a Widespread Foundation Species of Eastern North American Rivers.” Aquatic Botany 139 
#'  (March 2017): 65–74. https://doi.org/10.1016/j.aquabot.2017.02.009.
#' 
#' @examples
#' #Result: macrophyte biomass loss of 0 g
#' desiccation(old.size=10, D=.4, depth=2, depth.limit=1)
#' 
#' #Result: macrophyte biomass loss of 4 g
#' desiccation(old.size=10, D=.4, depth=1, depth.limit=2)
#' 
#' #Result: macrophyte biomass loss of 0 g
#' desiccation(old.size=10, D=.4, depth=c(1.3, 1.2, 1.1, 1), depth.limit=0.5)
#' 
#' #Result: macrophyte stem length loss of 4 cm
#' desiccation(old.size=10, D=.4, depth=c(1.3, 1.2, 1.1, 1), depth.limit=2)
#' 
# #Result: macrophyte stem length loss of 0 cm
#' desiccation(old.size=10, D=.4, depth=c(1.3, 1.2, 2.1, 1), depth.limit=2)
#' 
#' #Result: macrophyte stem length loss of 12 cm
#' desiccation(old.size=15, D=.8, depth=0, depth.limit=.5)
#' 
#' @export

desiccation <- function(old.size, D, depth, depth.limit){
  #Error handling for invalid model specification
  if(D < 0 | D > 1){stop("D must be between 0 and 1")}
  if(is.logical(old.size) || is.logical(D) || is.logical(depth) || is.logical(depth.limit)){stop("Invalid input")}
  if(old.size < 0){stop("Macrophyte size cannot be negative")}
  
  #Computation of size (e.g., biomass) lost to desiccation
  #Desiccation occurs if depth is less than depth.limit
  else if(all(depth < depth.limit)){dry.loss <- D * old.size}
  #Otherwise desiccation does not affect macrophyte size
  else {dry.loss <- 0}
  
  #Return biomass increment (a positive number).
  return(dry.loss)
} #End fxn call
