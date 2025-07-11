#' Calculate macrophyte losses to desiccation
#'
#' \code{desiccation} calculatesm acrophyte losses due to desiccation.
#' 
#' @param old.size size at prior time step (e.g., biomass or stem length).
#' @param depth water depth at current time step.
#' @param depth.limit shallowest water depth the macrophyte can tolerate without experiencing desiccation.
#' @param dry.time current length of time the macrophyte has spent in water shallower than depth.limit.
#' @param dry.time.limit length of time the macrophyte can tolerate in water shallower than depth.limit without experiencing desiccation.
#' @param D desiccation rate in proportion lost per day.
#'
#' @return daily macrophyte loss due to desiccation, a positive number.
#' 
#' @details 
#' This function calculates macrophyte size (e.g., stem length or biomass) lost due to desiccation in a 
#' given time step. Desiccation is assumed to reduce macrophyte size at rate D whenever water is shallower 
#' than a given depth (depth.limit) for a given amount of time (dry.time.limit) or longer. All measures of 
#' macrophyte size must use the same units. Podostemum, like many macrophytes, has been shown to be 
#' sensitive to desiccation (Wood and Freeman 2017).
#' 
#' @references
#'  Wood, James, and Mary Freeman. “Ecology of the Macrophyte Podostemum Ceratophyllum Michx. (Hornleaf 
#'  Riverweed), a Widespread Foundation Species of Eastern North American Rivers.” Aquatic Botany 139 
#'  (March 2017): 65–74. https://doi.org/10.1016/j.aquabot.2017.02.009.
#' 
#' @examples
#' #Result: macrophyte biomass loss of 5 g
#' scour(old.size=10, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=2.5, type=1)
#' 
#' #Result: macrophyte biomass loss of 0 g
#' scour(old.size=0.8, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=2.5, type=2)
#' 
#' #Result: macrophyte biomass loss of 0.4 g
#' scour(old.size=0.8, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=3.6, type=2)
#' 
#' #Result: macrophyte stem length loss of 0 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=1, type=3)
#' 
#' #Result: macrophyte stem length loss of 2 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=2.5, type=3)
#' 
#' #Result: macrophyte stem length loss of 6 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=3.6, type=3)
#' 
#' @export
scour <- function(old.size, size.min, S, Vlow, Vhigh, V, type){
  #Error handling for invalid model specification
  if(!(type %in% seq(1,3))){stop("Invalid model specification")}
  if(S < 0 | S > 1){stop("S must be between 0 and 1")}
  if(is.logical(old.size) || is.logical(S) || is.logical(Vlow) || is.logical(Vhigh) || is.logical(V) || is.logical(type)){stop("Invalid input")}
  if(old.size < 0 || size.min < 0){stop("Macrophyte size cannot be negative")}
  if(Vlow < 0 || Vhigh < 0 || V < 0){stop("Velocity cannot be negative")}
  if(Vlow > Vhigh){stop("Vlow must be less than or equal to Vhigh")}
  
  #Computation of size (e.g. biomass) lost to scour
  #Type 1: lose constant proportion of size
  else if(type == 1){scour.loss <- S * old.size}
  
  #Type 2: lose constant proportion of size when velocity above threshold, otherwise no scour
  else if(type == 2){scour.loss <- ifelse(V > Vhigh, S * old.size, 0)}
  
  #Type 3: step function: lose no biomass when V < Vlow, lose maximum proportion of biomass when V > Vhigh, and transition linearly between them.
  else if(type == 3){scour.loss <- ifelse(V < Vlow, 0,
                                          ifelse(V < Vhigh, ((S/(Vhigh-Vlow))*V+(S*Vlow/(Vlow-Vhigh))) * old.size, S * old.size))}
  
  #Return size increment (a positive number).
  return(scour.loss)
}
