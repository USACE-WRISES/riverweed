#' Calculate macrophyte losses to scour based on empirical data
#'
#' \code{scour} calculates macrophyte losses due to scour.
#' 
#' @param old.size size at prior time step.
#' @param size.min minimum size at which macrophyte is susceptible to scour.
#' @param S maximum scour rate in proportion lost per day.
#' @param Vhigh flow velocity above which scour is at the maximum rate.
#' @param Vlow flow velocity below which scour is 0.
#' @param V flow velocity.
#' @param type an integer between 1 and 3 specifying the numerical form of the scour model.
#'
#' @return daily macrophyte loss due to scour, a positive number.
#' 
#' @details 
#' This function calculates macrophyte size (e.g., stem length or biomass) lost due to scour in a 
#' given time step. The extent of scour is considered to be positively related to flow velocity (Wood et 
#' al. 2019). This function allows the velocity-scour relationship to take multiple forms, described 
#' below, where old.size is the size of the macrophyte at the beginning of the time step, S is the maximum
#' scour rate in proportion lost per day, Vlow is the flow velocity below which scour is 0, Vhigh 
#' is the flow velocity above which scour is at its maximum rate S, and V is the flow velocity in
#' the current time step. All measures of macrophyte size must use the same units.
#' 
#' Type 1: Constant scour.
#' 
#' Type 2: Scour occurs at rate S whenever velocity is above a threshold.
#' 
#' Type 3: Scour is described by a step function, starting at zero for velocities less 
#' than Vlow, and increasing linearly to reach its maximum rate S when velocity reaches Vhigh.
#' 
#' @references
#'  Wood JL, Skaggs JW, Conn C, Freeman MC. 2019. Water velocity regulates macro-consumer herbivory on
#'  the benthic macrophyte Podostemum ceratophyllum Michx. Freshwater Biology 64(11): 2037-2045. 
#'  DOI: 10.1111/fwb.13393.
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
