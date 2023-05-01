#' Calculate macrophyte losses to scour based on empirical data
#'
#' \code{scour} calculates the size of a macrophyte after losses due to scour.
#' 
#' @param old.size size at prior time step.
#' @param size.min minimum size at which macrophyte is susceptible to scour.
#' @param S maximum scour rate in proportion lost per day.
#' @param Vhigh flow velocity (in m/s) above which scour is at the maximum rate.
#' @param Vlow flow velocity (in m/s) below which scour is 0.
#' @param V flow velocity in m/s.
#' @param type an integer between 1 and 3 specifying the numerical form of the scour model.
#'
#' @return size of macrophyte after scour
#' 
#' @details 
#' This function calculates macrophyte size (stem length or biomass) following losses due to scour in a 
#' given time step. The extent of scour is considered to be positively related to flow velocity (Wood et 
#' al. 2019). This function allows the velocity-scour relationship to take multiple forms, described 
#' below, where old.size is the size of the macrophyte at the beginning of the time step, S is the maximum
#' scour rate in proportion lost per day, Vlow is the flow velocity (in m/s) below which scour is 0, Vhigh 
#' is the flow velocity (in m/s) above which scour is at its maximum rate S, and V is the flow velocity in
#' the current time step, in m/s. All measures of macrophyte size must use the same units.
#' 
#' @references
#'  Wood JL, Skaggs JW, Conn C, Freeman MC. 2019. Water velocity regulates macro-consumer herbivory on
#'  the benthic macrophyte Podostemum ceratophyllum Michx. Freshwater Biology 64(11): 2037-2045. 
#'  DOI: 10.1111/fwb.13393.
#' 
#' @examples
#' #Result: Stem length of 5 cm
#' scour(old.size=10, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=2.5, type=1)
#' 
#' #Result: Ash-free dry mass of 0.8 g
#' scour(old.size=0.8, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=2.5, type=2)
#' 
#' #Result: Ash-free dry mass of 0.4 g
#' scour(old.size=0.8, size.min=2, S=0.5, Vlow=2.0, Vhigh=3.5, V=3.6, type=2)
#' 
#' #Result: Stem length of 10 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=1, type=3)
#' 
#' #Result: Stem length of 8 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=2.5, type=3)
#' 
#' #Result: Stem length of 4 cm
#' scour(old.size=10, size.min=2, S=0.6, Vlow=2.0, Vhigh=3.5, V=3.6, type=3)
#' 
#' @export
scour <- function(old.size, size.min, S, Vlow, Vhigh, V, type){
  #Error handling for invalid model specification
  if(!(type %in% seq(1,3))){new.size <- "Invalid model specification"}
  
  #Computation of size (e.g. biomass) lost to scour
  #Type 1: lose constant proportion of size
  else if(type == 1){scour.loss <- rep(S, length.out=length(V))}
  
  #Type 2: lose constant proportion of size when velocity above threshold, otherwise no herbivory
  else if(type == 2){scour.loss <- ifelse(V > Vhigh, S, 0)}
  
  #Type 3: step function: lose no biomass when V < Vlow, lose maximum proportion of biomass when V > Vhigh, and transition linearly between them.
  else if(type == 3){scour.loss <- ifelse(V < Vlow, 0,
                                          ifelse(V < Vhigh, (S/(Vhigh-Vlow))*V+(S*Vlow/(Vlow-Vhigh)), S))}
  
  #Compute change in biomass from proportional change
  new.size <- old.size * (1-scour.loss)
  
  #Send output
  return(new.size)
}
