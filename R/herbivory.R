#' Calculate macrophyte losses to herbivory based on empirical data
#'
#' \code{herbivory} calculates the size of a macrophyte after losses due to herbivory.
#' 
#' @param old.size size at prior time step.
#' @param H maximum herbivory rate in proportion lost per day.
#' @param Vlow flow velocity (in m/s) below which herbivory is at the maximum rate.
#' @param Vhigh flow velocity (in m/s) above which herbivory is 0.
#' @param V flow velocity in m/s.
#' @param type an integer between 1 and 3 specifying the numerical form of the herbivory model.
#'
#' @return size of macrophyte after herbivory
#'  
#' @details 
#' This function calculates macrophyte size (stem length or biomass) following losses due to herbivory in a 
#' given time step. The extent of herbivory is considered to be negatively related to flow velocity (Wood 
#' et al. 2019). This function allows the velocity-herbivory relationship to take multiple forms, described 
#' below, where old.size is the size of the macrophyte at the beginning of the time step, H is the maximum 
#' herbivory rate in proportion lost per day, Vlow is the flow velocity (in m/s) below which herbivory
#' is at its maximum rate H, Vhigh is the flow velocity (in m/s) above which herbivory ceases, and V
#' is the flow velocity in the current time step, in m/s. All measures of macrophyte size must use the same
#' units.
#' 
#' Type 1: Constant herbivory.
#' 
#' Type 2: Herbivory is driven by a threshold in velocity.
#' 
#' Type 3: Herbivory is described by a step function, starting at its maximum rate for velocities less 
#' than Vlow, and declining linearly to reach zero when velocity reaches Vhigh.
#' 
#' @references
#'  Wood JL, Skaggs JW, Conn C, Freeman MC. 2019. Water velocity regulates macro-consumer herbivory on
#'  the benthic macrophyte Podostemum ceratophyllum Michx. Freshwater Biology 64(11): 2037-2045. 
#'  DOI: 10.1111/fwb.13393.
#' 
#' @examples
#' #Result: Stem length of 18 cm
#' herbivory(20, H=0.1, Vlow=0.5, Vhigh=2.5, V=1, type=1)
#' 
#' #Result: Ash-free dry mass of 0.1 g
#' herbivory(0.2, H=0.5, Vlow=0.5, Vhigh=2.5, V=1.0, type=2)
#' 
#' #Result: Ash-free dry mass of 0.2 g
#' herbivory(0.2, H=0.5, Vlow=0.5, Vhigh=2.5, V=2.7, type=2)
#' 
#' #Result: Stem length of 14 cm
#' herbivory(20, H=0.3, Vlow=0.5, Vhigh=2.5, V=0.2, type=3)
#' 
#' #Result: Stem length of 15.8 cm
#' herbivory(20, H=0.3, Vlow=0.5, Vhigh=2.5, V=1.1, type=3)
#' 
#' #Result: Stem length of 20 cm
#' herbivory(20, H=0.3, Vlow=0.5, Vhigh=2.5, V=2.6, type=3)
#' 
#' @export
herbivory <- function(old.size, H, Vlow, Vhigh, V, type){
  #Error handling for invalid inputs
  if(!(type %in% seq(1,3))){stop("Invalid model specification")}
  if(H<0 | H>1){stop("H must be between 0 and 1")}
  if(is.logical(old.size) || is.logical(H) || is.logical(Vlow) || is.logical(Vhigh) || is.logical(V) || is.logical(type)){stop("Invalid input")}
  if(old.size < 0){stop("Macrophyte size cannot be negative")}
  if(Vlow < 0 || Vhigh < 0 || V < 0){stop("Velocity cannot be negative")}
  if(Vlow > Vhigh){stop("Vlow must be less than or equal to Vhigh")}
  
  #Computation of size (e.g. biomass) lost to herbivory
    #Type 1: lose constant proportion of size
    else if(type == 1){herb.loss <- rep(H, length.out=length(V))}
  
    #Type 2: lose constant proportion of size when velocity below threshold, otherwise no herbivory
    else if(type == 2){herb.loss <- ifelse(V < Vhigh, H, 0)}

    #Type 3: proportion of size lost declines linearly with velocity
    else if(type == 3){herb.loss <- ifelse(V < Vlow, H,
                                           ifelse(V < Vhigh, (H/(Vlow-Vhigh))*V-(H*Vhigh/(Vlow-Vhigh)), 0))}
  
  #Return size increment (a positive number).
  return(herb.loss)
}
