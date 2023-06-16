#' Calculate macrophyte growth based on empirical data
#'
#' \code{growth.empirical} calculates the daily biomass growth of a macrophyte
#' 
#' @param old.size size at prior time step.
#' @param growth.rate relative growth rate in unit / unit per time step.
#' @param max.size maximum size if using logistic growth (must be in same units as old.size; 
#' set to NA for exponential growth).
#' @param type form of growth model as integer of 1 for exponential growth or 2 for logistic growth.
#'
#' @return size of macrophyte after growth.
#'  
#' @details 
#' This function calculates macrophyte growth in a given time step using empirically parameterized 
#' exponential or logistic growth functions, according to the following formulas (Krebs 2009), where 
#' change.size is the growth during that time step, growth.rate is the relative growth rate in 
#' unit / unit per time step, old.size is the size of the macrophyte at the beginning of the time step, 
#' and max.size is the maximum size the macrophyte is allowed to reach. All measures of macrophyte size 
#' must use the same units.
#' 
#' Type 1: Exponential growth model.
#' $change.size = growth.rate*old.size$
#' 
#' Type 2: Logistic growth model.
#' $change.size = growth.rate*((max.size-old.size)/max.size)$
#' 
#' For Podostemum, Wood et al. (2019) estimated a relative growth rate of 0.026 cm/cm/day (95% credible
#' interval 0.013-0.039 cm/cm/day) in the absence of herbivory. We estimate maximum reasonable sizes of 
#' 50 cm or 0.4 g ash-free dry mass based on Rack (2022). 
#' 
#' @references
#' Krebs CJ. 2009. Ecology: The experimental analysis of distribution and abundance, 6th edition. Boston, MA: 
#' Benjamin Cummings.
#' Rack, Laura. 2022. River Basin Center, University of Georgia. Unpublished data. 
#' Wood, James L.; Skaggs, Jon W.; Conn, Caitlin; and Freeman, Mary C. 2019. Water velocity regulates
#' macro-consumer herbivory on the benthic macrophyte Podostemum ceratophyllum Michx. Freshwater Biology
#' 64(11): 2037-2045. DOI: 10.1111/fwb.13393
#' 
#' @examples
#' #Result: stem length of 10.5 cm
#' growth.empirical(10, 0.05, max.size=NA, type=1)
#' 
#' #Result: stem length of 31.5 cm
#' growth.empirical(30, 0.05, max.size=50, type=1)
#' 
#' #Result: stem length of 30.6 cm
#' growth.empirical(30, 0.05, max.size=50, type=2)
#' 
#' @export
growth.empirical <- function(old.size, growth.rate, max.size, type){
  #Type 1: Exponential growth
  if(type == 1){change.size <- growth.rate * old.size}
  
  #Type 2: Logistic growth
  else if(type == 2){change.size <- growth.rate * ((max.size-old.size)/max.size) * old.size}
  
  #Compute new biomass from change and old biomass
  new.size <- old.size + change.size
  
  #Send output
  return(new.size)
}

