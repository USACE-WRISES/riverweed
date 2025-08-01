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
#' @return daily macrophyte biomass gain due to exponential or logistic growth.
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
#' 1200 g ash free dry mass / m2 based on Grubaugh and Wallace (1995) and Wood et al. (2019). Note that 
#' max.size is given a default value of 1e40 so that simulating exponential growth (type=1) does not 
#' require specifying a maximum size for most reasonable biomass values. 
#' 
#' @references
#' Krebs CJ. 2009. Ecology: The experimental analysis of distribution and abundance, 6th edition. Boston, MA: 
#' Benjamin Cummings.
#' Grubaugh, Jack W, and J Bruce Wallace. 1995. Functional Structure and Production of the Benthic Community 
#' in a Piedmont River: 1956-1957 and 1991-1992. Limnology and Oceanography 40(3): 490–501.
#' Wood, James L.; Skaggs, Jon W.; Conn, Caitlin; and Freeman, Mary C. 2019. Water velocity regulates
#' macro-consumer herbivory on the benthic macrophyte Podostemum ceratophyllum Michx. Freshwater Biology
#' 64(11): 2037-2045. DOI: 10.1111/fwb.13393
#' 
#' @examples
#' #Result: macrophyte biomass gain of 0.5 g
#' growth.empirical(10, 0.05, max.size=NA, type=1)
#' 
#' #Result: macrophyte biomass gain of 0.5 g
#' growth.empirical(10, 0.05, type=1)
#' 
#' #Result: macrophyte biomass gain of 1.5 g
#' growth.empirical(30, 0.05, max.size=50, type=1)
#' 
#' #Result: macrophyte biomass gain of 0.6 g
#' growth.empirical(30, 0.05, max.size=50, type=2)
#' 
#' @export
growth.empirical <- function(old.size, growth.rate, max.size=1e40, type){
  
  #Error handling for invalid model specification
  if(!(type %in% seq(1,2))){stop("Invalid model specification")}
  
  #Error handling for invalid inputs
  if(old.size < 0){stop("Macrophyte size cannot be negative")}
  if(type==2){if(max.size < 0){stop("Macrophyte size cannot be negative")}}
  #if(type==2){if(old.size > max.size){stop("old.size cannot exceed max.size")}}
  if(growth.rate < 0){stop("Growth rate cannot be negative")}
  if(is.logical(old.size) || is.logical(growth.rate) || isTRUE(max.size) || isFALSE(max.size) || 
     is.logical(type)){stop("Invalid input")}
  
  
  #Type 1: Exponential growth
  if(type == 1){change.size <- growth.rate * old.size}
  
  #Type 2: Logistic growth
  else if(type == 2){change.size <- growth.rate * ((max.size-old.size)/max.size) * old.size}
  
  #Return size increment
  return(change.size)
}

