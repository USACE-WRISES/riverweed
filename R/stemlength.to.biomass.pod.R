#' Convert Podostemum stem length to biomass 
#'
#' \code{stemlength.to.biomass.pod} calculates the ash-free biomass of riverweed given stem length 
#'     based on multiple statistical relationships.
#' 
#' @param stem.cm stem length in centimeters (cm)
#' @param type an integer between 1 and 6 selecting one of the statistical models detailed below.
#'
#' @return biomass in ash-free dry mass in grams
#'  
#' @details 
#' Given Podostemum stem length in cm, this function uses relationships derived from 
#' (Rack 2022 unpublished data) to calculate the stem's ash-free dry mass (AFDM) in grams.
#' Multiple alternative relationships may be specified as follows. The original data set
#' included stem lengths ranging from 1.0 cm to 31.2 cm and biomasses ranging from 0.0033 g to 0.2856 g. 
#' The data set included both branched and unbranched stems all of the dark, leafy growth form of Podostemum. 
#' In all functions, $B_{g}$ denotes ash free dry mass in grams and $L_{cm}$ denotes stem length in 
#' centimeters. An error message is output for incorrect type specification. A warning message is 
#' produced for unrealistic stem length, here considered to be above 50 cm.
#' 
#' Type 1: Linear model including branched and unbranched stems ($R^{2} = 0.727$).
#' $B_{g} = 0.0048452*L_{cm}$
#' 
#' Type 2: Linear model including unbranched stems only ($R^{2} = 0.935$).
#' $B_{g} = 0.0023196*L_{cm}$
#' 
#' Type 3: Linear model including branched stems only ($R^{2} = 0.870$).
#' $B_{g} = 0.0067453*L_{cm}$
#' 
#' Type 4: Power model including branched and unbranched stems ($R^{2} = 0.748$).
#' $B_{g} = 0.003102984 * L_{cm} ^ 1.020115$
#' 
#' Type 5: Power model including unbranched stems only ($R^{2} = 0.753$).
#' $B_{g} = 0.004178842 * L_{cm} ^ 0.752031$
#' 
#' Type 6: Power model including branched stems only ($R^{2} = 0.800$).
#' $B_{g} = 0.001903129 * L_{cm} ^ 1.363027$
#' 
#' @references
#' Rack, Laura. 2022. River Basin Center, University of Georgia. Unpublished data. 
#' 
#' @examples
#' #Result: biomass of 0.048452 g ash-free dry mass
#' stemlength.to.biomass.pod(10, type=1)
#' 
#' #Result: biomass of 0.023196 g ash-free dry mass
#' stemlength.to.biomass.pod(10, type=2)
#' 
#' #Result: biomass of 0.067453 g ash-free dry mass
#' stemlength.to.biomass.pod(10, type=3)
#' 
#' @export
stemlength.to.biomass.pod <- function(stem.cm, type){
  #Error handling for invalid model specification
  if(!(type %in% seq(1,6))){stop("Invalid model specification")}
  
  #Error handling for logical inputs
  if(is.logical(stem.cm) || is.logical(type)){stop("Invalid input")}

  #Computation of AFDM for all other (realistic) stem lengths
    #Type 1
    else if(type == 1){afdm.g <- 0.0048452 * stem.cm}
  
    #Type 2
    else if(type == 2){afdm.g <- 0.0023196 * stem.cm}
  
    #Type 3
    else if(type == 3){afdm.g <- 0.0067453 * stem.cm}

    #Type 4
    else if(type == 4){afdm.g <- 0.003102984 * stem.cm^1.020115}
    
    #Type 5
    else if(type == 5){afdm.g <- 0.004178842 * stem.cm^0.752031}
    
    #Type 6
    else if(type == 6){afdm.g <- 0.001903129 * stem.cm^1.363027}

  #Send warning for any stem length greater than 50 cm
  if(any(stem.cm < 0) || any(stem.cm > 50)){warning("Unrealistic stem lengths")}

  #Send output
  return(afdm.g)
}
