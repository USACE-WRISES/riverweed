#' Convert Podostemum biomass to stem length
#'
#' \code{biomass.to.stemlength.pod} calculates stem length of riverweed given biomass
#'     based on multiple statistical relationships.
#' 
#' @param afdm.g biomass in ash-free dry mass in grams
#' @param type an integer between 1 and 6 selecting one of the statistical models detailed below.
#'
#' @return stem length in centimeters (cm)
#'  
#' @details 
#' Given Podostemum biomass in ash-free dry mass (AFDM) in grams, this function uses relationships 
#' derived from (Rack 2022 unpublished data) to calculate the stem's length in centimeters (cm).
#' Multiple alternative relationships may be specified as follows. The original data set
#' included biomasses ranging from 0.0033 g to 0.2856 g and stem lengths ranging from 1.0 cm to 31.2 cm. 
#' Relationships here are the inverses of relationships determined by regressing biomass on stem length, 
#' and the R2 values provided refer to those regressions before inversion.
#' The data set included both branched and unbranched stems all of the dark, leafy growth form of Podostemum. 
#' In all functions, $B_{g}$ denotes ash free dry mass in grams and $L_{cm}$ denotes stem length in 
#' centimeters. An error message is output for incorrect type specification and in the case of
#' logical (TRUE/FALSE) inputs.  A warning message is produced for negative biomasses.  A warning
#' message is produced for unrealistic biomass, here considered to be above 0.4 g.  
#' 
#' Type 1: Linear model including branched and unbranched stems ($R^{2} = 0.727$).
#' $L_{cm} = B_{g}/0.0048452$
#' 
#' Type 2: Linear model including unbranched stems only ($R^{2} = 0.935$).
#' $L_{cm} = B_{g}/0.0023196$
#' 
#' Type 3: Linear model including branched stems only ($R^{2} = 0.870$).
#' $L_{cm} = B_{g}/0.0067453$
#' 
#' Type 4: Power model including branched and unbranched stems ($R^{2} = 0.748$).
#' $L_{cm} = (B_{g}/0.003102984) ^ (1/1.020115)$
#' 
#' Type 5: Power model including unbranched stems only ($R^{2} = 0.753$).
#' $L_{cm} = (B_{g}/0.004178842) ^ (1/0.752031)$
#' 
#' Type 6: Power model including branched stems only ($R^{2} = 0.800$).
#' $L_{cm} = (B_{g}/0.001903129) ^ (1/1.363027)$
#' 
#' @references
#' Rack, Laura. 2022. River Basin Center, University of Georgia. Unpublished data. 
#' 
#' @examples
#' #Result: 20.63898
#' biomass.to.stemlength.pod(0.1, type=1)
#' 
#' #Result: 43.11088
#' biomass.to.stemlength.pod(0.1, type=2)
#' 
#' #Result: 14.82514
#' biomass.to.stemlength.pod(0.1, type=3)
#' 
#' 
#' @export
biomass.to.stemlength.pod <- function(afdm.g, type){
  #Error handling for invalid model specification
  if(!(type %in% seq(1,6))){stop("Invalid model specification")}
  
  #Error handling for logical inputs
  if(is.logical(afdm.g) || is.logical(type)){stop("Invalid input")}
  
  #Computation of AFDM for all other (realistic) stem lengths
  #Type 1
  else if(type == 1){stem.cm <- afdm.g / 0.0048452}
  
  #Type 2
  else if(type == 2){stem.cm <- afdm.g / 0.0023196}
  
  #Type 3
  else if(type == 3){stem.cm <- afdm.g / 0.0067453}
  
  #Type 4
  else if(type == 4){stem.cm <- (afdm.g / 0.003102984) ^ (1 / 1.020115)}
  
  #Type 5
  else if(type == 5){stem.cm <- (afdm.g / 0.004178842) ^ (1 / 0.752031)}
  
  #Type 6
  else if(type == 6){stem.cm <- (afdm.g / 0.001903129) ^ (1 / 1.363027)}
  
  #Send warning for any stem length greater than 50 cm
  if(any(afdm.g < 0) || any(afdm.g > 0.4)){warning("Unrealistic biomasses")}
  
  #Send output
  return(stem.cm)
}