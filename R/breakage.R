#' Break stems of unrealistic stem length
#'
#' \code{breakage} removes stem length for physiologically unrealistic stem length
#' 
#' @param stem.cm stem length in cm.
#' @param max.stem.cm physiological maximum of stem length in cm.
#'
#' @return daily macrophyte loss due to breakage, a positive number.
#'  
#' @details 
#' This function removes stem length in excess of a user-defined physiological maximum, returning 
#' unrealistically high stem lengths to their maximum allowed value. Parameterization is based on
#' observations that most Podostemum stems are less than 20 cm long (Hammond 1937), and we rarely 
#' observe stems longer than ~30 cm. This function could also be applied to biomass, in which case a 
#' reasonable upper limit for a single stem is ~0.4 g ash-free dry mass.
#' 
#' #' @references
#' Hammond, Bayard L. 1937. Development of Podostemon ceratophyllum. Bulletin of the Torrey Botanical 
#' Club 64(1): 17-36.
#' 
#' @examples
#' #Result: 0
#' breakage(20, 50)
#' 
#' #Result: 20
#' breakage(70, 50)
#' 
#' 
#' @export
breakage <- function(stem.cm, max.stem.cm){
  #Error handling for invalid inputs
  if(stem.cm < 0 || max.stem.cm < 0){stop("Stem length cannot be negative")}
  if(is.logical(stem.cm) || is.logical(max.stem.cm)){stop("Invalid input")}
  
  
  #If current stem length is less than or equal to max stem length, set loss increment to zero; otherwise reduce current stem length to max stem length (loss increment is amount broken off to reach max allowed stem length).
  stem.out <- ifelse(stem.cm <= max.stem.cm, 0, stem.cm-max.stem.cm)
  
  #Send output
  return(stem.out)
} #End fxn call
