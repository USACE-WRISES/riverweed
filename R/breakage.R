#' Break stems of unrealistic stem length
#'
#' \code{breakage} removes stem length for physiologically unrealistic stem length
#' 
#' @param stem.cm stem length in cm.
#' @param max.stem.cm physiological maximum of stem length in cm.
#'
#' @return size of macrophyte after breakage as stem length in cm.
#'  
#' @details 
#' This function removes stem length in excess of a user-defined physiological maximum, returning 
#' unrealistically high stem lengths to their maximum allowed value. Parameterization is based on
#' observations that most Podostemum stems are less than 20 cm long (Hammond 1937), and we rarely 
#' observe stems longer than ~30 cm. 
#' 
#' #' @references
#' Hammond, Bayard L. 1937. Development of Podostemon ceratophyllum. Bulletin of the Torrey Botanical 
#' Club 64(1): 17-36.
#' 
#' @examples
#' #Result: 20 cm
#' breakage(20, 50)
#' 
#' #Result: 50 cm
#' breakage(70, 50)
#' 
#' #Result: vector of values with max of 50 cm
#' breakage(seq(10,100,10), 50)
#' 
#' @export
breakage <- function(stem.cm, max.stem.cm){
  #Compute the minimum of the two inputs
  stem.out <- ifelse(stem.cm <= max.stem.cm, stem.cm, max.stem.cm)

  #Send output
  return(stem.out)
}
