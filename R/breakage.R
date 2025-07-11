#' Break unrealistically large macrophytes
#'
#' \code{breakage} calculates macrophyte losses due to stem breakage above a physiological threshold.
#' 
#' @param current.size current size of macrophyte (e.g., biomass or stem length)
#' @param max.size physiological maximum size before breakage occurs
#'
#' @return daily macrophyte loss due to breakage, a positive number.
#'  
#' @details 
#' This function removes macrophyte size in excess of a user-defined physiological 
#' maximum, returning unrealistically large macrophytes to their maximum allowed size. 
#' Rationale for Podostemum is based on Hammond (1937)'s observation that most
#' Podostemum stems are less than 20 cm long, and we rarely observe stems longer 
#' than ~30 cm.
#' 
#' #' @references
#' Hammond, Bayard L. 1937. Development of Podostemon ceratophyllum. Bulletin of the 
#' Torrey Botanical Club 64(1): 17-36.
#' 
#' @examples
#' #Result: macrophyte biomass loss of 0 g
#' breakage(20, 50)
#' 
#' #Result: macrophyte biomass loss of 20 g
#' breakage(70, 50)
#' 
#' 
#' @export
breakage <- function(current.size, max.size){
  #Error handling for invalid inputs
  if(current.size < 0 || max.size < 0){stop("Macrophyte size cannot be negative")}
  if(is.logical(current.size) || is.logical(max.size)){stop("Invalid input")}
  
  
  #If current size is less than or equal to max size, set loss increment to zero; otherwise reduce current size to max size (loss increment is amount broken off to reach max allowed size).
  size.out <- ifelse(current.size <= max.size, 0, current.size-max.size)
  
  #Send output
  return(size.out)
} #End fxn call
