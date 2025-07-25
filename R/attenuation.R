#' Calculate light attenuation in water
#'
#' \code{attenuation} calculates available photosynthetically active radiation (PAR) at a given depth (e.g., the leaf surface), taking into account reflection off the water surface and attenuation with depth. 
#' 
#' @param PAR photosynthetically active radiation at the water surface, in microEinsteins.
#' @param z water depth in meters.
#' @param prop.reflect proportion of light reflected by the water surface (default 0.1).
#' @param K light attenuation coefficient of water (default 0.12)
#' @param Kp light attenuation coefficient of plant material.  GenVeg uses a default of 0.02 m2/g, but they use the following for crops: rice 0.38 m2/g, wheat 0.48 m2/g, corn 0.51 m2/g, citing Chakraborty et al. 2018. GenVeg's NetLogo photosynthesis function uses 0.0235 citing van Nes et al. 2003 Eco Mod which provided that value for Vallisneria.
#' @param Bz plant biomass above depth z.
#' @param selfshading a logical value indicating whether the macrophyte experiences self-shading (TRUE) or not (FALSE).
#'
#' @return a vector of three PAR values in microEinsteins, corresponding to light available to macrophyte leaves at three representive times of day, roughly midday, morning/afternoon, and dawn/dusk.
#'  
#' @details 
#' Given photosynthetically active radiation (PAR) available at the water surface, attenuation calculates PAR at any water depth, including losses of PAR due to reflection off the water surface, attenuation in the water column, and optional self-shading by macrophytes.  Default values are taken from GenVeg (NetLogo version, photosynthesis function), who cite (http://www.lakeaccess.org/ecology/lakeecologyprim3.html) for the light attenuation coefficient K, and (http://www.esf.edu/efb/schulz/Limnology/Light.html) for the equation without self-shading.  Kp and Bz are given default values of zero so that they do not need to be specified when selfshading=F.  As a result, setting selfshading=T will not affect the result of attenuation unless nonzero (positive) values of Kp and Bz are specified.
#' 
#' 
#' 
#' 
#' @references
#' Chakraborty, P. K.; Banerjee, S.; Mukherjee, A.; Nath, R.; and Samanta, S. 2018. Extinction coefficient and photosynthetically active radiation use efficiency of summer rice as influenced by transplanting dates. Journal of Environmental Biology 39: 467-471. DOI: http://doi.org/10.22438/jeb/39/4/MRN-661.
#' GenVeg
#' van Nes, Egbert H; Scheffer, Marten; van den Berg, Marcel S., and Coops, Hugo. 2003. Charisma: a spatial explicit simulation model of submerged macrophytes. Ecological Modelling 159: 103-116. https://doi.org/10.1016/S0304-3800(02)00275-2.
#' http://www.lakeaccess.org/ecology/lakeecologyprim3.html
#' http://www.esf.edu/efb/schulz/Limnology/Light.html
#' 
#' @examples
#' #Result: a vector of the following three PAR values in microEinsteins: 
#' #2394.6852, 1596.4568, 399.1142
#' attenuation(PAR=c(3000, 2000, 500), z=1, prop.reflect=0.1, K=0.12, Kp=0.0235, Bz=0.3, selfshading=FALSE)
#' 
#' #Several ways of increasing the amount of light attenuated, yielding:
#' #1351.8429, 901.2286, 225.3071
#' attenuation(PAR=c(3000, 2000, 500), z=1.5, prop.reflect=0.2, K=0.32, Kp=0.235, Bz=0.4, selfshading=TRUE)
#' 
#' #
#' 
#' @export

attenuation <- function(PAR, z, prop.reflect, K, Kp=0, Bz=0, selfshading){
  
  #Error handling for invalid parameter values
  if(is.logical(PAR) || is.logical(z) || is.logical(prop.reflect) || is.logical(K) || is.logical(Kp) || is.logical(Bz)){stop("Invalid input")}
  if(any(PAR<0)){stop("PAR cannot be negative")}
  if(z<0){stop("Depth z cannot be negative")}
  if(prop.reflect<0 || prop.reflect>1){stop("prop.reflect must be between 0 and 1")}
  if(K<0 || Kp<0){stop("Light attenuation coefficients cannot be negative")}
  if(Bz<0){stop("Biomass pools cannot be negative")}
  
  if(selfshading==TRUE){
  Iz <- ((1 - prop.reflect) * PAR) * exp(-K*z - Kp*Bz)
  } else {
    Iz <- ((1 - prop.reflect) * PAR) * exp(-K*z)
  }

  
  
return(Iz)
  
  
  }
