#' Calculate light attenuation in water
#'
#' \code{attenuate} calculates available photosynthetically active radiation (PAR) at the water surface, taking into account reflection off the water surface and attenuation with depth. 
#' 
#' @param PAR photosynthetically active radiation at the water surface, in microEinsteins.
#' @param z water depth in meters.
#' @param prop.reflect proportion of light reflected by the water surface (default 0.1).
#' @param K light attenuation coefficient of water (default 0.12)
#' @param Kp light attenuation coefficient of plant material.  GenVeg uses a default of 0.02 m2/g, but they use the following for crops: rice 0.38 m2/g, wheat 0.48 m2/g, corn 0.51 m2/g, citing Chakraborty et al. 2018. GenVeg's NetLogo photosynthesis function uses 0.0235 citing van Nes et al. 2003 Eco Mod which provided that value for Vallisneria.
#' @param Bz plant biomass above depth z
#' @param selfshading a logical value indicating whether the macrophyte experiences self-shading (TRUE) or not (FALSE).
#'
#' @return a vector of three PAR values in microEinsteins, corresponding to light available to macrophyte leaves at three representive times of day, roughly midday, morning/afternoon, and dawn/dusk.
#'  
#' @details 
#' Given photosynthetically active radiation (PAR) available at the water surface, attenuate calculates PAR at any water depth, including losses of PAR due to reflection off the water surface, attenuation in the water column, and optional self-shading by macrophytes.  Default values are taken from GenVeg (NetLogo version, photosynthesis function), who cite (http://www.lakeaccess.org/ecology/lakeecologyprim3.html) for the light attenuation coefficient K, and (http://www.esf.edu/efb/schulz/Limnology/Light.html) for the equation without self-shading.
#' 
#' 
#' 
#' 
#' @references
#' Chakraborty, P. K.; Banerjee, S.; Mukherjee, A.; Nath, R.; and Samanta, S. 2018. Extinction coefficient and photosynthetically active radiation use efficiency of summer rice as influenced by transplanting dates. Journal of Environmental Biology 39: 467-471. DOI: http://doi.org/10.22438/jeb/39/4/MRN-661.
#' van Nes, Egbert H; Scheffer, Marten; van den Berg, Marcel S., and Coops, Hugo. 2003. Charisma: a spatial explicit simulation model of submerged macrophytes. Ecological Modelling 159: 103-116. 
#' http://www.lakeaccess.org/ecology/lakeecologyprim3.html
#' http://www.esf.edu/efb/schulz/Limnology/Light.html
#' 
#' @examples
#' #Result: Stem length of 20.63898 centimeters
#' biomass.to.stemlength.pod(0.1, type=1)
#' 
#' #Result: Stem length of 43.11088 centimeters
#' biomass.to.stemlength.pod(0.1, type=2)
#' 
#' #Result: Stem length of 14.82514 centimeters
#' biomass.to.stemlength.pod(0.1, type=3)
#' 
#' #Result: Error message indicating incorrect model specification
#' biomass.to.stemlength.pod(0.1, type=7)
#' 
#' #Result: Warning message indicating unrealistic biomass
#' #biomass.to.stemlength.pod(1, type=1)
#' 
#' @export

attenuate <- function(PAR, z, prop.reflect, K, Kp, Bz, selfshading){
  if(selfshading==TRUE){
  Iz <- ((1 - prop.reflect) * PAR) * exp(-K*z - Kp*Bz)
  } else {
    Iz <- ((1 - prop.reflect) * PAR) * exp(-K*z)
  }
return(Iz)
  }
