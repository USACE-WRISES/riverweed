#' Calculate growth due to photosynthesis
#'
#' \code{photosynthesis} calculates the amount of biomass gained by a macrophyte on a given day, given a species-specific maximum photosynthetic rate and optional parameters to account for growth limitation by temperature, light, depth, carbonate, and nutrients.
#' 
#' @param PAR a vector of three values of photosynthetically active radiation reaching macrophyte leaves at three representative times of day, in microEinsteins.
#' @param pbiomass biomass of photosynthetic tissue, in g
#' @param Pmax maximum photosynthetic rate in g glucose / g photosynthetic tissue / day. This quantity is species specific; GenVeg provides the following values: rice 0.027, wheat 0.5, corn 0.0372.  
#' @param daylength length of day in hours from sunrise to sunset.
#' @param glucosereq glucose requirement for growth. GenVeg, citing Teh (2006) Table 7.4, give values of 1.436 for leaves, 1.513 for stems, and 1.444 for roots.
#' @param Hi half-saturation constant for light, in units of PAR (microEinsteins).  GenVeg (NetLogo) photosynthesis function suggests 14 for C. aspera and 52 for P. pectinatus.
#' @param temp average daily temperature in °C.
#' @param D depth of photosynthetic tissue below the water surface (?), in same units as Hd. 
#' @param Hd half-saturation constant for depth, in same units as D. GenVeg (NetLogo) photosynthesis function describes this as the half-saturation constant for light diffusion and uses a default of 1 m.  
#' @param C ambient carbonate concentration, in same units as Hc.
#' @param Hc half-saturation constant for carbonate, in same units as C. GenVeg (NetLogo) photosynthesis function suggests 60, but notes that it varies by species.
#' @param N ambient concentration of limiting nutrient, in same units as Hn.
#' @param Hn half-saturation concentration of limiting nutrient, in same units as N.
#'
#' @return daily biomass gain due to gross photosynthetic assimilation.
#'  
#' @details 
#' This function calculates gross photosynthetic assimilation in units of macrophyte biomass given light availability, plant height, river depth, and several plant parameters.  It is designed for use with the light, daylength, and attenuate functions to calculate light availability for photosynthesis.  It is designed to be used alongside the respiration function (which calculates maintenance respiration) to calculate net photosynthetic biomass assimilation.  This function assumes Michaelis-Menten or Monod dynamics for light, carbonate, and nutrient assimilation, as well as depth, following the Charisma model (van Nes et al. 2003). Temperature is assumed to limit photosynthesis by a Hill function.  This function applies to a single plant organ or tissue, but can be looped over different organs or tissues with different photosynthetic properties.  
#' 
#' 
#' 
#' 
#' @references
#' GenVeg
#' 
#' Carr, Geneviève M.; Duthie, Hamish C.; and Taylor, William D. 1997. Models of aquatic plant productivity: a review of the factors that influence growth. Aquatic Botany 59: 195-215. 
#' Chakraborty, P. K.; Banerjee, S.; Mukherjee, A.; Nath, R.; and Samanta, S. 2018. Extinction coefficient and photosynthetically active radiation use efficiency of summer rice as influenced by transplanting dates. Journal of Environmental Biology 39: 467-471. DOI: http://doi.org/10.22438/jeb/39/4/MRN-661.
#' Teh, Christopher B. S. 2006. Introduction to mathematical modeling of crop growth: How the equations are derived and assembled into a computer model. Boca Raton, FL: BrownWalker Press.
#' van Nes, Egbert H; Scheffer, Marten; van den Berg, Marcel S., and Coops, Hugo. 2003. Charisma: a spatial explicit simulation model of submerged macrophytes. Ecological Modelling 159: 103-116. 
#' 
#' @examples
#' #Result: biomass gain of 0.9980636 g
#' photosynthesis(PAR=c(3000, 2000, 500), pbiomass=1, Pmax=0.2, daylength=13, glucosereq=1.5, Hi=30, temp=17)
#' 
#' @export

photosynthesis <- function(PAR, pbiomass, Pmax, daylength, glucosereq, Hi=0, temp, D=1, Hd=0, C=1, Hc=0, N=1, Hn=0){
  #Error handling for invalid parameter values
  if(is.logical(PAR) || is.logical(pbiomass) || is.logical(Pmax) || is.logical(daylength) || is.logical(glucosereq) || is.logical(Hi) || is.logical(temp) || is.logical(D) || is.logical(Hd) || is.logical(C) || is.logical(Hc) || is.logical(N) || is.logical(Hn)){stop("Invalid input")}
  if(any(PAR<0)){stop("PAR cannot be negative")}
  if(pbiomass<0){stop("Biomass cannot be negative")}
  if(daylength<0 || daylength>24){stop("Day length must be between 0 and 24 hours")}
  if(glucosereq<0){stop("Glucose requirement cannot be negative")}
  if(Hi < 0 || Hd < 0 || Hc < 0 || Hn < 0){stop("Half-saturation constants cannot be negative")}
  if(D < 0 || C < 0 || N < 0){stop("Limiting factor values cannot be negative")}
  if(temp < -90 || temp > 60){warning("Specified temperature has not been recorded in nature on Earth")}
  
  fgross <- rep(NA,3) #Empty vector for intermediate outputs
  dtga <- rep(NA,3) #Empty vector for intermediate outputs
  wgauss <- c(0.2778, 0.4444, 0.2778) #Weights for Gaussian integration
  
  for (hr in 1:3){                              #radiation measured 3X day, roughly correlates to morning, noon, afternoon
    #LD 20230616 actually to near-noon, mid-afternoon, and late afternoon, assuming morning and afternoon light availability are symmetric.
    intSolarRad <- PAR[hr] #LD edit 20230616: removing self-shading from photosynthesis function and leaving it for attenuate function.
    # intSolarRad <- PAR[hr]*exp(-k*pbiomass)#from Charisma instructions--> tells you how much of the light a plant is going to get as PAR in microeinsteins based
    #on photosynthetic biomass
    #k: Light attenuation coefficient, Default at 0.02 m2/g. Rice  - 0.38 from Charkraborty etal. 2018.
    #print(c("intSolarRad",intSolarRad))
    
    #Allow several possible limiting factors
    #Monod function for light. Math from Charisma via GenVeg, which describes this as:
    #amount of light absorbed, per half-saturation constants from Charisma equ. 3. The Monod or Michaelis/Menten function is
    #adequate for describing the photosynthetic response to light (Carr et al. 1997). 
    #LD: Logic of "missing" is new here; sets light-monod equal to 1 (so not limiting growth) unless both PAR and Hi are specified. 
    #GenVeg NetLogo version cite Teh (2006) eqn 119 for light.monod.  
    light.monod <- ifelse(missing(Hi), 1, (intSolarRad / (intSolarRad + Hi)))
    #print(c("light.monod", light.monod))

    #Hill function for temperature, from Charisma via GenVeg
    temp.hill <- ifelse(missing(temp), 1, ((1.35 * temp^3) / (temp^3 + 14^3)))
    #Monod function for depth, from Charisma via GenVeg
    depth.monod <- ifelse(missing(D) | missing(Hd), 1, (D / (Hd + D)))
    #Monod function for carbonate, from GenVeg
    carbonate.monod <- ifelse(missing(C) | missing(Hc), 1, (C / (Hc + C)))
    #Monod function for a limiting nutrient, after GenVeg
    nutrient.monod <- ifelse(missing(N) | missing(Hn), 1, (N / (Hn + N)))
    
    #Calculate photosynthetic assimilation in grams of glucose
    P <- Pmax * light.monod * temp.hill * depth.monod * carbonate.monod * nutrient.monod
    
    fgross[hr] <- P        #calculates gross assimilation of fgross (like APT) via photosynthesis at specific hour calculate growth per day at three times per day, morning, middday, and evenning and this amount is weighted based on how much light is hitting hte plant based on the latitude of your study site
    dtga[hr] <- fgross[hr]*wgauss[hr]          #weights fgross for specific time of day
  }
  
  
  dtgaCollapsed <- sum(dtga)*pbiomass            #calculates total biomass (g carbohydrates) gained across plant (pbiomass is amount of leaves/green matter)
  
  assimilatedCH20 <- dtgaCollapsed*daylength  #Total biomass (g carbohydrates) for day length
  
  gphot <- assimilatedCH20*(30/44)            #converts carbohydrates to glucose where photosysnthesis unit is glucose and then we later convert that glucose to biomass
  
  gphotbm <- gphot/glucosereq #Convert glucose to biomass
  
  #Return gross biomass gain due to photosynthesis
  return(gphotbm)
  
} #End function call