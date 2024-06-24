
###  Return a list of default parameters for GBVS
makeIttiParams <- function() {
  
  p <- list()
  
  ########## General #######################
  
  p$salmapmaxsize <- 48
  p$verbose <- 0
  p$verboseout <- "screen"
  p$saveInputImage <- 0
  p$blurfrac <- 0.02
  
  ########## Feature channel parameters #######################
  
  p$channels <- c("D", "I", "o")
  p$colorWeight <- 1
  p$intensityWeight <- 1
  p$orientationWeight <- 1
  p$contrastWeight <- 1
  p$flickerWeight <- 1
  p$motionWeight <- 1
  p$dklcolorWeight <- 1
  p$labcolorWeight <- 1
  
  p$gaborangles <- c(0, 45, 90, 135)
  p$contrastwidth <- 0.1
  p$flickerNewFrameWt <- 1
  p$motionAngles <- c(0, 45, 90, 135)
  
  ########## GBVS parameters #######################
  
  p$unCenterBias <- 0
  p$levels <- c(2, 3, 4)
  p$multilevels <- list()
  p$sigma_frac_act <- 0.15
  p$sigma_frac_norm <- 0.06
  p$num_norm_iters <- 1
  p$tol <- 0.0001
  p$cyclic_type <- 2
  
  ########## Itti/Koch and Simpler Saliency Algorithm parameters #######################
  
  p$useIttiKochInsteadOfGBVS <- 0
  p$activationType <- 1
  p$normalizationType <- 1
  p$normalizeTopChannelMaps <- 0
  p$ittiCenterLevels <- c(2, 3)
  p$ittiDeltaLevels <- 2
  p$ittiblurfrac <- 0.03
  
  return(p)
}