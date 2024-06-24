initItti<- function(param, imgsize) {
  # cat("initializing....\n")
  
  if (min(param$levels) < 2) {
    cat("oops. cannot use level 1.. trimming levels used\n")
    param$levels <- param$levels[param$levels > 1]
  }
  
  if (param$useIttiKochInsteadOfGBVS) {
    param$activationType <- 2
    param$normalizationType <- 3
    param$normalizeTopChannelMaps <- 1
  }
  
  param$maxcomputelevel <- max(param$levels)
  if (param$activationType == 2) {
    param$maxcomputelevel <- max(param$maxcomputelevel, 
                                 max(param$ittiCenterLevels) + 
                                   max(param$ittiDeltaLevels))
  }
  
  h <- imgsize[1]
  w <- imgsize[2]
  
  
  scale <- param$salmapmaxsize / max(w, h)
  salmapsize <- round(c(h, w) * scale)
  
  grframe <- NULL
  
  # gabor filters
  gaborParams <- list(stddev = 2, elongation = 2, 
                      filterSize = -1, filterPeriod = pi)
  
  gaborFilters <- list()
  
  for (i in 1:length(param$gaborangles)){
    
    theta <- param$gaborangles[i]
    
    gaborFilters[[i]] <- list()
    gaborFilters[[i]]$g0 <- makeGaborFilterItti(gaborParams, theta, 0)
    gaborFilters[[i]]$g90 <- makeGaborFilterItti(gaborParams, theta, 90)
  }
  
  param$gaborParams <- gaborParams
  param$gaborFilters <- gaborFilters
  param$salmapsize <- salmapsize
  param$origimgsize <- imgsize
  
  return(list(grframe = grframe, param = param))
}