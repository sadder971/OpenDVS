makeGaborFilterItti <- function(gaborParams, angle, phase) {
  
  major_stddev <- gaborParams$stddev
  minor_stddev <- major_stddev * gaborParams$elongation
  max_stddev <- max(major_stddev, minor_stddev)
  
  sz <- gaborParams$filterSize
  if (sz == -1) {
    sz <- ceiling(max_stddev * sqrt(10))
  } else {
    sz <- floor(sz / 2)
  }
  
  # Converting the angle and phase to radians
  rtDeg <- angle * pi / 180
  psi <- phase * pi / 180
  
  omega <- 2 * pi / gaborParams$filterPeriod
  co <- cos(rtDeg)
  si <- -sin(rtDeg)
  major_sigq <- 2 * major_stddev^2
  minor_sigq <- 2 * minor_stddev^2
  
  vec <- -sz:sz
  vlen <- length(vec)
  vco <- vec * co
  vsi <- vec * si
  
  major <- outer(vco, vsi, `+`)
  major2 <- major^2
  
  minor <- outer(vsi, vco, `-`)
  minor2 <- minor^2
  
  # Computing the Gabor filter values
  result <- cos(omega * major + psi) * 
    exp(-major2 / major_sigq - minor2 / minor_sigq)
  
  # Normalizing the filter values
  filter <- result - mean(result)
  filter <- filter / sqrt(sum(filter^2))
  
  return(filter)
}