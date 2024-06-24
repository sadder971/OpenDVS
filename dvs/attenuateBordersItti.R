# linearly attenuates a border region of borderSize
# on all sides of the 2d data array
attenuateBordersItti <- function(data, borderSize) {
  
  result <- data
  dsz <- dim(data)
  
  # Ensure the borderSize doesn't exceed half the height of the data
  if (borderSize * 2 > dsz[1]) {
    borderSize <- floor(dsz[1] / 2) 
  }
    
  if (borderSize * 2 > dsz[2]) {
    borderSize <- floor(dsz[2] / 2) 
  }
 
  
  if (borderSize < 1) return(data) 
  
  bs <- 1:borderSize
  coeffs <- bs / (borderSize + 1)
  
  # Attenuating the top and bottom borders
  for (i in bs){
    result[i, ] <- result[i, ] * coeffs[i]
    result[dsz[1] - i + 1, ] <- result[dsz[1] - i + 1, ] * coeffs[i]
  }
  
  # Attenuating the left and right borders
  for (i in bs){
    result[, i] <- result[, i] * coeffs[i]
    result[, dsz[2] - i + 1] <- result[, dsz[2] - i + 1] * coeffs[i]
  }
  return(result)
}