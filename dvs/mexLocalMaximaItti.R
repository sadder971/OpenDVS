# Retrieving the value at the x and y coordinate in an image
getVal <- function(img, x, y) {
  return(img[y, x]) 
}

# Computing the local maxima of an image
mexLocalMaximaItti <- function(img, thresh) {
  h <- dim(img)[1]
  w <- dim(img)[2]
  
  lm_sum <- 0
  lm_num <- 0
  
  # Start loop the image from the second row and column to the second last row and column.
  # This ensures we don't go out of bounds when checking the neighbors
  for (j in 2:(h - 1)) { 
    for (i in 2:(w  - 1)) { 
      val <- getVal(img, i, j)
      if (val >= thresh &&
          val >= getVal(img, i - 1, j) &&
          val >= getVal(img, i + 1, j) &&
          val >= getVal(img, i, j + 1) &&
          val >= getVal(img, i, j - 1)) {
        
        lm_sum <- lm_sum + val
        
        lm_num <- lm_num + 1
      }
    }
  }
  
  # Computing the average value of the local maxima
  lm_avg <- ifelse(lm_sum > 0, lm_sum / lm_num, 0)
  
  return(list(lm_avg = lm_avg, lm_num = lm_num, 
              lm_sum = lm_sum))
}