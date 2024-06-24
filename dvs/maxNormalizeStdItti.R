# sourceCpp("LocalMaximaGBVS.cpp")

# It normalizes the input by adjusting its values based on local maxima
maxNormalizeStdItti<- function(data) {
  
  M <- 10
  
  data <- data * M
  
  thresh <- M / 10
  
  # Computing the local maxima
  #lm_values <- rcppLocalMaximaGBVS(data, thresh)
  lm_values <- mexLocalMaximaItti(data, thresh)
  lm_avg <- lm_values$lm_avg
  lm_num <- lm_values$lm_num
  
  # If there is more than one local maximum, adjust the data based on the average
  if (lm_num > 1) {
    result <- data * (M - lm_avg)^2
  } 
  
  # If there is only one local maximum, adjust the data by multiplying M square
  else if (lm_num == 1) {
    result <- data * M^2
  } 
  else{
    result <- data
  }
  return(result)
}