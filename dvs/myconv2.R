# This function performs a 2D convolution between a and b
myconv2 <- function(a, b) {
  
  # Calculating the rows and columns required for padding
  vpad <- ceiling((dim(b)[1] - 1) / 2)
  hpad <- ceiling((dim(b)[2] - 1) / 2)
  
  ap <- padImage(a, vpad, hpad)
  
  # Computing the 2d convolution of two matrices
  # and return the central part of the convolution with the same size as the padded matrix
  cp <- conv2(ap, b, shape = "same")
  
  start_row <- vpad + 1
  end_row <- start_row + dim(a)[1] - 1
  start_col <- hpad + 1
  end_col <- start_col + dim(a)[2] - 1
  
  # Extracting the central portion of the convolution result
  c <- cp[start_row:end_row, start_col:end_col]
  
  return(c)
}