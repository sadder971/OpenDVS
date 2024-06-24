# This function extracts the Red, Green, and Blue channels from an image.
# It also computes the intensity image (ii) which is the maximum value 
mygetrgb <- function(img) {
  r <- img[,,1]
  g <- img[,,2]
  b <- img[,,3]
  ii <- pmax(r, pmax(g, b))
  return(list(r = r, g = g, 
              b = b, ii = ii))
}