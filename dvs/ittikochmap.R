library(jpeg)
library(grid)
library(colorspace)
library(stringr)
library(imager)
library(OpenImageR)
library(scales)
library(gsignal)

ittikochmap <- function(img, channels = c("L", "I", "O")) {
  
  params <- makeIttiParams()
  params$useIttiKochInsteadOfGBVS <- 1
  params$channels <- channels
  params$verbose <- 0
  params$unCenterBias <- 0
  
  # Uncomment the line below (ittiDeltaLevels <- c(2, 3)) for more faithful implementation
  # Known to give suboptimal results for small images, i.e., < 640 in height or width
  # params$ittiDeltaLevels <- c(2, 3)
  
  if (is.character(img)) {
    file_ext <- tolower(tools::file_ext(img))
    
    # Read image based on file extension
    if (file_ext == "png") {
      img <- readPNG(img)[,,1:3] 
    } 
    else if (file_ext == "jpg" || file_ext == "jpeg") {
      img <- readJPEG(img)
    } 
  }
  
  if (is(img, "integer")) {
    img <- as.double(img) / 255
  }
  
  params$salmapmaxsize <- round(max(dim(img)) / 8)
  
  result <- Itti(img, params)
  
  return(result)
}