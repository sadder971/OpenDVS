I_intensity <- function(fparam, img = NULL, imgR = NULL, 
                        imgG = NULL, imgB = NULL, typeidx = NULL) {
  
  if (missing(img) & missing(imgR) & 
      missing(imgG) & missing(imgB) & 
      missing(typeidx)) {
    out <- list()
    out <- list()
    out$weight <- fparam$intensityWeight
    out$numtypes <- 1
    out$descriptions <- c("Intensity")
    return(out)
  } 
  
  else {
    out <- list()
    out$map <- img
    return(out)
  }
}