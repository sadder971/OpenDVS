O_orientation <- function(fparam, img = NULL, imgR = NULL, 
                          imgG = NULL, imgB = NULL, typeidx = NULL) {
  
  if (missing(img) & missing(imgR) & missing(imgG) & 
      missing(imgB) & missing(typeidx)) {
    out <- list(weight = fparam$orientationWeight, 
                numtypes = length(fparam$gaborFilters))
    
    out$descriptions <- vector("list", length(fparam$gaborFilters))
    
    for (i in 1:length(fparam$gaborFilters)) {
      out$descriptions[[i]] <- sprintf("Gabor Orientation %g", 
                                       fparam$gaborangles[i])
    }
  } 
  else {
    gaborFilters <- fparam$gaborFilters
    
    j <- typeidx
    # Convolving the image with the two Gabor filters
    f0 <- myconv2(img, gaborFilters[[j]]$g0)
    f90 <- myconv2(img, gaborFilters[[j]]$g90)
    
    # Combining the absolute values of the two filters to get the final orientation map
    out <- list(map = abs(f0) + abs(f90))
    
    # Attenuating the borders of the orientation map to reduce boundary artifacts
    out$map <- attenuateBordersItti(out$map, 13)
  }
  return(out)
}