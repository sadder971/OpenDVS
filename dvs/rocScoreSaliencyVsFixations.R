
makeFixationMask <- function(X, Y, origImgSize, salMapSize) {
  # Check if lengths of X and Y are equal
  if (length(X) != length(Y)) {
    stop("makeFixationMask Error: number of X and Y coordinates should be the same!")
  }
  
  # Calculate scale and apply it
  scale <- salMapSize[1] / origImgSize[1]
  X <- floor(X * scale + 0.5)
  
  Y <- floor(Y * scale + 0.5)
  
  
  # Clamp values to within the allowable range
  X <- pmin(pmax(X, 1), salMapSize[2])
  Y <- pmin(pmax(Y, 1), salMapSize[1])
  
  # Create an empty mask
  mask <- matrix(0, nrow = salMapSize[1], ncol = salMapSize[2])
  
  # Accumulate fixations in the mask
  for (i in seq_along(X)) {
    mask[Y[i], X[i]] <- mask[Y[i], X[i]] + 1
  }
  # write.csv(mask, file = "fixationMask.csv", row.names = FALSE)
  return(mask)
  
}


getIntelligentThresholds <- function(vals) {
  # Find unique values
  uniqueVals <- unique(vals)
  
  threshs <- sort(uniqueVals)
  
  return(threshs)
}


library(dplyr)

getBestRows <- function(p) {
  df <- as.data.frame(p)

  pnew <- df %>%
    group_by(V2) %>%
    slice(which.max(V1)) %>%
    ungroup()
  
  pnew_matrix <- as.matrix(pnew)
  return(pnew_matrix)
}


library(pracma)
areaROC <- function(p) {
  
  p <- getBestRows(p)
  
  # Sort rows based on the second column (FPR) and then the first column (TPR)
  xy <- p[order(p[,2], p[,1]),]
  
  # Prepend (0,0) and append (1,1) to the x and y vectors
  x <- c(0, xy[,2], 1)
  y <- c(0, xy[,1], 1)
  
  # Calculate the area under the curve using trapezoidal rule
  A <- trapz(x, y)
  
  return(A)
}


rocSal <- function(salmap, mask) {
  
  salmap <- rescale(as.matrix(salmap), from = c(min(salmap), max(salmap)))
  
  if (mode(salmap) == "numeric") {
    # Scale the values by 50 and convert to integers
    salmap <- floor(salmap * 50 + 0.5)
    # print(salmap)
  } 
  # Get intelligent thresholds
  t <- getIntelligentThresholds(as.vector(salmap))
  
  Nt <- length(t)
  p <- matrix(0, nrow = Nt, ncol = 2)
  
  # Count true and false points
  Ntrues <- sum(mask)
  # print(Ntrues)
  Nfalses <- sum(mask == 0)
  # print(Nfalses)
  if (Nfalses == 0) Nfalses <- 1e-6
  
  for (ti in 1:Nt) {
    Ti <- t[ti]
    
    shouldbefix <- salmap >= Ti
    
    TPm <- mask * shouldbefix
    # print(TPm)
    TP <- sum(TPm)
    # print(TP)
    tpr <- TP / Ntrues
    
    FPm <- (mask == 0) * shouldbefix
    FP <- sum(FPm)
    # print(FP)
    fpr <- FP / Nfalses
    
    p[ti, ] <- c(tpr, fpr)
    
  }
  # print(p)
  a <- areaROC(p)
  return(a)
}


rocScoreSaliencyVsFixations <- function(salmap, X, Y, origImgSize) {
  
  # Calculate the saliency map size (it's assumed salmap is already a matrix)
  salMapSize <- dim(salmap)
  
  # Create the fixation mask
  mask <- makeFixationMask(X, Y, origImgSize, salMapSize)
  
  # Calculate the ROC score
  a <- rocSal(salmap, mask)
  
  return(round(a, 2))
}
