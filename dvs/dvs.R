for (package in c("png", "jpeg", "colorspace", 
                  "stringr", "imager", "OpenImageR", 
                  "scales", "gsignal", "RcppArmadillo", 
                  "Rcpp", "reticulate", "dplyr", "pracma")) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    user_choice <- menu(c("Yes", "No"),
                        title = paste("Do you want to install the package ", package, "?", sep = ""))
    if (user_choice == 1) {  
      install.packages(package)
      library(package, character.only = TRUE)
    } else {  
      message(paste("Skipping installation of", package))
    }
  }
}

dvs <- function(input) {
  
  # Check if input is a path (string) or an already loaded image (array)
  if (is.character(input)) {
    file_ext <- tolower(tools::file_ext(input))
    
    # Read image based on file extension
    if (file_ext == "png") {
      img <- readPNG(input)[,,1:3] # Assuming the PNG might have an alpha channel, which is discarded
    } 
    else if (file_ext == "jpg" || file_ext == "jpeg") {
      img <- readJPEG(input)
      
      # If the JPEG is grayscale, adjust its dimensions
      if (length(dim(img)) < 3) { # Check if the image does not have 3 dimensions
        img <- array(img, dim = c(dim(img)[1], dim(img)[2], 3)) # Duplicate the grayscale values across 3 channels
        for (i in 2:3) {
          img[,,i] <- img[,,1] # Copy the grayscale values to simulate RGB
        }
      }
    } else {
      stop("Unsupported file format. Please provide a PNG or JPEG file.")
    }
  } else if (is.array(input)) {
    # Input is already a loaded image, use it directly
    img <- input
  } else {
    stop("Input must be a file path (as character) or an already loaded image (as array).")
  }
  
  
  # Compute Itti-Koch saliency map
  ikout <- ittikochmap(img)
  ikmap <- ikout$master_map_resized
  
  # Compute text saliency
  S <- textSaliency(input)
  
  w <- 2
  # Make linear combinations (with text saliency given twice the weight)
  comb <- (as.matrix(S) * w + as.matrix(ikmap)) / (w + 1) # Weighted average to keep scaling from 0 to 1
  
  # Update the ikout object
  ikout$master_map_resized <- comb  # Update the combined map
  
  ikout$map_types <- c(ikout$map_types, "text") # Update map type array
  
  ikout$top_level_feat_maps <- c(ikout$top_level_feat_maps, list(S)) # Include text saliency map
  
  return(ikout)
}