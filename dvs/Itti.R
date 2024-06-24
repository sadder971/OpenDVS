Itti <- function(img, param = NULL) {
  
  # Read the image, if it is a file path
  if (is.character(img)) {
    img <- readJPEG(img)
  }
  
  # Ensure the image is large enough
  if (dim(img)[1] < 128 || dim(img)[2] < 128) {
    stop("GBVS Error: gbvs() meant to be used with images >= 128x128")
  }
  
  # Set default GBVS parameters if not provided
  if (is.null(param)) {
    param <- makeIttiParams()
  }
  
  # Initialize GBVS parameters
  result <- initItti(param, dim(img))
  
  param <- result$param
  
  grframe <- result$grframe
  
  
  if (param$useIttiKochInsteadOfGBVS) {
    # message("NOTE: Computing STANDARD Itti/Koch instead of Graph-Based Visual Saliency (GBVS)")
  }
  
  ### STEP 1 : compute raw feature maps from image  
  rawfeatmaps <- getFeatureMaps(img, param)
  
  ### STEP 2 : compute activation maps from feature maps 
  mapnames <- names(rawfeatmaps)
  mapweights <- numeric(length(mapnames))
  map_types <- character(length(mapnames))
  allmaps <- list()
  i <- 0
  
  # cat("computing activation maps...\n")
  
  for (fmapi in seq_along(mapnames)) {
    mapsobj <- rawfeatmaps[[mapnames[fmapi]]]
    
    numtypes <- mapsobj$info$numtypes
    mapweights[fmapi] <- mapsobj$info$weight
    map_types[fmapi] <- mapsobj$description
    
    
    for (typei in 1:numtypes) {
      for (centerLevel in param$ittiCenterLevels) {
        for (deltaLevel in param$ittiDeltaLevels) {
          # cat(sprintf("making an itti-style activation (%s) feature map using center-surround subtraction.\n", mapnames[fmapi]))
          i <- i + 1
          # Extracting the center imag          
          center_ <- mapsobj$maps$origval[[typei]][[centerLevel]]
          sz_ <- dim(center_)
          
          new_img <- as.cimg(mapsobj$maps$origval[[typei]][[centerLevel + deltaLevel]])
          resized_img <- imager::resize(new_img, size_x = sz_[1],
                                        size_y = sz_[2], interpolation_type = 5)
          
          surround_ <- as.matrix(resized_img)
          # Computing the activation map using center-surround subtraction         
          feature_map <- list(map = (center_ - surround_)^2, 
                              maptype = c(fmapi, centerLevel, deltaLevel))

          # Append the feature map to the list of all feature maps
          allmaps[[length(allmaps) + 1]] <- feature_map
        }
      }
    }
  }

  #for (i in 1:length(allmaps)) {
    #write.csv(allmaps[[i]]$map, file = paste0("Rallmaps__testing_", i, ".csv"), row.names = FALSE, col.names = FALSE)
  #}

  
  #### STEP 3 : normalize activation maps
  
  # cat("normalizing activation maps...\n")
  norm_maps <- list()
  
  for (i in 1:length(allmaps)) {
    # cat(sprintf("normalizing a feature map (%d)... ", i))
    
    if (param$normalizationType == 3) {
      # cat("using global - mean local maxima scheme.\n")
      
      new_img <- as.cimg(allmaps[[i]]$map)
      resized_img <- imager::resize(new_img, size_x = param$salmapsize[1],
                                    size_y = param$salmapsize[2], 
                                    interpolation_type = 5)
      norm_maps[[i]] <- list(map = NULL, maptype = NULL)
      norm_maps[[i]]$map <- maxNormalizeStdItti(rescale(as.matrix(resized_img), 
                                                        from = c(min(as.matrix(resized_img)), 
                                                                 max(as.matrix(resized_img))), 
                                                        to = c(0, 1)))
    }
    norm_maps[[i]]$maptype <- allmaps[[i]]$maptype
  }
  
  
  ### STEP 4 : average across maps within each feature channel
  # cat("summing across maps within each feature channel.\n")
  
  # Initialization
  mapnames <- names(rawfeatmaps)
  comb_norm_maps <- vector("list", length(mapnames))
  cmaps <- vector("list", length(mapnames))
  Nfmap <- vector("list", length(mapnames))
  
  for (i in 1:length(mapnames)) {
    cmaps[[i]] <- 0
    Nfmap[[i]] <- 0
  }
  
  # Summing across maps within each feature channel
  for (j in 1:length(norm_maps)) {
    map <- norm_maps[[j]]$map
    fmapi <- norm_maps[[j]]$maptype[1]
    Nfmap[[fmapi]] <- Nfmap[[fmapi]] + 1
    cmaps[[fmapi]] <- cmaps[[fmapi]] + map
  }
  
  for (fmapi in 1:length(mapnames)) {
    if (param$normalizeTopChannelMaps && param$normalizationType == 3)  {
      # cat("Performing additional top-level feature map normalization using global - mean local maxima scheme.\n")
      
      # Normalizing the cmap using the maxNormalizeStdGBVS function
      cmaps[[fmapi]] <- maxNormalizeStdItti(rescale(cmaps[[fmapi]], 
                                                    from = c(min(cmaps[[fmapi]]), 
                                                             max(cmaps[[fmapi]])), 
                                                    to = c(0, 1)))
    }
    comb_norm_maps[[fmapi]] <- cmaps[[fmapi]]
  }
  
  #for (i in 1:length(mapnames)) {
    #write.csv(cmaps[[i]], file = paste0("Rcmaps_", i, ".csv"), row.names = FALSE)
  #}
  
  ### STEP 5 : sum across feature channels
  # cat("summing across feature channels into master saliency map.\n")
  
  master_idx <- length(mapnames) + 1
  comb_norm_maps[[master_idx]] <- 0
  
  # Loop through each feature channel and sum its contribution to the master saliency map
  for (fmapi in 1:length(mapnames)) {
    # cat(sprintf("adding in %s map with weight %0.3g (max = %0.3g)\n", 
                # map_types[fmapi], 
                # mapweights[fmapi], 
                # max(cmaps[[fmapi]])))
    
    # Add the weighted feature map to the master saliency map
    comb_norm_maps[[master_idx]] <- comb_norm_maps[[master_idx]] + 
      cmaps[[fmapi]] * mapweights[fmapi]
  }
  # Extracting the master saliency map
  master_map <- comb_norm_maps[[master_idx]]
  # Modify the borders of the master map to reduce artifacts
  master_map <- attenuateBordersItti(master_map,  4)
  master_map <- rescale(master_map, 
                        from = c(min(master_map), max(master_map)), 
                        to = c(0, 1))
  
  
  ## STEP 6: blur for better results
  
  blurfrac <- param$blurfrac
  if (param$useIttiKochInsteadOfGBVS) {
    blurfrac <- param$ittiblurfrac
  }
  
  if (blurfrac > 0) {
    # cat(sprintf("applying final blur with width = %0.3g\n", blurfrac))
    
    #Generating a Gaussian kernel based on the size of the master map and the blur fraction
    k <- mygausskernel(max(dim(master_map)) * blurfrac, 2)
    k <- matrix(k, nrow = 1)
    
    # Convolve the master map with the Gaussian kernel twice
    master_map <- myconv2(myconv2(master_map, k), t(k))
    master_map <- rescale(master_map, 
                          from = c(min(master_map), 
                                   max(master_map)), 
                          to = c(0, 1))
    
  }
  
  
  # Save descriptive, rescaled (0-255) output for user
  
  feat_maps <- list()
  for (i in seq_along(mapnames)) {
    feat_maps[[i]] <- rescale(comb_norm_maps[[i]], 
                              from = c(min(comb_norm_maps[[i]]), 
                                       max(comb_norm_maps[[i]])),
                              to = c(0, 1))
  }
  
  intermed_maps <- list()
  for (i in seq_along(allmaps)) {
    allmaps[[i]]$map <- rescale(allmaps[[i]]$map, 
                                from = c(min(allmaps[[i]]$map), 
                                         max(allmaps[[i]]$map)), 
                                to = c(0, 1))
    
    norm_maps[[i]]$map <- rescale(norm_maps[[i]]$map, 
                                  from = c(min(norm_maps[[i]]$map), 
                                           max(norm_maps[[i]]$map)), 
                                  to = c(0, 1))
  }
  
  intermed_maps$featureActivationMaps <- allmaps
  intermed_maps$normalizedActivationMaps <- norm_maps
  
  reiszed_map <- imager::resize(as.cimg(master_map), 
                                size_x = dim(img)[1], 
                                size_y = dim(img)[2], 
                                interpolation_type = 5)
  
  master_map_resized <- rescale(reiszed_map, 
                                from = c(min(reiszed_map), 
                                         max(reiszed_map)),
                                to = c(0, 1))
  
  out <- list()
  out$master_map <- master_map
  out$master_map_resized <- master_map_resized
  out$top_level_feat_maps <- feat_maps
  out$map_types <- map_types
  out$intermed_maps <- intermed_maps
  out$rawfeatmaps <- rawfeatmaps
  out$paramsUsed <- param
  
  if (param$saveInputImage) {
    out$inputimg <- img
  }
  
  return(out)
}