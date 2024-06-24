library(Rcpp)
library(RcppArmadillo)
sourceCpp("subsample.cpp")

initialize_subsample <- function(img) {
  # Check if the image is a color image 
  is_color <- dim(img)[3] == 3
  
  # If it is a color image, extract the RGB channels and the intensity image
  if (is_color) {
    channels <- mygetrgb(img)
    imgr <- channels$r
    imgg <- channels$g
    imgb <- channels$b
    imgi <- channels$ii
  } 
  
  else{
    imgi <- img
  }
  # Storing the first subsampled R, G, and B channels.
  imgL <- list()
  imgR <- list()
  imgG <- list()
  imgB <- list()
  
  imgL[[1]] <- Subsample(imgi)
  
  if (is_color){
    imgR[[1]] <- Subsample(imgr)
    imgG[[1]] <- Subsample(imgg)
    imgB[[1]] <- Subsample(imgb)
  } 
  else {
    imgR[[1]] <- NULL
    imgG[[1]] <- NULL
    imgB[[1]] <- NULL
  }
  
  return(list(imgL = imgL, imgR = imgR, imgG = imgG, 
              imgB = imgB, is_color = is_color))
}

generate_pyramids <- function(channel_data, max_levels) {
  for (i in 2:max_levels) {
    channel_data$imgL[[i]] <- Subsample(channel_data$imgL[[i - 1]])
    if (channel_data$is_color) {
      channel_data$imgR[[i]] <- Subsample(channel_data$imgR[[i - 1]])
      channel_data$imgG[[i]] <- Subsample(channel_data$imgG[[i - 1]])
      channel_data$imgB[[i]] <- Subsample(channel_data$imgB[[i - 1]])
    }
    
    if (dim(channel_data$imgL[[i]])[1] < 3 || 
        dim(channel_data$imgL[[i]])[2] < 3) {
      cat(sprintf("Reached minimum size at level = %d. Cutting off additional levels\n", i))
      return(list(channel_data = channel_data, levels = 2:i))
    }
  }
  return(list(channel_data = channel_data, levels = 2:max_levels))
}


compute_feature_maps <- function(channel_data, param, levels) {
  rawfeatmaps <- list()
  channel_files <- c("I_intensity", "L_labcolor", "O_orientation")
  
  for (ci in seq_along(channel_files)) {
    
    channelLetter <- str_extract(channel_files[ci], "^[A-Z]")
    channelName <- str_extract(channel_files[ci], "(?<=_).*$")
    func_to_call <- get(paste0(channelLetter, "_", channelName))
  
    useChannel <- channelLetter %in% param$channels
    
    if ((channelLetter %in% c("C", "D", "L")) && 
        useChannel && !channel_data$is_color){
      message(sprintf("Skipping channel: %s", channelName))
      next
    }
    else if (useChannel) {
      rawfeatmaps[[channelName]] <- 
        compute_single_featuremap(func_to_call, param,
                                  channel_data, levels, channelName)
    }
      
  }
  return(rawfeatmaps)
}

compute_single_featuremap <- function(func_to_call, param, 
                                      channel_data, levels, channelName) {
  obj <- list()
  
  obj$info <- list(
    weight = func_to_call(param)[["weight"]],
    numtypes = func_to_call(param)[["numtypes"]],
    descriptions = func_to_call(param)[["descriptions"]])
  
  obj$description <- channelName
  obj$maps <- list(val = vector("list", length(levels)))
  obj$maps$origval <- list()
  
  for (ti in 1:obj$info$numtypes) {
    obj$maps$val[[ti]] <- vector("list", length(levels))
    obj$maps$origval[[ti]] <- list()

    for (lev in levels) {
      map <- do.call(func_to_call, list(param, channel_data$imgL[[lev]], 
                                        channel_data$imgR[[lev]], 
                                        channel_data$imgG[[lev]], 
                                        channel_data$imgB[[lev]], ti))
      obj$maps$origval[[ti]][[lev]] <- map$map
      img_array <- array(map$map, dim = c(nrow(map$map), ncol(map$map), 1, 1))
      resized_img <- imager::resize(img_array, size_x = param$salmapsize[1], 
                                    size_y = param$salmapsize[2], 
                                    interpolation_type = 5)
      obj$maps$val[[ti]][[lev]] <- as.matrix(resized_img[, , 1, 1])
    }
  }
  return(obj)
}

getFeatureMaps <- function(img, param) {
  channel_data <- initialize_subsample(img)
  pyramid_data <- generate_pyramids(channel_data, param$maxcomputelevel)
  
  # for (i in 1:5) {
    # img_matrix <- as.matrix(pyramid_data$channel_data$imgL[[i]])
    # file_name <- sprintf("example_level%d.csv", i)
    # write.csv(img_matrix, file_name, row.names = FALSE)
  # }
  
  rawfeatmaps <- compute_feature_maps(pyramid_data$channel_data, 
                                      param, pyramid_data$levels)
  return(rawfeatmaps)
}

