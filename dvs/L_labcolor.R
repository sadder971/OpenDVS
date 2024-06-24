L_labcolor <- function(fparam, img = NULL, imgR = NULL, 
                       imgG = NULL, imgB = NULL, typeidx = NULL) {
  if (missing(img) & missing(imgR) & missing(imgG) & 
      missing(imgB) & missing(typeidx)) {
    out <- list()
    out$weight <- fparam$labcolorWeight
    out$numtypes <- 3
    out$descriptions <- c("LAB lightness Channel", 
                          "LAB Color Channel 1", 
                          "LAB Color Channel 2")
    return(out)
  } 
  else{
    lab_matrix <- as(sRGB(R = as.vector(imgR), G = as.vector(imgG), 
                          B = as.vector(imgB)), "LAB")
    
    l_channel <- matrix(coords(lab_matrix)[,1], 
                        nrow = dim(img)[1], 
                        ncol = dim(img)[2])
    a_channel <- matrix(coords(lab_matrix)[,2], 
                        nrow = dim(img)[1], 
                        ncol = dim(img)[2])
    b_channel <- matrix(coords(lab_matrix)[,3], 
                        nrow = dim(img)[1], 
                        ncol = dim(img)[2])
    
    out <- list()
    if (typeidx == 1){
      out$map <- l_channel
    } 
    else if (typeidx == 2){
      out$map <- a_channel
    } 
    else if (typeidx == 3){
      out$map <- b_channel
    }
    return(out)
  }
}