# Creates a 1D Gaussian kernel based on the standard deviation and the number of standard deviations
mygausskernel <- function(std, nstds) {
  maxi <- round(std * nstds)
  a <- c(dnorm(0, mean = 0, sd = std), 
         numeric(maxi))
  
  for (i in 1:maxi) {
    a[i+1] <- dnorm(i, mean = 0, sd = std)
  }
  
  k <- c(rev(a[-1]), a)
  k <- k / sum(k)
  
  return(k)
}