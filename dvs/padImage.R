repmat <- function(a, n, m) {
  kronecker(matrix(1, n, m), a)
}

# pads a matrix with its border values
padImage <- function(a, vpad, hpad = NULL) {
  if (is.null(hpad)) {
    hpad <- vpad
  }
  
  # Creating top and bottom padding, replicate the first and last rows respectively
  u <- repmat(a[1, , drop = FALSE], vpad, 1)
  b <- repmat(a[nrow(a), , drop = FALSE], vpad, 1)
  
  # Creating left and right padding, replicate the first and last columns respectively
  l <- repmat(a[, 1, drop = FALSE], 1, hpad)
  r <- repmat(a[, ncol(a), drop = FALSE], 1, hpad)
  
  # Creating corner padding, upper-left, upper-right, bottom-left, and bottom-right.
  ul <- repmat(a[1, 1], vpad, hpad)
  ur <- repmat(a[1, ncol(a)], vpad, hpad)
  bl <- repmat(a[nrow(a), 1], vpad, hpad)
  br <- repmat(a[nrow(a), ncol(a)], vpad, hpad)
  
  # Constructing the padded matrix by combining the original matrix and the padding
  top <- cbind(ul, u, ur)
  mid <- cbind(l, a, r)
  bottom <- cbind(bl, b, br)
  
  result <- rbind(top, mid, bottom)
  
  return(result)
}