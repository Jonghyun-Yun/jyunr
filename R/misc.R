## cartesian to polar coordinates
cart2pol <- function(x, y) {
  theta = atan2(x, y)
  r = sqrt(x^2 + y^2)
  return(list(theta = theta, r = r))
}
