
#' Check equilibrium state
#'
#' @param B the square regression matrix
#'
#'
equilibrium_check <- function(B){
  if(all(abs(Re(eigen(B)$values)) < 1)) print("Equilibirum state has been reached.")
}



#' Generate data from multivariate standard normal distribution
#'
#' @param B the square regression matrix
#' @param N the sample size (default = 1e6)
#' @param seed set the specific seed
#'
#' @return the matrix of data (N by variable numbers)
#'
gen_dat <- function(B, N = 1e6, seed = NULL){
  dimension <- ncol(B)
  # I - B inverse
  inverse <- solve(diag(dimension) - B)
  # sample errors (std. normal)
  errors <- MASS::mvrnorm(n = N, mu = rep(0, dimension), Sigma = diag(dimension))
  # generate data: (I_B)*errors
  data <- t(apply(errors, 1, function(x) inverse %*% x))
  colnames(data) <-  colnames(B)
  return(data)
}
