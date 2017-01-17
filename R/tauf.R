# See A. B. OWEN and P. A. ROEDIGER, The Sign of the Logistic Regression Coefficient,
# The American Statistician, November 2014, Vol. 68, No. 4, pp 297 - 301

tauf <- function(x, y, response=1) {
  if(response == 0){
    y <- 1-y
  }
  st <- x
  i1 <- which(y == 1)
  i0 <- which(y == 0)
  r1 <- r0 <- n <- rep(1, length(x))
  r1[i0] <- 0
  r0[i1] <- 0
  nt <- sum(n)
  n1 <- sum(r1)
  n0 <- sum(r0)

  tau5 <- n0 * n1 * (weighted.mean(st, r1) - weighted.mean(st, r0)) / (n0 + n1)
  xx <- list(tau5, n1/nt)
  names(xx) <- c("tau", "p1")
  return(xx)
}
