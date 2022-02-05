gradient <- function(f, x, delta = x * 0 + 1.0e-5) {
  ##########################################################################################
  ## This function computes the gradient of a function f given a vector input x.
  ##########################################################################################   
  g <- x * 0
  for (i in 1:length(x)) {
    x1 <- x
    x1[i] <- x1[i] + delta[i]
    f1 <- f(x1)
    x2 <- x
    x2[i] <- x2[i] - delta[i]
    f2 <- f(x2)
    g[i] <- (f1 - f2) / delta[i] / 2
  }
  return(g)
}

