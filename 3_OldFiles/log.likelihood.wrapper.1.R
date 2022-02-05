log.likelihood.wrapper.1 <- function(parameters){
  
  out <- unpack.parameters.1(parameters)
  
  for (n in names(out)) {
    eval(parse(text=paste0(n, "<-out$", n)))
  }
  
  return(kalman.log.likelihood.1(x_prior, x_post, P_prior, P_post, F, Q, R,rho))
  
}

