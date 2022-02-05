log.likelihood.wrapper.2 <- function(parameters){
  
  out <- unpack.parameters.2(parameters)
  
  for (n in names(out)) {
    eval(parse(text=paste0(n, "<-out$", n)))
  }
  
  return(kalman.log.likelihood.2(x_prior, x_post, P_prior, P_post, F, Q, R,rho))
  
}

