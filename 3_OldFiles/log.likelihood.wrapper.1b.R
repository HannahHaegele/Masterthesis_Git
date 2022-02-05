log.likelihood.wrapper.1b <- function(parameters){
  
  out <- unpack.parameters.1b(parameters)
  
  for (n in names(out)) {
    eval(parse(text=paste0(n, "<-out$", n)))
  }
  
  return(kalman.log.likelihood.1b(x_prior, x_post, P_prior, P_post, F, Q, R,rho))
  
}

