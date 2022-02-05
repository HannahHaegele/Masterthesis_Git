kalman.states.wrapper <- function(parameters){
  
  out <- unpack.parameters(parameters)
  
  for (n in names(out)) {
    eval(parse(text=paste0(n, "<-out$", n)))
  }
  
  states <- kalman.states(x_prior, x_post, P_prior, P_post, F, Q, R,rho)
  return(states)
  
}
