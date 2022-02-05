unpack.parameters.1b <- function(parameters) {
  
  rho <- parameters[1]
  
  F <- matrix(0, 4, 4)
  F[1, 1] <- F[2, 2] <- F[3, 3] <- F[4, 4] <- 1
  
  R <- parameters[3]
  
  #Q <- covariances_0
  Q <- matrix(0,4,4)
  Q[2, 2] <- initial.parameters[4]
  Q[3, 3] <- initial.parameters[5]
  Q[4, 4] <- initial.parameters[6]
  Q[1,] <- c(initial.parameters[2]/s,0,0,0)
  Q[,1] <- c(initial.parameters[2]/s,0,0,0)


  x_prior <- matrix(NA, nrow = 4, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 4, ncol = (nrow(data1)+1))
  x_post[,1] <- c(6,coefficients_0)
  
  P_prior <- array(0, dim = c(4,4,nrow(data1)))
  P_post <- array(0, dim = c(4,4,nrow(data1)+1))
  P_post[,,1] <- matrix(0, nrow = 4, ncol = 4)
  #P_post[,,1] <- Q
  
  return(list("F"=F, "Q"=Q, "R"=R, "rho"=rho, "x_prior"=x_prior, "x_post"=x_post, "P_prior"=P_prior, "P_post"=P_post))
}