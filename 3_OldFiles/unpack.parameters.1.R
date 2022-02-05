unpack.parameters.1 <- function(parameters) {
  
  rho <- parameters[1]
  
  F <- matrix(0, 4, 4)
  F[1, 1] <- rho
  F[2, 2] <- F[3, 3] <- F[4, 4] <- 1
  
  R <- parameters[3]

  Q <- covariances_0
  Q[1,] <- c(parameters[2],0,0,0)
  Q[,1] <- c(parameters[2],0,0,0)
  
  x_prior <- matrix(NA, nrow = 4, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 4, ncol = (nrow(data1)+1))
  x_post[,1] <- c(0,coefficients_0)
  
  P_prior <- array(0, dim = c(4,4,nrow(data1)))
  P_post <- array(0, dim = c(4,4,nrow(data1)+1))
  #P_post[,,1] <- matrix(0, nrow = 4, ncol = 4)
  P_post[,,1] <- Q
  
  return(list("F"=F, "Q"=Q, "R"=R, "rho"=rho, "x_prior"=x_prior, "x_post"=x_post, "P_prior"=P_prior, "P_post"=P_post))
}
