unpack.parameters.2 <- function(parameters) {
  
  F <- matrix(0, 5, 5)
  F[1, 1] <- F[2, 1] <- F[3, 3] <- F[4, 4] <- F[5, 5]<- 1
  
  rho <- parameters[1]
  
  R <- diag(c(parameters[2], parameters[3]))
  
  Q <- matrix(NA, nrow = 5, ncol =5)
  Q[3:5,3:5] <- covariances_0[-1,-1]
  Q[1,] <- c(parameters[4],0,0,0,0)
  Q[,1] <- c(parameters[4],0,0,0,0)
  Q[2,] <- c(0,0,0,0,0)
  Q[,2] <- c(0,0,0,0,0)
  
  x_prior <- matrix(NA, nrow = 5, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 5, ncol = (nrow(data1)+1))
  x_post[,1] <- c(6,6,coefficients_0)
  
  P_prior <- array(0, dim = c(5,5,nrow(data1)))
  P_post <- array(0, dim = c(5,5,nrow(data1)+1))
  P_post[,,1] <- matrix(0, nrow = 5, ncol = 5)
  #P_post[,,1] <- Q
  
  return(list("F"=F, "Q"=Q, "R"=R, "rho"=rho, "x_prior"=x_prior, "x_post"=x_post, "P_prior"=P_prior, "P_post"=P_post))
}
