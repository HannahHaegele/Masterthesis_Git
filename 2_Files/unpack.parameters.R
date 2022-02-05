unpack.parameters <- function(parameters) {
  
  rho <- parameters[2]
  
  F <- matrix(0, 5, 5)
  F[1, 1] <- F[2, 2] <- F[3, 3] <- F[4, 4] <- 1
  F[5, 5] <- rho
  
  R <- matrix(0,2,2)
  R[1, 1] <- mean(roll.vare)
  R[2, 2] <- 1e-4

  Q <- matrix(0,5,5)
  Q[1, 1] <- mean(roll.var[1,])
  Q[2, 2] <- mean(roll.var[2,])
  Q[3, 3] <- mean(roll.var[3,])
  Q[4, 4] <- var(diff(data$urate))*(1/(15+1))
  Q[5, 5] <- var(diff(data$urate))*(1-(1/(15+1)))


  x_prior <- matrix(NA, nrow = 5, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 5, ncol = (nrow(data1)+1))
  x_post[,1] <- c(roll.coeff[1,1], roll.coeff[2,2], roll.coeff[3,3], 0, 0)
  
  P_prior <- array(0, dim = c(5,5,nrow(data1)))
  P_post <- array(0, dim = c(5,5,nrow(data1)+1))
  P_post[,,1] <- diag(nrow = 5, ncol = 5)
  
  return(list("F"=F, "Q"=Q, "R"=R, "rho"=rho, "x_prior"=x_prior, "x_post"=x_post, "P_prior"=P_prior, "P_post"=P_post))
}