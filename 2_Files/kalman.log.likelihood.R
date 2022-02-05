
kalman.log.likelihood <- function(x_prior, x_post, P_prior, P_post, F,Q,R,rho) {
  
  h <- function(i,x_prior) {
    y <- matrix(0,2,1)
    y[1,1] <- -(x_prior[5,i]) + x_prior[4,i]
    y[2,1] <- (x_prior[5,i])*(x_prior[1,i]) + x_prior[2,i]*data1$relative.import.price.inflation[i] + x_prior[3,i]*data1$expected.inf[i] + (1-x_prior[3,i])*data1$inflation.yoy[i]
    return(y)
  }
  
  H <- function(i,x_prior) {
    a <- t(c(0, 0, 0, 1, -1))
    b <- t(c((x_prior[5,i]), data1$relative.import.price.inflation[i], (data1$expected.inf[i]-data1$inflation.yoy[i]), x_prior[1,i], x_prior[1,i]))
    rbind(a,b)
  }
  
  z <- function(i) {
    rbind(data1$urate[i],data1$inflation[i])
  }
  
  
  ll.vec <- matrix(0,(nrow(data1)),1)
  ll.cum <- 0
  n <- 2 #y is two-dimensional
  
  for (i in 1:(nrow(data1))) {
    
    #prediction step 
    x_prior[,i] <- F %*% x_post[,i]
    P_prior[,,i] <- F %*% tcrossprod(P_post[,,i],F) + Q 
    
    #update step part 1
    y <- z(i) - h(i,x_prior)
    S <- H(i,x_prior) %*% tcrossprod(P_prior[,,i],H(i,x_prior)) + R
    S_inv <- solve(S)
    K <- tcrossprod(P_prior[,,i],H(i,x_prior)) %*% S_inv
    
    #estimate log-likelihood 
    ll.vec[i] <- drop(-(n / 2) * log(2 * atan(1) * 4) - 0.5 * log(det(S)) -0.5 * t(y) %*% solve(S, y))
    ll.cum <- ll.cum + ll.vec[i]
    
    #update step part 2
    x_post[,i+1] <- x_prior[,i] + K %*% y
    P_post[,,i+1] <- (diag(1,5,5) - K %*% H(i,x_prior)) %*% P_prior[,,i]
    
    
    
  }
  return(list("ll.vec"=ll.vec,"ll.cum"=ll.cum))
  
}
