
kalman.log.likelihood.1b <- function(x_prior, x_post, P_prior, P_post, F,Q,R,rho) {
  
  h <- function(i) {
    -(x_prior[2,i])*(data1$urate[i]-x_prior[1,i]) + x_prior[3,i]*data1$relative.import.price.inflation[i] + x_prior[4,i]*data1$expected.inf[i] + (1-x_prior[4,i])*data1$inflation.yoy[i]
  }
  
  H <- function(i) {
    t(c((x_prior[2,i]), -(data1$urate[i]-x_prior[1,i]), data1$relative.import.price.inflation[i], (data1$expected.inf[i]-data1$inflation.yoy[i])))
  }
  
  z <- function(i) {data1$inflation[i]}
  
  
  ll.vec <- matrix(0,(nrow(data1)),1)
  ll.cum <- 0
  n <- 2 #y is two-dimensional
  
  for (i in 1:(nrow(data1))) {
    
    #prediction step 
    x_prior[,i] <- F %*% x_post[,i]
    P_prior[,,i] <- F %*% tcrossprod(P_post[,,i],F) + Q 
    
    #update step part 1
    y <- z(i) - h(i)
    S <- H(i) %*% tcrossprod(P_prior[,,i],H(i)) + R
    S_inv <- solve(S)

    #if(is.singular.matrix(S)==TRUE) {S_inv <- pinv(S)}
    
    K <- tcrossprod(P_prior[,,i],H(i)) %*% S_inv
    
    #estimate log-likelihood 
    ll.vec[i] <- drop(-(n / 2) * log(2 * atan(1) * 4) - 0.5 * log(det(S)) -0.5 * t(y) %*% solve(S, y))
    ll.cum <- ll.cum + ll.vec[i]
    
    #update step part 2
    x_post[,i+1] <- x_prior[,i] + K %*% y
    P_post[,,i+1] <- (diag(1,4,4) - K %*% H(i)) %*% P_prior[,,i]   
    
    
  }
  return(list("ll.vec"=ll.vec,"ll.cum"=ll.cum))
  
}
