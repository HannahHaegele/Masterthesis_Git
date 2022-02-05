
kalman.states.2 <- function(x_prior, x_post, P_prior, P_post, F,Q,R,rho) {
  
  h <- function(i,rho,x_prior) {
    y <- matrix(0,2,1)
    y[1,1] <- x_prior[1,i] - rho*x_prior[2,i] + rho*data1$l.urate[i]
    y[2,1] <- -(x_prior[3,i])*(data1$urate[i]-x_prior[1,i]) + x_prior[4,i]*data1$relative.import.price.inflation[i] + x_prior[5,i]*data1$expected.inf[i] + (1-x_prior[5,i])*data1$inflation.yoy[i]
    return(y)
  }
  
  H <- function(i,rho,x_prior) {
    a <- t(c(1, -rho, 0, 0, 0))
    b <- t(c((x_prior[3,i]), 0, -(data1$urate[i]-x_prior[1,i]), data1$relative.import.price.inflation[i], (data1$expected.inf[i]-data1$inflation.yoy[i])))
    rbind(a,b)
  }
  
  z <- function(i) {
    rbind(data1$urate[i],data1$inflation[i])
  }
  
  #Step 1: forward recursions
  for (i in 1:(nrow(data1))) {
    
    #prediction step 
    x_prior[,i] <- F %*% x_post[,i]
    P_prior[,,i] <- F %*% tcrossprod(P_post[,,i],F) + Q 
    
    #update step
    y <- z(i) - h(i,rho,x_prior)
    S <- H(i,rho,x_prior) %*% tcrossprod(P_prior[,,i],H(i,rho,x_prior)) + R
    S_inv <- solve(S)
    K <- tcrossprod(P_prior[,,i],H(i,rho,x_prior)) %*% S_inv
    
    x_post[,i+1] <- x_prior[,i] + K %*% y
    P_post[,,i+1] <- (diag(1,5,5) - K %*% H(i,rho,x_prior)) %*% P_prior[,,i]
    
    
     #constraint 1 (adjustment of Kalman filter recursions when updated state vector does not satisfy inequality constraint)
    if (x_post[3,i+1]<0 | x_post[4,i+1]<0 | x_post[5,i+1]<0 | x_post[5,i+1]>1){
       
       # lower-bounds 
       A.lbs <- rbind(c( 1, 0, 0, 0, 0),
                      c( 0, 1, 0, 0, 0),
                     c( 0, 0, 1, 0, 0),
                      c( 0, 0, 0, 1, 0),
                      c( 0, 0, 0, 0, 1))
       b.lbs <- c(-1000, -1000, 0, 0, 0)
       
       # upper-bounds on variables
       A.ubs <- rbind(c( -1, 0, 0, 0, 0),
                      c( 0, -1, 0, 0, 0),
                      c( 0, 0, -1, 0, 0),
                      c( 0, 0, 0, -1, 0),
                      c( 0, 0, 0, 0, -1))
       b.ubs <-  c(-1000, -1000, -1000, -1000, -1)
       
       Amat = t(rbind(A.lbs, A.ubs))
       bvec = c(b.lbs, b.ubs)
       
       #representation in quadratic programming form  
       Dmat <- matrix(0,5,5)
       
       P_post_inv <- solve(P_post[,,i+1])
       diag(Dmat) <- diag(P_post_inv)*2
       
       Dmat[lower.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[1,5]+P_post_inv[5,1],
                                  P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[2,5]+P_post_inv[5,2],
                                  P_post_inv[3,4]+P_post_inv[4,3],P_post_inv[3,5]+P_post_inv[5,3],
                                  P_post_inv[4,5]+P_post_inv[5,4])
       
       Dmat[upper.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1],
                                  P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[2,3]+P_post_inv[3,2],
                                  P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3],
                                  P_post_inv[1,5]+P_post_inv[5,1],P_post_inv[2,5]+P_post_inv[5,2],P_post_inv[3,5]+P_post_inv[5,3],P_post_inv[4,5]+P_post_inv[5,4])
       
       
       a <- t(-x_post[,i+1]) %*% P_post_inv[,1] + P_post_inv[1,1] * (-x_post[1,i+1]) + P_post_inv[1,2] * (-x_post[2,i+1]) + P_post_inv[1,3] * (-x_post[3,i+1]) + P_post_inv[1,4] * (-x_post[4,i+1] + P_post_inv[1,5] * (-x_post[5,i+1]))
       
       b <- t(-x_post[,i+1]) %*% P_post_inv[,2] + P_post_inv[2,2] * (-x_post[2,i+1]) + P_post_inv[2,1] * (-x_post[1,i+1]) + P_post_inv[2,3] * (-x_post[3,i+1]) + P_post_inv[2,4] * (-x_post[4,i+1] + P_post_inv[2,5] * (-x_post[5,i+1]))
       
       c <- t(-x_post[,i+1]) %*% P_post_inv[,3] + P_post_inv[3,3] * (-x_post[3,i+1]) + P_post_inv[3,1] * (-x_post[1,i+1]) + P_post_inv[3,2] * (-x_post[2,i+1]) + P_post_inv[3,4] * (-x_post[4,i+1] + P_post_inv[3,5] * (-x_post[5,i+1]))
       
       d <- t(-x_post[,i+1]) %*% P_post_inv[,4] + P_post_inv[4,4] * (-x_post[4,i+1]) + P_post_inv[4,1] * (-x_post[1,i+1]) + P_post_inv[4,2] * (-x_post[2,i+1]) + P_post_inv[4,3] * (-x_post[3,i+1] + P_post_inv[4,5] * (-x_post[5,i+1]))
       
       e <- t(-x_post[,i+1]) %*% P_post_inv[,5] + P_post_inv[5,5] * (-x_post[5,i+1]) + P_post_inv[5,1] * (-x_post[1,i+1]) + P_post_inv[5,2] * (-x_post[2,i+1]) + P_post_inv[5,3] * (-x_post[3,i+1] + P_post_inv[5,4] * (-x_post[4,i+1]))
       
       dvec <- c(-a,-b,-c,-d,-e)
       
       solve.QP(Dmat,dvec,Amat,bvec=bvec)
       
       #update state vector
       x_post[,i+1] <- solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution
     }
     
     # #constraint 2 (deviation of ML estimates of shock variances relative to initial estimates obtained from rolling regressions)
     # #assumed that the logical order of these constraints is this way, could possibly be the other way round? 
     # if (sum(diag(P_post[,,i+1]) > diag(Q))>0) {
     #   P_post[,,i+1] <- Q
     # }
  }  
  
  #Step2: backward recursions
  
  x_post_smooth <- matrix(NA, nrow = 5, ncol = (nrow(data1)-1))
  P_post_smooth <- array(0, dim = c(5,5,nrow(data1)))
  
  #Start at t = T-1
  i <- nrow(data1)-1
  
  L <- P_post[,,i+1] %*% t(F) %*% solve(P_prior[,,i+1])
  x_post_smooth[,i] <- x_post[,i+1] + L %*% (x_post[,i+2] - x_prior[,i+1])
  P_post_smooth[,,i] <- P_post[,,i+1] + L %*% (P_post[,,i+2] - P_prior[,,i+1]) %*% t(L)
  
  #Continue for t = T-2,T-3,...,0
  for (i in (nrow(data1)-2):1) {
    
    L <- P_post[,,i+1] %*% crossprod(F,solve(P_prior[,,i+1]))
    x_post_smooth[,i] <- x_post[,i+1] + L %*% (x_post_smooth[,i+1] - x_prior[,i+1])
    P_post_smooth[,,i] <- P_post[,,i+1] + L %*% (P_post_smooth[,,i+1] - P_prior[,,i+1]) %*% t(L)
    
    if (x_post_smooth[3,i+1]<0 | x_post_smooth[4,i+1]<0 | x_post_smooth[5,i+1]<0 | x_post_smooth[5,i+1]>1){
      
      # lower-bounds 
      A.lbs <- rbind(c( 1, 0, 0, 0, 0),
                     c( 0, 1, 0, 0, 0),
                     c( 0, 0, 1, 0, 0),
                     c( 0, 0, 0, 1, 0),
                     c( 0, 0, 0, 0, 1))
      b.lbs <- c(-1000, -1000, 0, 0, 0)
      
      # upper-bounds on variables
      A.ubs <- rbind(c( -1, 0, 0, 0, 0),
                     c( 0, -1, 0, 0, 0),
                     c( 0, 0, -1, 0, 0),
                     c( 0, 0, 0, -1, 0),
                     c( 0, 0, 0, 0, -1))
      b.ubs <-  c(-1000, -1000, -1000, -1000, -1)
      
      Amat = t(rbind(A.lbs, A.ubs))
      bvec = c(b.lbs, b.ubs)
      
      #representation in quadratic programming form  
      Dmat <- matrix(0,5,5)
      
      P_post_inv <- solve(P_post_smooth[,,i+1])
      diag(Dmat) <- diag(P_post_inv)*2
      
      Dmat[lower.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[1,5]+P_post_inv[5,1],
                                 P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[2,5]+P_post_inv[5,2],
                                 P_post_inv[3,4]+P_post_inv[4,3],P_post_inv[3,5]+P_post_inv[5,3],
                                 P_post_inv[4,5]+P_post_inv[5,4])
      
      Dmat[upper.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1],
                                 P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[2,3]+P_post_inv[3,2],
                                 P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3],
                                 P_post_inv[1,5]+P_post_inv[5,1],P_post_inv[2,5]+P_post_inv[5,2],P_post_inv[3,5]+P_post_inv[5,3],P_post_inv[4,5]+P_post_inv[5,4])
      
      
      a <- t(-x_post_smooth[,i+1]) %*% P_post_inv[,1] + P_post_inv[1,1] * (-x_post_smooth[1,i+1]) + P_post_inv[1,2] * (-x_post_smooth[2,i+1]) + P_post_inv[1,3] * (-x_post_smooth[3,i+1]) + P_post_inv[1,4] * (-x_post_smooth[4,i+1] + P_post_inv[1,5] * (-x_post_smooth[5,i+1]))
      
      b <- t(-x_post_smooth[,i+1]) %*% P_post_inv[,2] + P_post_inv[2,2] * (-x_post_smooth[2,i+1]) + P_post_inv[2,1] * (-x_post_smooth[1,i+1]) + P_post_inv[2,3] * (-x_post_smooth[3,i+1]) + P_post_inv[2,4] * (-x_post_smooth[4,i+1] + P_post_inv[2,5] * (-x_post_smooth[5,i+1]))
      
      c <- t(-x_post_smooth[,i+1]) %*% P_post_inv[,3] + P_post_inv[3,3] * (-x_post_smooth[3,i+1]) + P_post_inv[3,1] * (-x_post_smooth[1,i+1]) + P_post_inv[3,2] * (-x_post_smooth[2,i+1]) + P_post_inv[3,4] * (-x_post_smooth[4,i+1] + P_post_inv[3,5] * (-x_post_smooth[5,i+1]))
      
      d <- t(-x_post_smooth[,i+1]) %*% P_post_inv[,4] + P_post_inv[4,4] * (-x_post_smooth[4,i+1]) + P_post_inv[4,1] * (-x_post_smooth[1,i+1]) + P_post_inv[4,2] * (-x_post_smooth[2,i+1]) + P_post_inv[4,3] * (-x_post_smooth[3,i+1] + P_post_inv[4,5] * (-x_post_smooth[5,i+1]))
      
      e <- t(-x_post_smooth[,i+1]) %*% P_post_inv[,5] + P_post_inv[5,5] * (-x_post_smooth[5,i+1]) + P_post_inv[5,1] * (-x_post_smooth[1,i+1]) + P_post_inv[5,2] * (-x_post_smooth[2,i+1]) + P_post_inv[5,3] * (-x_post_smooth[3,i+1] + P_post_inv[5,4] * (-x_post_smooth[4,i+1]))
      
      dvec <- c(-a,-b,-c,-d,-e)
      
      solve.QP(Dmat,dvec,Amat,bvec=bvec)
      
      #update state vector
      x_post_smooth[,i+1] <- solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution
    }
    
  }
  
  return(list("x_post"=x_post,"x_post_smooth"=x_post_smooth))
}

