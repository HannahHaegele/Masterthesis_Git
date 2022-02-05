

rolling.window.1 <- function() {
  
  for (i in 1:41) {
    
    data <- filter(data1, t>=1+(i-1)*4 & t<=40+(i-1)*4)
    
    reg0 <- lm(inflation ~ gap + relative.core.import.price.inflation + expected.yoy.inflation,data=data) 
    
    reg <- ConsReg(inflation ~ gap + relative.core.import.price.inflation + expected.yoy.inflation ,data=data,
                   constraints ='gap < 0, relative.core.import.price.inflation > 0, expected.yoy.inflation > 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
    
    
    X <- cbind(rep(1,nrow(data)),data$gap,data$relative.core.import.price.inflation,data$expected.yoy.inflation) #design matrix of regression model 
    
    #store coefficient estimates, sigma2 as well as variance-covariance matrices (sigma2*(X'X)^-1) for each rolling window 
    as.vector(assign(paste0("coefficients_", i), summary(reg)$coeff[1:4]))
    assign(paste0("variance_estimate_", i), sum(reg$residuals^2)/(nrow(data)-4))
    assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-4) * solve(crossprod(X))))
    
  }
  
  #take the average of r.w. results: coefficient estimates, sigma2 as well as variance-covariance matrices
  
  variance_estimate <- do.call(rbind, lapply( paste0("variance_estimate_", 1:31) , get) )
  variance_estimate_0 <- as.vector(colMeans(variance_estimate))
  
  coefficients <- do.call(rbind, lapply( paste0("coefficients_", 1:31) , get) )
  coefficients[,2] <- (-1)*coefficients[,2]
  coefficients_0 <- as.vector(colMeans(coefficients))[2:4]
  
  covariances_0 <- do.call(cbind, lapply( paste0("covariance_matrix_", 1:i) , get))
  covariances_0  <- array(covariances_0, dim=c(4,4,i))
  covariances_0 <- apply(covariances_0 , c(1, 2), mean, na.rm = TRUE)
  
  variance_NAIRU <- var(data1$NAIRU)
  
  return(list("variance_estimate_0"=variance_estimate_0,"coefficients_0"=coefficients_0, "covariances_0"= covariances_0," variance_NAIRU"=variance_NAIRU))
  
}