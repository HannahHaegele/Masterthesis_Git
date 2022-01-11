
#preamble ####
rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(readr)
library(corrplot)
library(dplyr)
library(magrittr)
library(data.table)
library(here)
library(seasonal) #used for seasonsal adjustment
library(ggplot2)
library(ConsReg)
library(quadprog)
library(cowplot)
library(forecast)
library(lmtest)
library(zoo)

#read in data ####

#covid dummy
covid <- as.data.frame(read_csv(here('0_Data/phillips_corona.csv'))) %>%
  select(c(covid,date))

#data unemployment (sa)
unemployment <- read_csv(here('0_Data/UNRATE.csv')) %>%
  rename(unemp = UNRATE) 
as.data.frame(unemployment)

#data NAIRU (not sa)
NAIRU <- read_csv(here('0_Data/NROU.csv')) %>%
  rename(NAIRU = NROU) 
as.data.frame(NAIRU)

#data inflation expectation (not sa)
inflation_expectation <- read_csv(here('0_Data/EXPINF10YR.csv')) %>%
  rename(expect = EXPINF10YR, ) 
as.data.frame(inflation_expectation)

#data import price deflator (sa)
importpricedeflator <- as.data.frame(read_csv(here('0_Data/A255RD3Q086SBEA.csv'))) %>%
  rename(IMPdef = A255RD3Q086SBEA) 

#data cpi (sa)
cpi <- read_csv(here('0_Data/CPALTT01USM661S.csv')) %>%
  rename(cpi = CPALTT01USM661S) 
as.data.frame(cpi)

#testing inflation expectation ts for seasonal adjustment ####

inflationexpectation0<-ts(inflation_expectation[,-1],frequency=12,start=c(1982,1))
inflationexpectation0.seas<-seas(inflationexpectation0)
qs(inflationexpectation0.seas)

#there is no evidence of seasonality in the original and hence the time series is not adjusted

#estimating headline inflation  ####

#quarterly, annualized headline inflation 
  cpi$annualized_inflation <- NA
  for (i in 5:nrow(cpi)) {
    cpi$annualized_inflation[i] <- ((cpi$cpi[i]-cpi$cpi[i-3])/cpi$cpi[i-3])*100*4
  }

#yoy inflation as average over past 12 months   
  cpi$yoy_inflation <- NA
  for (i in 13:nrow(cpi)) {
    cpi$yoy_inflation[i] <- (cpi$annualized_inflation[i-1]+cpi$annualized_inflation[i-2]+cpi$annualized_inflation[i-3]+cpi$annualized_inflation[i-4]+
                               cpi$annualized_inflation[i-5]+cpi$annualized_inflation[i-6]+cpi$annualized_inflation[i-7]+cpi$annualized_inflation[i-8]+
                               cpi$annualized_inflation[i-9]+cpi$annualized_inflation[i-10]+cpi$annualized_inflation[i-11]+cpi$annualized_inflation[i-12])/12
  }

#monthly, annualized headline inflation   
  # cpi$annualized_inflation <- NA
  # for (i in 2:nrow(cpi)) {
  #   cpi$annualized_inflation[i] <- ((cpi$cpi[i]-cpi$cpi[i-1])/cpi$cpi[i-1])*100*12
  # }

#comparison monthly and quarterly growth rates     
  # e <- ggplot(cpi) + geom_line( aes(x = DATE, y = annualized_inflation,group = 1,color='red')) + theme_bw()
  # e + geom_line( aes(x = DATE, y = annualized_inflation1,group = 1,color='blue'))

#yoy inflation as average over past 4 quarters
  # cpi$yoy_inflation <- NA
  # for (i in 13:nrow(cpi)) {
  #   cpi$yoy_inflation[i] <- (cpi$annualized_inflation[i-3]+cpi$annualized_inflation[i-6]+cpi$annualized_inflation[i-9]+cpi$annualized_inflation[i-12])/4
  # }
  
#yoy inflation as yearly headline inflation 
  # cpi$yoy_inflation1 <- NA
  # for (i in 13:nrow(cpi)) {
  #   cpi$yoy_inflation1[i] <- ((cpi$cpi[i]-cpi$cpi[i-12])/cpi$cpi[i-12])*100
  # }

#estimating import price inflation ####

  #interpolation for monthly import deflator data 
  monthly = seq(importpricedeflator$DATE[1], tail(importpricedeflator$DATE,1), by="month")
  importpricedeflator2 <- data.frame(DATE=monthly, spline(importpricedeflator, method="fmm",xout = monthly)$y) 
  importpricedeflator <- merge(importpricedeflator, importpricedeflator2, by="DATE", all=TRUE) %>%
    rename(IMPdef_monthly = spline.importpricedeflator..method....fmm...xout...monthly..y)
  
  #quarterly, annualized growth rates
  importpricedeflator$importpriceinflation <- NA
  for (i in 5:nrow(importpricedeflator)) {
    importpricedeflator$importpriceinflation[i] <- ((importpricedeflator$IMPdef_monthly[i]-importpricedeflator$IMPdef_monthly[i-3])/importpricedeflator$IMPdef_monthly[i-3])*100*4
  }
  
  #monthly, annualized growth rates
  # importpricedeflator$importpriceinflation  <- NA
  # for (i in 2:nrow(importpricedeflator)) {
  #   importpricedeflator$importpriceinflation[i] <- ((importpricedeflator$IMPdef_monthly[i]-importpricedeflator$IMPdef_monthly[i-1])/importpricedeflator$IMPdef_monthly[i-1])*100*12
  # }
  
  
  #comparison of volatility of quarterly and monthly growth rates  
  # e <- ggplot(importpricedeflator) + geom_line( aes(x = DATE, y = importpriceinflation,group = 1,color='red')) + theme_bw()
  # e + geom_line( aes(x = DATE, y = importpriceinflation1,group = 1,color='blue'))
  
  drop.cols <- c('IMPdef','IMPdef_monthly')
  importpricedeflator <- importpricedeflator %>% select(-one_of(drop.cols))

#alternative for import price inflation ####
#yields slighlty different results (but negative theta)


#quarterly growth rates
# importpricedeflator$quarterly_importpriceinflation <- NA
# for (i in 2:nrow(importpricedeflator)) {
#   importpricedeflator$quarterly_importpriceinflation[i] <- (importpricedeflator$IMPdef[i]-importpricedeflator$IMPdef[i-1])/importpricedeflator$IMPdef[i-1]*100
# }
# 
# #annualized growth rates
# importpricedeflator$importpriceinflation1 <- importpricedeflator$quarterly_importpriceinflation*4
# 
# drop.cols <- c('IMPdef','quarterly_importpriceinflation')
# importpricedeflator <- importpricedeflator %>% select(-one_of(drop.cols))
# 
# #interpolation for monthly growth rates
# monthly = seq(importpricedeflator$DATE[1], tail(importpricedeflator$DATE,1), by="month")
# importpricedeflator2 <- data.frame(DATE=monthly, spline(importpricedeflator, method="fmm",xout = monthly)$y)
# importpricedeflator <- merge(importpricedeflator, importpricedeflator2, by="DATE", all=TRUE) %>%
#   rename(importpriceinflation = spline.importpricedeflator..method....fmm...xout...monthly..y)
# 


#merge and transform data ####

data <- merge(unemployment,inflation_expectation,by.x = "DATE", by.y = "DATE", all.x = TRUE)  %>%
  merge(importpricedeflator,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(NAIRU,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(cpi,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(covid,by.x = "DATE", by.y = "date", all.x = TRUE) %>%
  mutate(expect_yoy = expect-yoy_inflation) %>%
  mutate(relativeimportpriceinflation = importpriceinflation-annualized_inflation) %>%
  rename(date = DATE) %>%
  relocate(covid, .after = date) %>%
  select(c(date,covid,unemp,expect,importpriceinflation,NAIRU,annualized_inflation,yoy_inflation,expect_yoy,relativeimportpriceinflation))


#subset of sample that contains data for all variables (expected inflation)
data1 <- filter(data,date > "1981-12-01" & date < "2021-08-01")
data1$t <- 1:nrow(data1) 
data1 <- relocate(data1, t) 

data1 <- as_tibble(data1)
write_csv(data1, here('0_Data/model_input_data.csv'))

#scatter-plot COVID ####

# ggplot(data=data, aes(x=unemp, y=annualized_inflation))+
#   geom_point()
# 
# 
# png(filename = here("1_Plots/Correlation_Covid.png") , height=350, width=350)
# ggplot(data=data %>% filter(covid==1), aes(x=unemp, y=annualized_inflation))+
#   geom_point()
# dev.off()


#linear regression ####
#sample: 1982-2021
# reg <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data1)
# summary(reg)
# vcov(reg)
# 
# # 
# # #regression with unemployment gap, using empirical NAIRU
# # unempgap <- data1$unemp-data1$NAIRU
# # reg <- lm(annualized_inflation~unempgap+relativeimportpriceinflation+expect_yoy,data=data1)
# # summary(reg)
# # 
# reg <- ConsReg(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data1,
#                constraints ='unemp < 0,relativeimportpriceinflation > 0,expect_yoy > 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(3.5,-0.5,0.1,0.2))
# summary(reg)


#HAC sandwich errors ####
#source by Achim Zeileis
#https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf
#http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/sandwich/sandwich.pdf

reg0 <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data1)

reg <- ConsReg(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data1,
               constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))

#bread 
X <- cbind(rep(1,nrow(data1)),data1$unemp,data1$relativeimportpriceinflation,data1$expect_yoy)
bread <-  solve(crossprod(X)) * as.vector(nrow(data1))

#estimating function 
estfun <- function (obj, ...) {
  wts <- weights(obj)
  if(is.null(wts)) wts <- 1
  as.vector(residuals(obj)) * wts * X
}

#meat (HAC estimator)

#isotonic autocorrelation function 
#returns decreasing autocorrelation function(decreasing autocorrelations are required to have decreasing weights in the HAC estimator)
isoacf <- function(x) {
  acfWeave <- function(x, lag = trunc(5*sqrt(length(x))))
  {
    x <- x - mean(x)
    autocov <- function(ii, xx)
      cov(xx[1:(length(xx)-ii+1)],xx[ii:length(xx)])
    covs <- sapply(2:lag, autocov, xx = x)
    covs/var(x)
  }

    lagmax <- length(x) - 1
    lagmax <- min(length(x) - 1, lagmax)
    covs <- as.vector(acf(x, lag.max = lagmax -1, plot = FALSE)$acf)[-1]
    rval <- c(1, -isoreg(1:(length(covs)+1), c(-covs, 0))$yf)

  return(rval)
}

#weights for the HAC estimator are estimated following Lumley and Heagerty (1999) 
#adaptive weighting scheme where the weights are chosen based on the estimated autocorrelations of the residuals
weightsLumley <- function(x,method = c("truncate", "smooth"), acf = isoacf, tol = 1e-7, data = list(), ...) {
  method <- match.arg(method)
  res <- residuals(x) 
  n <- length(res)

  index <- 1:n
  res <- res[index]
  
  rhohat <- acf(res)
  
  switch(method,
  "truncate" = {
  C <- 4
  lag <- max((1:length(rhohat))[rhohat^2*n > C])
  weights <- rep(1, lag)
  },
  "smooth" = {
  C <- 1
  weights <- C * n * rhohat^2
  weights <- ifelse(weights > 1, 1, weights)
  weights <- weights[1:max(which(abs(weights) > tol))]
  })
  
  return(weights)
}

meat <- function(obj, weights, ...) {
  psi <- estfun(obj)
  n <- nrow(psi)
  
  rval <- 0.5 * crossprod(psi) * weights[1]
  
  for(i in 2:length(weights)) {
    rval <- rval + weights[i] * crossprod(psi[1:(n-i+1),], psi[i:n,])
  }
  
  (rval + t(rval))/n
}

meat <- meat(reg,weightsLumley(reg))

#sandwich 
1/nrow(estfun(reg)) * (bread %*% meat %*% bread)

#ARMA errors ####

reg0 <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data)

reg <- ConsReg(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,
                                         constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))

png(filename = here("1_Plots/acf.png") , height=350, width=350)
plot(acf(reg$residuals))
dev.off()

png(filename = here("1_Plots/pcf.png") , height=350, width=350)
plot(pacf(reg$residuals))
dev.off()

#choosing optimal number of lags for the ARMA model 
#yields (often) ARMA(11,2)
AIC = c()
max.q=12 
max.p=12
index <- 1

for(q in 0:max.q){
  
  for(p in 0:max.p){
    
    reg <- ConsRegArima(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,order = c(p, q),
                        constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
    
    AIC[index] <- reg$aic
    index <- index + 1
    
  }
}

a <- ceiling(which.min(AIC)/(max.q+1)-1)

print(paste0("The AIC-optimal MA lag is ",ceiling(which.min(AIC)/(max.q+1)-1), " and the corresponding AR lag is ",which.min(AIC)-((a*(max.q+1)+1)))) 


#rolling window regressions ####
#if we have data on the full year 2021, then we would have 31 full rolling windows 
#df=number observations - number of parameters (including intercept)
# # 
# for (i in 1:31) {
#   data <- filter(data1, t>=1+(i-1)*12 & t<=120+(i-1)*12)
# 
#   reg0 <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data)
# 
#   reg <- ConsReg(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,
#                  constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
# 
#   X <- cbind(rep(1,nrow(data)),data$unemp,data$relativeimportpriceinflation,data$expect_yoy)
# 
#   as.vector(assign(paste0("coefficients_", i), summary(reg)$coeff[1:4]))
#   as.vector(assign(paste0("variances_coefficients_", i), (summary(reg)$coeff[5:8])^2))
# 
#   assign(paste0("variance_estimate_", i), sum(reg$residuals^2)/(nrow(data)-4))
# 
#   #variance-covariance matrices: sigma2*(X'X)^-1 and HAC sandwich estimation 
#   #assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-4)) * solve(crossprod(X)))
#   assign(paste0("covariance_matrix_", i), 1/nrow(estfun(reg)) * (bread %*% meat %*% bread))
#   
# }

# #average of all coefficients, their variances and the variance of the estimate (i.e. of the residuals/regression)
# coefficients <- do.call(rbind, lapply( paste0("coefficients_", 1:31) , get) )
# variances_coefficients <- do.call(rbind, lapply( paste0("variances_coefficients_", 1:31) , get) )
# 
# coefficients_0 <- as.vector(colMeans(coefficients))[2:4]
# variances_coefficients_0 <- as.vector(colMeans(variances_coefficients))[2:4]
# 
# variance_estimate <- do.call(rbind, lapply( paste0("variance_estimate_", 1:31) , get) )
# variance_estimate_0 <- as.vector(colMeans(variance_estimate))
# 
# variance_NAIRU <- var(data1$NAIRU,na.rm = TRUE)
# 
# #average of all variance-covariance matrices
# covariances_0 <- do.call(cbind, lapply( paste0("covariance_matrix_", 1:i) , get))
# covariances_0  <- array(covariances_0, dim=c(4,4,i))
# covariances_0 <- apply(covariances_0 , c(1, 2), mean, na.rm = TRUE)
# 
# #print output
# variances_coefficients_0
# variance_estimate_0
# variance_NAIRU
# covariances_0

#rolling window regressions with ARMA errors ####
# 
# #test for auto-correlated errors and examining the ARMA structure of the errors 
# # acf(reg$residuals)
# # pacf(reg$residuals)
# # auto.arima(residuals(reg))
# # bgtest(reg,order=4)
# 
for (i in 1:31) {
  data <- filter(data1, t>=1+(i-1)*12 & t<=120+(i-1)*12)
  
  reg0 <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data)
  
  reg <- ConsRegArima(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,order = c(11, 2),
                      constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
  

  X <- cbind(rep(1,nrow(data)),data$unemp,data$relativeimportpriceinflation,data$expect_yoy)

  as.vector(assign(paste0("coefficients_", i), summary(reg)$coeff[1:4]))
  #as.vector(assign(paste0("variances_coefficients_", i), (summary(reg)$coeff[5:8])^2))

  assign(paste0("variance_estimate_", i), sum(reg$residuals^2)/(nrow(data)-(reg$order[1]+4)))

  #variance-covariance matrices = sigma2*(X'X)^-1
  assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-(reg$order[1]+4))) * solve(crossprod(X)))
}

#average of all coefficients, their variances and the variance of the estimate (i.e. of the residuals/regression)
coefficients <- do.call(rbind, lapply( paste0("coefficients_", 1:31) , get) )
coefficients[,2] <- (-1)*coefficients[,2]
variances_coefficients <- do.call(rbind, lapply( paste0("variances_coefficients_", 1:31) , get) )

coefficients_0 <- as.vector(colMeans(coefficients))[2:4]
variances_coefficients_0 <- as.vector(colMeans(variances_coefficients))[2:4]

variance_estimate <- do.call(rbind, lapply( paste0("variance_estimate_", 1:31) , get) )
variance_estimate_0 <- as.vector(colMeans(variance_estimate))

variance_NAIRU <- var(data1$NAIRU,na.rm = TRUE)

#average of all variance-covariance matrices
covariances_0 <- do.call(cbind, lapply( paste0("covariance_matrix_", 1:i) , get))
covariances_0  <- array(covariances_0, dim=c(4,4,i))
covariances_0 <- apply(covariances_0 , c(1, 2), mean, na.rm = TRUE)

#print output
# variances_coefficients_0
# variance_estimate_0
# variance_NAIRU
# covariances_0

#EKF recursions ####

#initialization (values, vectors, matrices) 
  s = 15 
  rho = 0.9 
  
  x_prior <- matrix(NA, nrow = 4, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 4, ncol = (nrow(data1)+1))
  x_post[,1] <- c(0,coefficients_0)
  
  Q <- covariances_0
  Q[1,] <- c(variance_NAIRU*s,0,0,0)
  Q[,1] <- c(variance_NAIRU*s,0,0,0)
  
  R <- variance_estimate_0 
  
  P_prior <- matrix(0, nrow = 4, ncol = 4)
  P_post <- matrix(0, nrow = 4, ncol = 4)
  
  F <- diag(c(rho,1,1,1), 4, 4)
  
  h <- function(i) {
    (x_prior[2,i])*(x_prior[1,i]) + x_prior[3,i]*data1$relativeimportpriceinflation[i] + x_prior[4,i]*data1$expect[i] + (1-x_prior[4,i])*data1$yoy_inflation[i]
    }
  
  H <- function(i) {
    t(c((x_prior[2,i]), (x_prior[1,i]), data1$relativeimportpriceinflation[i], (data1$expect[i]-data1$yoy_inflation[i])))
  }

#recursions
for (i in 1:(nrow(data1))) {
  
  #prediction step 
  x_prior[,i] <- F %*% x_post[,i]
  P_prior <- F %*% tcrossprod(P_post,F) + Q 
  
  #update step 
  y <- data1$annualized_inflation[i]-h(i)
  S <- H(i) %*% tcrossprod(P_prior,H(i)) + R
  S_inv <- solve(S)
  K <- tcrossprod(P_prior,H(i)) %*% S_inv
  
  x_post[,i+1] <- x_prior[,i] + K %*% y
  P_post <- (diag(1,4,4) - K %*% H(i)) %*% P_prior
  
  #constraint 1 (adjustment of Kalman filter recursions when updated state vector does not satisfy inequality constraint)
  if (x_post[2,i+1]<0 | x_post[3,i+1]<0 | x_post[4,i+1]<0 | x_post[4,i+1]>1) {
    
    print(paste0("one of the inequality constraints is binding at iteration ", i, " and time ",data1$date[i]))

    # lower-bounds 
    A.lbs <- rbind(c( 1, 0, 0, 0),
                   c( 0, 1, 0, 0),
                   c( 0, 0, 1, 0),
                   c( 0, 0, 0, 1))
    b.lbs <- c(-1000, 0, 0, 0)
    
    # upper-bounds on variables
    A.ubs <- rbind(c( -1, 0, 0, 0),
                   c( 0, -1, 0, 0),
                   c( 0, 0, -1, 0),
                   c( 0, 0, 0, -1))
    b.ubs <-  c(-1000, -1000, -1000, -1)
    
    Amat = t(rbind(A.lbs, A.ubs))
    bvec = c(b.lbs, b.ubs)
    
    #representation in quadratic programming form  
    Dmat <- matrix(0,4,4)
    
    P_post_inv <- solve(P_post)
    diag(Dmat) <- diag(P_post_inv)*2
    
    Dmat[lower.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3])
    Dmat[upper.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[4,1]+P_post_inv[1,4],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3])
    
    a <- t(-x_post[,i+1]) %*% P_post_inv[,1] + P_post_inv[1,1] * (-x_post[1,i+1]) + P_post_inv[1,2] * (-x_post[2,i+1]) + P_post_inv[1,3] * (-x_post[3,i+1]) + P_post_inv[1,4] * (-x_post[4,i+1])
    
    b <- t(-x_post[,i+1]) %*% P_post_inv[,2] + P_post_inv[2,2] * (-x_post[2,i+1]) + P_post_inv[2,1] * (-x_post[1,i+1]) + P_post_inv[2,3] * (-x_post[3,i+1]) + P_post_inv[2,4] * (-x_post[4,i+1])
    
    c <- t(-x_post[,i+1]) %*% P_post_inv[,3] + P_post_inv[3,3] * (-x_post[3,i+1]) + P_post_inv[3,1] * (-x_post[1,i+1]) + P_post_inv[3,2] * (-x_post[2,i+1]) + P_post_inv[3,4] * (-x_post[4,i+1])
    
    d <- t(-x_post[,i+1]) %*% P_post_inv[,4] + P_post_inv[4,4] * (-x_post[4,i+1]) + P_post_inv[4,1] * (-x_post[1,i+1]) + P_post_inv[4,2] * (-x_post[2,i+1]) + P_post_inv[4,3] * (-x_post[3,i+1])
    
    dvec <- c(-a,-b,-c,-d)
    
    solve.QP(Dmat,dvec,Amat,bvec=bvec)
    
    #update state vector
    x_post[,i+1] <- solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution
    
  }
  
  #constraint 2 (deviation of ML estimates of shock variances relative to initial estimates obtained from rolling regressions)
  #assumed that the logical order of these constraints is this way, could possibly be the other way round? 
  if (sum(diag(P_post) > diag(Q))>0) {
    print(paste0("ML estimates of shock variances are larger than initial estimates obtained from rolling regressions at iteration ",i))
    P_post <- Q
  }
  
  # #control of quadratic programming representation 
  # constante <- (-x_post[,i+1] %*% P_post_inv[,1]) %*% (-x_post[1,i+1]) +
  #   (-x_post[,i+1] %*% P_post_inv[,2]) %*% (-x_post[2,i+1]) +
  #   (-x_post[,i+1] %*% P_post_inv[,3]) %*% (-x_post[3,i+1]) +
  #   (-x_post[,i+1] %*% P_post_inv[,4]) %*% (-x_post[4,i+1]) 
  # 
  # abs(t(solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution-x_post[,i+1]) %*% P_post_inv %*% (solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution-x_post[,i+1]) - constante - solve.QP(Dmat,dvec,Amat,bvec=bvec)$value) < 1e-8
  # 
}

#preparing output ####

#convert matrix to a wide data frame
  x_post_data <- as.data.frame(t(x_post))
  x_post_data <- x_post_data[-1,]
  
  x_post_data$date <- c(as.character(data1$date))
  x_post_data$date <- as.Date(x_post_data$date, format="%Y-%m-%d")
  
  colnames(x_post_data) <- c("unemploymentgap","kappa","gamma","theta","date")
  x_post_data$NAIRU <- (x_post_data$unemploymentgap - data1$unemp)*(-1)
  x_post_data$unemp <- data1$unemp
  x_post_data$annualizedinflation <- data1$annualized_inflation
  x_post_data$yoy_inflation <- data1$yoy_inflation

  x_post_data$inflation <- c()
  for (i in 1:nrow(x_post_data)) {
    x_post_data$inflation[i] <- (x_post_data[i,1])*(x_post_data[i,2]) + x_post_data[i,3]*data1$relativeimportpriceinflation[i] + x_post_data[i,4]*data1$expect[i] + (1-x_post_data[i,4])*data1$yoy_inflation[i]
  }
  
  #add recession dates for recession bars in plots (source: https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
  recessions.df = read.table(textConnection(
    "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2020-04-01"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)
  
  recessions.trim = subset(recessions.df, Trough >= min(data1$date))

#plotting NAIRU/unemployment ####

a <- ggplot(x_post_data) + geom_line( aes(x = date, y = NAIRU,group = 1,colour='NAIRU')) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = unemp,group = 1,colour='unemployment rate')) +
    scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment"="blue")) 

#plotting kappa ####

b <- ggplot(x_post_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#plotting gamma #####

c <- ggplot(x_post_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#plotting theta #####

d <- ggplot(x_post_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#plotting predicted/actual inflation #####
e <- ggplot(x_post_data) + geom_line( aes(x = date, y = annualizedinflation,group = 1,colour='actual inflation')) + theme_bw()
e + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = inflation,group = 1,colour='predicted inflation')) + 
    scale_colour_manual(" ", values = c("actual inflation" ="red", "predicted inflation"="blue")) 
#plot_grid(a, b, c, d, labels = "AUTO")


#save plots ####

png(filename = here("1_Plots/NAIRU.png") , height=350, width=350)
a <- ggplot(x_post_data) + geom_line( aes(x = date, y = NAIRU,group = 1,colour='NAIRU')) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
  geom_line(aes(x = date, y = unemp,group = 1,colour='unemployment rate')) +
  scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment"="blue")) 
dev.off()

png(filename = here("1_Plots/kappa.png") , height=350, width=350)
b <- ggplot(x_post_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
dev.off()

png(filename = here("1_Plots/gamma.png") , height=350, width=350)
c <- ggplot(x_post_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
dev.off()

png(filename = here("1_Plots/theta.png") , height=350, width=350)
d <- ggplot(x_post_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
dev.off()

png(filename = here("1_Plots/fit.png") , height=350, width=350)
e <- ggplot(x_post_data) + geom_line( aes(x = date, y = annualizedinflation,group = 1,colour='actual inflation')) + theme_bw()
e + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
  geom_line(aes(x = date, y = inflation,group = 1,colour='predicted inflation')) + 
  scale_colour_manual(" ", values = c("actual inflation" ="red", "predicted inflation"="blue")) 
dev.off()

