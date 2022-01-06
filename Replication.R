#16/12/21 interpolation, finalizing dataset 
#17/12/21 fixed window regressions -> estimates, variances -> current version 
#18/12/21 import price inflation neu (plus alternative estimation); seasonal adjustment angefangen 
#20/12/21 v3 
#21/12/21 v4, mistake in sqrt(diag(vcov(reg)))) -> diag(vcov(reg))
#22/12/21 fixed mistake with data in rolling window (used data1 previously instead of data), covariance matrices, 
#         fixed mistake with lagged values of lagged yoy_inflation (both in dataset creation and h function, H Jacobian)

#differences v4: constrained least squares using ConsReg instead of non-constrained least squares 
#                full Q matrix (covariance averages) instead of diagonal Q matrix 
#differences v5: function h now includes lagged yoy_inflation (by one quarter)
#differences v6: tried to implement the first constraint in the filter (quadratic programming problem)


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

#read in data ####

#covid dummy
covid <- as.data.frame(read_csv(here('0_Data/phillips_corona.csv'))) %>%
  select(c(covid,date))

#data unemployment (sa)
unemployment <- read_csv(here('0_Data/UNRATE.csv')) %>%
  rename(unemp = UNRATE) 
#inflation_expectation$expect <- as.numeric(expect)
as.data.frame(unemployment)

#data NAIRU (not sa)
NAIRU <- read_csv(here('0_Data/NROU.csv')) %>%
  rename(NAIRU = NROU) 
#NAIRU$NAIRU <- as.numeric(NAIRU$NAIRU)
as.data.frame(NAIRU)

#data inflation expectation (not sa)
inflation_expectation <- read_csv(here('0_Data/EXPINF10YR.csv')) %>%
  rename(expect = EXPINF10YR, ) 
#inflation_expectation$expect <- as.numeric(expect)
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

#inflation ####
cpi$monthly_inflation <- NA
for (i in 2:nrow(cpi)) {
  cpi$monthly_inflation[i] <- ((cpi$cpi[i]-cpi$cpi[i-1])/cpi$cpi[i-1])*100
}

cpi$annualized_inflation <- cpi$monthly_inflation*12

cpi$yoy_inflation <- NA
for (i in 13:nrow(cpi)) {
  cpi$yoy_inflation[i] <- ((cpi$cpi[i]-cpi$cpi[i-12])/cpi$cpi[i-12])*100
}

#import price inflation ####

#interpolation for monthly import deflator data 
monthly = seq(importpricedeflator$DATE[1], tail(importpricedeflator$DATE,1), by="month")
importpricedeflator2 <- data.frame(DATE=monthly, spline(importpricedeflator, method="fmm",xout = monthly)$y) 
importpricedeflator <- merge(importpricedeflator, importpricedeflator2, by="DATE", all=TRUE) %>%
  rename(IMPdef_monthly = spline.importpricedeflator..method....fmm...xout...monthly..y)

#monthly growth rates (in percentage terms)
importpricedeflator$monthly_importpriceinflation <- NA
for (i in 2:nrow(importpricedeflator)) {
  importpricedeflator$monthly_importpriceinflation[i] <- ((importpricedeflator$IMPdef_monthly[i]-importpricedeflator$IMPdef_monthly[i-1])/importpricedeflator$IMPdef_monthly[i-1])*100
}

#annualized growth rates 
importpricedeflator$importpriceinflation <- importpricedeflator$monthly_importpriceinflation*12

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
  mutate(expect_yoy = expect-lag(yoy_inflation,3L)) %>%
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

data2 <- filter(data,date > "1981-09-01" & date < "2021-08-01")
data2$t <- 1:nrow(data2) 
data2 <- relocate(data2, t) 


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
#   #variance-covariance matrices = sigma2*(X'X)^-1
#   assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-4)) * solve(crossprod(X)))
# }
# 
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
  
  reg <- ConsRegArima(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,order = c(0, 1),
                      constraints ='unemp <= 0,relativeimportpriceinflation >= 0,expect_yoy >= 0',optimizer='mcmc',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
  

  X <- cbind(rep(1,nrow(data)),data$unemp,data$relativeimportpriceinflation,data$expect_yoy)

  as.vector(assign(paste0("coefficients_", i), summary(reg)$coeff[1:4]))
  as.vector(assign(paste0("variances_coefficients_", i), (summary(reg)$coeff[5:8])^2))

  assign(paste0("variance_estimate_", i), sum(reg$residuals^2)/(nrow(data)-4))

  #variance-covariance matrices = sigma2*(X'X)^-1
  assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-4)) * solve(crossprod(X)))
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
variances_coefficients_0
variance_estimate_0
variance_NAIRU
covariances_0

#EKF recursions ####

#initialization (values, vectors, matrices) 
s = 15 
rho = 0.9 

x_prior <- matrix(NA, nrow = 4, ncol = nrow(data2)-3)
x_post <- matrix(NA, nrow = 4, ncol = nrow(data2)-3+1)
x_post[,1] <- c(0,coefficients_0)
#w <- c(s*variance_NAIRU,variances_coefficients_0)

Q <- covariances_0
Q[1,] <- c(variance_NAIRU*s,0,0,0)
Q[,1] <- c(variance_NAIRU*s,0,0,0)

R <- variance_estimate_0 

P_prior <- matrix(0, nrow = 4, ncol = 4)
P_post <- matrix(0, nrow = 4, ncol = 4)

F <- diag(c(rho,1,1,1), 4, 4)

h <- function(i) {
  (x_prior[2,i])*(x_prior[1,i]) + x_prior[3,i]*data2$relativeimportpriceinflation[i+3] + x_prior[4,i]*data2$expect[i+3] + (1-x_prior[4,i])*data2$yoy_inflation[i]
}

H <- function(i) {
  t(c((x_prior[2,i]), (x_prior[1,i]), data2$relativeimportpriceinflation[i+3], (data2$expect[i+3]-data2$yoy_inflation[i])))
}

#Extended Kalman filter recursions
for (i in 1:(nrow(data2)-3)) {
  
  #prediction step 
  x_prior[,i] <- F %*% x_post[,i]
  P_prior <- F %*% tcrossprod(P_post,F) + Q 
  
  #update step 
  y <- data2$annualized_inflation[i+3]-h(i)
  S <- H(i) %*% tcrossprod(P_prior,H(i)) + R
  S_inv <- solve(S)
  K <- tcrossprod(P_prior,H(i)) %*% S_inv
  
  # For tracking progress
  if (y > variance_estimate_0) {
    print(paste0("the estimated residual with value ",y," is larger than average initial shock variance at iteration ",data2$date[i+3]))
    #y <- variance_estimate_0
  }
  
  
  x_post[,i+1] <- x_prior[,i] + K %*% y
  P_post <- (diag(1,4,4) - K %*% H(i)) %*% P_prior
  
  if (x_post[2,i+1]<0 | x_post[3,i+1]<0 | x_post[4,i+1]<0 | x_post[4,i+1]>1) {
    
    print(paste0("one of the constraints is binding at iteration ", i, " and time ",data2$date[i+3], " with values ",cat(x_post[,i+1],"\n",sep="\t")))

    
    #constraints for the quadratic optimization problem 
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
    
    #quadratic optimisation model 
    Dmat <- matrix(0,4,4)
    
    P_post_inv <- solve(P_post)
    diag(Dmat) <- diag(P_post_inv)*2
    
    Dmat[lower.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[1,4]+P_post_inv[4,1],P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3])
    Dmat[upper.tri(Dmat)] <- c(P_post_inv[1,2]+P_post_inv[2,1], P_post_inv[1,3]+P_post_inv[3,1],P_post_inv[2,3]+P_post_inv[3,2],P_post_inv[4,1]+P_post_inv[1,4],P_post_inv[2,4]+P_post_inv[4,2],P_post_inv[3,4]+P_post_inv[4,3])
    
    a <- t(-x_post[,i+1]) %*% P_post_inv[,1] + P_post_inv[1,2] * (-x_post[2,i+1]) + P_post_inv[1,3] * (-x_post[3,i+1]) + P_post_inv[1,4] * (-x_post[4,i+1])
    
    b <- t(-x_post[,i+1]) %*% P_post_inv[,2] + P_post_inv[2,1] * (-x_post[1,i+1]) + P_post_inv[2,3] * (-x_post[3,i+1]) + P_post_inv[2,4] * (-x_post[4,i+1])
    
    c <- t(-x_post[,i+1]) %*% P_post_inv[,3] + P_post_inv[3,1] * (-x_post[1,i+1]) + P_post_inv[3,2] * (-x_post[2,i+1]) + P_post_inv[3,4] * (-x_post[4,i+1])
    
    d <- t(-x_post[,i+1]) %*% P_post_inv[,4] + P_post_inv[4,1] * (-x_post[1,i+1]) + P_post_inv[4,2] * (-x_post[2,i+1]) + P_post_inv[4,3] * (-x_post[3,i+1])
    
    dvec <- c(-a,-b,-c,-d)
    
    solve.QP(Dmat,dvec,Amat,bvec=bvec)
    
    x_post[,i+1] <- solve.QP(Dmat,dvec,Amat,bvec=bvec)$solution
    
  }
  
  
}

#plotting results ####

#simple plot
par(mfrow=c(2,2))
plot(x_post[1,],type="line",sub ="unemployment gap",xlab = "",ylab = "")
plot(x_post[2,],type="line",sub ="kappa",xlab = "",ylab = "")
plot(x_post[3,],type="line",sub ="gamma",xlab = "",ylab = "")
plot(x_post[4,],type="line",sub ="theta",xlab = "",ylab = "")

#using ggplot and FRED recession bars 
#convert matrix to a wide data frame, delete starting values (first row)
x_post_data <- as.data.frame(t(x_post))
x_post_data <- x_post_data[-1,]
x_post_data$date <- c(as.character(data2$date[-(1:3)]))
x_post_data$date <- as.Date(as.POSIXct(x_post_data$date, format="%Y-%m-%d"))

colnames(x_post_data) <- c("unemploymentgap","kappa","gamma","theta","date")
x_post_data$NAIRU <- (x_post_data$unemploymentgap - data1$unemp)*(-1)

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

recessions.trim = subset(recessions.df, Peak >= min(data1$date))

png(filename = here("1_Plots/NAIRU.png") , height=350, width=350)
a <- ggplot(x_post_data) + geom_line( aes(x = date, y = NAIRU,group = 1)) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
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

png(filename = here("1_Plots/parameters.png") , height=350, width=350)
plot_grid(a, b, c, d, labels = "AUTO")
dev.off()

