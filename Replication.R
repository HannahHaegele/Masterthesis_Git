
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
library(ggrepel) #used for labeling scatter plot covid

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

  #interpolation of NAIRU 
  monthly = seq(NAIRU$DATE[1], tail(NAIRU$DATE,1), by="month")
  NAIRU2 <- data.frame(DATE=monthly, spline(NAIRU, method="fmm",xout = monthly)$y) 
  NAIRU <- merge(NAIRU, NAIRU2, by="DATE", all=TRUE) %>%
    select(c(DATE,spline.NAIRU..method....fmm...xout...monthly..y)) %>%
    rename(NAIRU = spline.NAIRU..method....fmm...xout...monthly..y)


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

#merge and transform data ####

data <- merge(unemployment,inflation_expectation,by.x = "DATE", by.y = "DATE", all.x = TRUE)  %>%
  merge(importpricedeflator,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(NAIRU,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(cpi,by.x = "DATE", by.y = "DATE", all.x = TRUE) %>%
  merge(covid,by.x = "DATE", by.y = "date", all.x = TRUE) %>%
  mutate(expect_yoy = expect-yoy_inflation) %>%
  mutate(relativeimportpriceinflation = importpriceinflation-annualized_inflation) %>%
  mutate(ugap = unemp-NAIRU) %>%
  rename(date = DATE) %>%
  relocate(covid, .after = date) %>%
  select(c(date,covid,unemp,expect,importpriceinflation,NAIRU,annualized_inflation,yoy_inflation,expect_yoy,relativeimportpriceinflation,ugap))

  write_csv(data, here('0_Data/model_input_data.csv'))

#subset of sample that contains data for all variables (expected inflation)
data1 <- filter(data,date > "1981-12-01" & date < "2021-08-01")
data1$t <- 1:nrow(data1) 
data1 <- relocate(data1, t) 

data1 <- as_tibble(data1)
write_csv(data1, here('0_Data/model_input_data_1982.csv'))

#identifying ARMA model based on AIC ####

#due to MCMC optimization in ConsRegArima, this loop yields different results for ARMA models, e.g. ARMA(1,3), ARMA(4,2), ARMA(4,3)
AIC = c()
max.q=12 
max.p=12
index <- 1

for(q in 0:max.q){
  
  for(p in 0:max.p){
    
    reg <- ConsRegArima(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,order = c(p, q),
                        constraints ='unemp < 0,relativeimportpriceinflation > 0,expect_yoy > 0',optimizer='mcmc',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))
    
    AIC[index] <- reg$aic
    index <- index + 1
    
  }
}

a <- ceiling(which.min(AIC)/(max.q+1)-1)

print(paste0("The AIC-optimal MA lag is ",ceiling(which.min(AIC)/(max.q+1)-1), " and the corresponding AR lag is ",which.min(AIC)-((a*(max.q+1)+1)))) 



#rolling window regressions with ARMA errors ####
for (i in 1:31) {
  
  #10 year rolling window regressions
  data <- filter(data1, t>=1+(i-1)*12 & t<=120+(i-1)*12)
  reg0 <- lm(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data) 
  reg <- ConsRegArima(annualized_inflation~unemp+relativeimportpriceinflation+expect_yoy,data=data,order = c(4, 3),
                      constraints ='unemp < 0,relativeimportpriceinflation > 0,expect_yoy > 0',optimizer='mcmc',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))

  
  X <- cbind(rep(1,nrow(data)),data$unemp,data$relativeimportpriceinflation,data$expect_yoy) #design matrix of regression model 
  
  #store coefficient estimates, sigma2 as well as variance-covariance matrices (sigma2*(X'X)^-1) for each rolling window 
  as.vector(assign(paste0("coefficients_", i), summary(reg)$coeff[1:4]))
  assign(paste0("variance_estimate_", i), sum(reg$residuals^2)/(nrow(data)-(reg$order[1]+reg$order[2]+4)))
  assign(paste0("covariance_matrix_", i), (sum(reg$residuals^2)/(nrow(data)-(reg$order[1]+reg$order[2]+4))) * solve(crossprod(X)))
  
}

#take the average of r.w. results: coefficient estimates, sigma2 as well as variance-covariance matrices

coefficients <- do.call(rbind, lapply( paste0("coefficients_", 1:31) , get) )
coefficients[,2] <- (-1)*coefficients[,2] #multiply by -1 to get positive value for kappa 
coefficients_0 <- as.vector(colMeans(coefficients))[2:4]

variance_estimate <- do.call(rbind, lapply( paste0("variance_estimate_", 1:31) , get) )
variance_estimate_0 <- as.vector(colMeans(variance_estimate))

covariances_0 <- do.call(cbind, lapply( paste0("covariance_matrix_", 1:i) , get))
covariances_0  <- array(covariances_0, dim=c(4,4,i))
covariances_0 <- apply(covariances_0 , c(1, 2), mean, na.rm = TRUE)

variance_NAIRU <- var(data1$NAIRU,na.rm = TRUE)


#EKF recursions ####

#initialization
  s = 15 
  rho = 0.9
  
  x_prior <- matrix(NA, nrow = 4, ncol = nrow(data1))
  x_post <- matrix(NA, nrow = 4, ncol = (nrow(data1)+1))
  x_post[,1] <- c(0,coefficients_0)
  
  Q <- covariances_0
  Q[1,] <- c(variance_NAIRU*s,0,0,0)
  Q[,1] <- c(variance_NAIRU*s,0,0,0)
  
  R <- variance_estimate_0 
  
  P_prior <- array(0, dim = c(4,4,nrow(data1)))
  P_post <- array(0, dim = c(4,4,nrow(data1)+1))
  
  #P_post[,,1] <- matrix(0, nrow = 4, ncol = 4)
  #P_post[,,1] <- diag(nrow = 4, ncol = 4)
  P_post[,,1] <- Q
  
  F <- diag(c(rho,1,1,1), 4, 4)
  
  h <- function(i) {
    -(x_prior[2,i])*(x_prior[1,i]) + x_prior[3,i]*data1$relativeimportpriceinflation[i] + x_prior[4,i]*data1$expect[i] + (1-x_prior[4,i])*data1$yoy_inflation[i]
    }
  
  H <- function(i) {
    t(c(-(x_prior[2,i]), -(x_prior[1,i]), data1$relativeimportpriceinflation[i], (data1$expect[i]-data1$yoy_inflation[i])))
  }

  
#Step 1: forward recursions
for (i in 1:(nrow(data1))) {
  
  #prediction step 
  x_prior[,i] <- F %*% x_post[,i]
  P_prior[,,i] <- F %*% tcrossprod(P_post[,,i],F) + Q 
  
  #update step 
  y <- data1$annualized_inflation[i]-h(i)
  S <- H(i) %*% tcrossprod(P_prior[,,i],H(i)) + R
  S_inv <- solve(S)
  K <- tcrossprod(P_prior[,,i],H(i)) %*% S_inv
  
  x_post[,i+1] <- x_prior[,i] + K %*% y
  P_post[,,i+1] <- (diag(1,4,4) - K %*% H(i)) %*% P_prior[,,i]
  
  #constraint 1 (adjustment of Kalman filter recursions when updated state vector does not satisfy inequality constraint)
  if (x_post[2,i+1]<0 | x_post[3,i+1]<0 | x_post[4,i+1]<0 | x_post[4,i+1]>1) {
    
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
    
    P_post_inv <- solve(P_post[,,i+1])
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
  if (sum(diag(P_post[,,i+1]) > diag(Q))>0) {
    print(paste0("ML estimates of shock variances are larger than initial estimates obtained from rolling regressions at iteration ",i))
    P_post[,,i+1] <- Q
  }
}  
  
#Step2: backward recursions

  x_post_smooth <- matrix(NA, nrow = 4, ncol = (nrow(data1)-1))
  P_post_smooth <- array(0, dim = c(4,4,nrow(data1)))

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

   }

#preparing output ####

#data for filtered estimates
  x_post_data <- as.data.frame(t(x_post))
  x_post_data <- x_post_data[-1,]
  
  x_post_data$date <- c(as.character(data1$date))
  x_post_data$date <- as.Date(x_post_data$date, format="%Y-%m-%d")
  
  colnames(x_post_data) <- c("unemploymentgap","kappa","gamma","theta","date")
  x_post_data$NAIRU <- (x_post_data$unemploymentgap - data1$unemp)*(-1)
  x_post_data$unemp <- data1$unemp
  x_post_data$annualizedinflation <- data1$annualized_inflation
  x_post_data$yoy_inflation <- data1$yoy_inflation
  x_post_data$ugap <- data1$ugap

  x_post_data$inflation <- c()
  for (i in 1:nrow(x_post_data)) {
    x_post_data$inflation[i] <- (x_post_data[i,1])*(-x_post_data[i,2]) + x_post_data[i,3]*data1$relativeimportpriceinflation[i] + x_post_data[i,4]*data1$expect[i] + (1-x_post_data[i,4])*data1$yoy_inflation[i]
  }
  
#data for smoothed estimates  
  x_post_smooth_data <- as.data.frame(t(x_post_smooth))
  
  x_post_smooth_data$date <- c(as.character(data1$date))[-(length(data1$date))]
  x_post_smooth_data$date <- as.Date(x_post_smooth_data$date, format="%Y-%m-%d")
  
  colnames(x_post_smooth_data) <- c("unemploymentgap","kappa","gamma","theta","date")
  x_post_smooth_data$NAIRU <- (x_post_smooth_data$unemploymentgap - data1$unemp[-(length(data1$date))])*(-1)
  x_post_smooth_data$unemp <- data1$unemp[-(length(data1$date))]
  x_post_smooth_data$annualizedinflation <- data1$annualized_inflation[-(length(data1$date))]
  x_post_smooth_data$yoy_inflation <- data1$yoy_inflation[-(length(data1$date))]
  x_post_smooth_data$ugap <- data1$ugap[-(length(data1$date))]
  
  x_post_smooth_data$inflation <- c()
  for (i in 1:nrow(x_post_smooth_data)) {
    x_post_smooth_data$inflation[i] <- (x_post_smooth_data[i,1])*(-x_post_smooth_data[i,2]) + x_post_smooth_data[i,3]*data1$relativeimportpriceinflation[i] + x_post_smooth_data[i,4]*data1$expect[i] + (1-x_post_smooth_data[i,4])*data1$yoy_inflation[i]
  }
  
  #recession dates for recession bars in plots (source: https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/)
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

#filtered estimates 
a <- ggplot(x_post_data) + geom_line( aes(x = date, y = NAIRU,group = 1,colour='NAIRU')) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = unemp,group = 1,colour='unemployment rate')) +
    scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment rate"="blue")) 

#smoothed estimates 
a <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = NAIRU,group = 1,colour='NAIRU')) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
  geom_line(aes(x = date, y = unemp,group = 1,colour='unemployment rate')) +
  scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment rate"="blue")) 

#plotting upgap #### 

#smoothed estimates 
a <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = unemploymentgap,group = 1,colour='predicted ugap')) + theme_bw()
a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
  geom_line(aes(x = date, y = ugap,group = 1,colour='empirical ugap')) +
  scale_colour_manual(" ", values = c("predicted ugap" ="red", "empirical ugap"="blue"))


#plotting kappa ####

#filtered estimates 
b <- ggplot(x_post_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#smoothed estimates 
b <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#plotting gamma #####

#filtered estimates 
c <- ggplot(x_post_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#smoothed estimates 
c <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#plotting theta #####

#filtered estimates 
d <- ggplot(x_post_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#smoothed estimates 
d <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)


#plotting predicted/actual inflation #####

#filtered estimates 
e <- ggplot(x_post_data) + geom_line( aes(x = date, y = annualizedinflation,group = 1,colour='actual inflation')) + theme_bw()
e + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = inflation,group = 1,colour='predicted inflation')) + 
    scale_colour_manual(" ", values = c("actual inflation" ="red", "predicted inflation"="blue")) 

#smoothed estimates 
e <- ggplot(x_post_smooth_data) + geom_line( aes(x = date, y = annualizedinflation,group = 1,colour='actual inflation')) + theme_bw()
e + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
  geom_line(aes(x = date, y = inflation,group = 1,colour='predicted inflation')) + 
  scale_colour_manual(" ", values = c("actual inflation" ="red", "predicted inflation"="blue")) 

#save plots ARMA ####
  
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
  
  png(filename = here("1_Plots/NAIRU_smooth.png") , height=350, width=350)
  a <- ggplot(x_post_data) + geom_line( aes(x = date, y = NAIRU,group = 1,colour='NAIRU')) + theme_bw()
  a + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = unemp,group = 1,colour='unemployment rate')) +
    scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment"="blue")) 
  dev.off()
  
  png(filename = here("1_Plots/kappa_smooth.png") , height=350, width=350)
  b <- ggplot(x_post_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
  b + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
  dev.off()
  
  png(filename = here("1_Plots/gamma_smooth.png") , height=350, width=350)
  c <- ggplot(x_post_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
  c + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
  dev.off()
  
  png(filename = here("1_Plots/theta_smooth.png") , height=350, width=350)
  d <- ggplot(x_post_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
  d + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)
  dev.off()
  
  png(filename = here("1_Plots/fit_smooth.png") , height=350, width=350)
  e <- ggplot(x_post_data) + geom_line( aes(x = date, y = annualizedinflation,group = 1,colour='actual inflation')) + theme_bw()
  e + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) +
    geom_line(aes(x = date, y = inflation,group = 1,colour='predicted inflation')) + 
    scale_colour_manual(" ", values = c("actual inflation" ="red", "predicted inflation"="blue")) 
  dev.off()
