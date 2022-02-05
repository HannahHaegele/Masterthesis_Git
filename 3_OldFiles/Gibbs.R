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


#subset of sample that contains data for all variables (expected inflation)
data1 <- filter(data,date > "1981-12-01" & date < "2021-08-01")
data1$t <- 1:nrow(data1) 
data1 <- relocate(data1, t) 

data1 <- as_tibble(data1)
write_csv(data1, here('0_Data/model_input_data.csv'))

#Gibbs ####

#Gibbs algorithm options
reps <-  12
burn <-  10
T0 <-  10 
D0 <-  0.1
tau <-  3.5e-01

#Step 1: Set prior based on training sample
y0 <-  as.vector(data1$annualized_inflation[1:T0])
x0 <- cbind(rep(1,nrow(data1)),data1$ugap,data1$relativeimportpriceinflation,data1$expect)[1:T0,]
b0 <- qr.solve(x0, y0)
e0 <-  y0-x0 %*% b0
sigma0 <- as.numeric(crossprod(e0)/10) #Variance-covariance matrix of e (obs eq)
V0 <-  sigma0 * solve(crossprod(x0)) #Variance-covariance matrix of beta (trans eq)

Q0 <-  V0*T0*tau
beta0 <- t(b0) #Initial state vector
P00 <- V0
R <- sigma0
Q <- Q0

#Remove training sample from data
Y <- data1$annualized_inflation[T0+1:nrow(data1)]
X <- cbind(rep(1,nrow(data1)),data1$ugap,data1$relativeimportpriceinflation,data1$expect)[(T0+1):nrow(data1),]

#Steps 2-4: Gibbs sampling algorithm

for(m in 1:reps) {
  
  #Step 2: Draw beta using Kalman filter, cond on R and Q
  #Step 2a: Set up matrices for the Kalman Filter
  
  ns <- ncol(beta0)
  F <- diag(ns)
  
  #Step 2b: Run Kalman filter
  #Start at t = 1
  
  x = t(X[1,])
  
  #Prediction
  beta10 <- tcrossprod(beta0,F)
  p10 <- F %*% tcrossprod(P00,F) + Q 
  yhat <-  t(tcrossprod(x,beta10))
  eta <- Y[1] - yhat
  feta <- x %*% tcrossprod(p10,x) + R
  
  #Updating
  K  <- tcrossprod(p10,x) %*% solve(feta)
  beta11 <- t(t(beta10) + tcrossprod(K,eta))
  p11 = p10 - K %*% (x %*% p10)
  
  #Continue for t = 2,3,...,T
  
  #Prediction
  
  #Updating
  
  
}





