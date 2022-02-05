##----------------------------------------------------------------------------------##
##
## Description: This is the main file with quarterly data!! 
##----------------------------------------------------------------------------------##
rm(list=ls())

##------------------------------------------------------------------------------##
## Load required packages and source all programs to be used in  estimation.
##------------------------------------------------------------------------------##
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
library("dynlm") #for time series regressions
library("nloptr") #nonlinear optimisation
library(tis)
library(matrixcalc) #for the test of a matrix being singular
library(pracma)


source(here('2_Files/gradient.R'))

source(here('2_Files/rolling.window.1.R'))
source(here('2_Files/unpack.parameters.1.R'))
source(here('2_Files/kalman.states.1.R'))
source(here('2_Files/kalman.states.wrapper.1.R'))
source(here('2_Files/format.output.1.R'))

source(here('2_Files/unpack.parameters.1b.R'))
source(here('2_Files/kalman.states.1b.R'))
source(here('2_Files/kalman.states.wrapper.1b.R'))
source(here('2_Files/format.output.1b.R'))

source(here('2_Files/kalman.log.likelihood.1b.R'))
source(here('2_Files/log.likelihood.wrapper.1b.R'))

source(here('2_Files/unpack.parameters.2.R'))
source(here('2_Files/kalman.states.2.R'))
source(here('2_Files/kalman.states.wrapper.2.R'))
source(here('2_Files/format.output.2.R'))

source(here('2_Files/kalman.log.likelihood.2.R'))
source(here('2_Files/log.likelihood.wrapper.2.R'))



##------------------------------------------------------------------------------##
## Read in data 
##------------------------------------------------------------------------------##
data1 <- read.table(here('2_Files/model_input_data.csv'),
                   sep=',',header=TRUE,stringsAsFactors=FALSE, na.strings=".")


## Get data in vector form beginning at est.data.start (set above)
data1 <- data.frame(lapply(data1, function(x) as.numeric(as.character(x))))
data1$date <- as.Date(data1$date)

##------------------------------------------------------------------------------##
## rolling window regressions (using the original gap estimate)
##------------------------------------------------------------------------------##

#rolling window regressions  ####
for (i in 1:41) {
  
  data <- filter(data1, t>=1+(i-1)*4 & t<=40+(i-1)*4)
  
  reg0 <- lm(inflation ~ gap + relative.import.price.inflation + expected.yoy.inflation,data=data) 
  
  reg <- ConsReg(inflation ~ gap + relative.import.price.inflation + expected.yoy.inflation ,data=data,
                      constraints ='gap < 0, relative.import.price.inflation > 0, expected.yoy.inflation > 0',optimizer='mcmc',family='gaussian',ini.pars.coef = c(reg0$coeff[1],-0.5,0.1,0.2))

  
  X <- cbind(rep(1,nrow(data)),data$gap,data$relative.import.price.inflation,data$expected.yoy.inflation) #design matrix of regression model 
  
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
variance_gap <- var(data1$gap)


##------------------------------------------------------------------------------##
## Model 1: one measurement equations, output gap as state variable 
##------------------------------------------------------------------------------##

s = 15 
initial.parameters <- c(0.9,variance_NAIRU*s, variance_estimate_0, variance_NAIRU,covariances_0[2,2],covariances_0[3,3],covariances_0[4,4])

states.fitered <- kalman.states.wrapper.1(initial.parameters)$x_post
states.smoothed <- kalman.states.wrapper.1(initial.parameters)$x_post_smooth

output <- format.output.1(states.smoothed)

#NAIRU and unemployment gap 
a <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = (unemploymentgap-urate)*(-1),group = 1)) + theme_bw()
a + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

a <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = unemploymentgap,group = 1)) + theme_bw()
a + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#kappa 
b <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#gamma 
c <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#theta
d <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

##------------------------------------------------------------------------------##
## Model 1b: one measurement equation, NAIRU as state variable 
##------------------------------------------------------------------------------##

s = 15 
initial.parameters <- c(0.9, variance_gap, variance_estimate_0, covariances_0[2,2],covariances_0[3,3],covariances_0[4,4])

states.fitered <- kalman.states.wrapper.1b(initial.parameters)$x_post
states.smoothed <- kalman.states.wrapper.1b(initial.parameters)$x_post_smooth

output <- format.output.1b(states.smoothed)

#NAIRU 
a <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = NAIRU ,group = 1, colour='NAIRU')) + theme_bw()
a + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2) + 
    geom_line(aes(x = date, y = urate ,group = 1, colour='unemployment rate')) +
    scale_colour_manual(" ", values = c("NAIRU" ="red", "unemployment rate"="blue")) 

#kappa 
b <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#gamma 
c <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#theta
d <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)


##------------------------------------------------------------------------------##
## Model 1b: ML hyper-parameters 
##------------------------------------------------------------------------------##

#Get parameter estimates via maximum likelihood
#function to be minimized 
f <- function(omega) {return(-log.likelihood.wrapper.1b(omega)$ll.cum)}

#Constraints
bounds <- matrix(c(
  0,1,
  0,initial.parameters[2],
  0,initial.parameters[3],
  0,initial.parameters[4],
  0,initial.parameters[5],
  0,initial.parameters[6]
), nc=2, byrow=TRUE)
colnames(bounds) <- c("lower", "upper")

# Convert the constraints to the ui and ci matrices
n <- nrow(bounds)
ui <- rbind( diag(n), -diag(n) )
ci <- c( bounds[,1], - bounds[,2] )

# Remove the infinite values
i <- as.vector(is.finite(bounds))
ui <- ui[i,]
ci <- ci[i]

# # Print the constraints
# k <- length(ci)
# n <- dim(ui)[2]
# for(i in seq_len(k)) {
#   j <- which( ui[i,] != 0 )
#   cat(paste( ui[i,j], " * ", "x[", (1:n)[j], "]", sep="", collapse=" + " ))
#   cat(" >= " )
#   cat( ci[i], "\n" )
# }

#constrained minimization (subtracting a fuzz value from the ci argument)
optim.out <- constrOptim(initial.parameters, f, grad=function(x) {gradient(f, x)}, ui=ui, ci=ci-1e-6, method = "BFGS")
omega <- optim.out$par

initial.parameters <- omega

##------------------------------------------------------------------------------##
## Model 2: two measurement equations, NAIRU and lagged NAIRU as state variables 
##------------------------------------------------------------------------------##
s = 15 
initial.parameters <- c(0.9, variance_gap, variance_estimate_0, covariances_0[2,2],covariances_0[3,3],covariances_0[4,4])

states.fitered <- kalman.states.wrapper.2(initial.parameters)$x_post
states.smoothed <- kalman.states.wrapper.2(initial.parameters)$x_post_smooth

output <- format.output.2(states.smoothed)


#predicted NAIRU and unemployment gap 
a <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = NAIRU,group = 1)) + theme_bw()
a + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

a <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = urate-NAIRU,group = 1)) + theme_bw()
a + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#kappa 
b <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#gamma 
c <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#theta
d <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

##------------------------------------------------------------------------------##
## Model 2: ML hyper-paramters 
##------------------------------------------------------------------------------##

#Get parameter estimates via maximum likelihood
#function to be minimized 
f <- function(omega) {return(-log.likelihood.wrapper.2(omega)$ll.cum)}

#Constraints
bounds <- matrix(c(
  0,1,
  0,initial.parameters[2],
  0,initial.parameters[3],
  0,initial.parameters[4],
  0,initial.parameters[5],
  0,initial.parameters[6]
), nc=2, byrow=TRUE)
colnames(bounds) <- c("lower", "upper")

# Convert the constraints to the ui and ci matrices
n <- nrow(bounds)
ui <- rbind( diag(n), -diag(n) )
ci <- c( bounds[,1], - bounds[,2] )

# Remove the infinite values
i <- as.vector(is.finite(bounds))
ui <- ui[i,]
ci <- ci[i]

# Print the constraints
k <- length(ci)
n <- dim(ui)[2]
for(i in seq_len(k)) {
  j <- which( ui[i,] != 0 )
  cat(paste( ui[i,j], " * ", "x[", (1:n)[j], "]", sep="", collapse=" + " ))
  cat(" >= " )
  cat( ci[i], "\n" )
}

#constrained minimization (subtracting a fuzz value from the ci argument)
optim.out <- constrOptim(initial.parameters, f, grad=function(x) {gradient(f, x)}, ui=ui, ci=ci-1e-6, method = "BFGS")
omega <- optim.out$par

initial.parameters <- omega



##------------------------------------------------------------------------------##
## EM algorithm 
##------------------------------------------------------------------------------##

## Set an upper and lower bound on the parameter vectors: 
omega.lb <- c(rep(0,length(initial.parameters)))
omega.ub <- c(1, rep(Inf, length(initial.parameters)-1))


#Get parameter estimates via maximum likelihood
f <- function(omega) {return(-log.likelihood.wrapper.1b(omega)$ll.cum)}

nloptr.out <- nloptr(initial.parameters, f, eval_grad_f=function(x) {gradient(f, x)},
                     lb=omega.lb, ub=omega.ub, opts=list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=200))

omega <- nloptr.out$solution


library(minpack.lm)

nlsLM(inflation ~ -gap + relative.import.price.inflation + expected.yoy.inflation,data=data, 
      start = list(a = 0.5, b = 0.1, c=0.2), lower = c(0,0,0), upper = c(0,0,1))

