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

source(here('2_Files/unpack.parameters.R'))
source(here('2_Files/kalman.log.likelihood.R'))
source(here('2_Files/log.likelihood.wrapper.R'))

source(here('2_Files/kalman.states.R'))
source(here('2_Files/kalman.states.wrapper.R'))
source(here('2_Files/format.output.R'))


##------------------------------------------------------------------------------##
## Read in data 
##------------------------------------------------------------------------------##
data1 <- read.table(here('2_Files/model_input_data.csv'),
                    sep=',',header=TRUE,stringsAsFactors=FALSE, na.strings=".")


## Get data in vector form beginning at est.data.start (set above)
data1 <- data.frame(lapply(data1, function(x) as.numeric(as.character(x))))
data1$date <- as.Date(data1$date)

##------------------------------------------------------------------------------##
## 1. Step: rolling window regressions
##------------------------------------------------------------------------------##

roll.coeff <- matrix(NA, nrow = 3, ncol = 41)
roll.var <- matrix(NA, nrow = 3, ncol = 41)
roll.vare <- c()

for (i in 1:41) {
  
  data <- filter(data1, t>=1+(i-1)*4 & t<=40+(i-1)*4)
  
  mean.urate <- mean(data$urate)
  approx.gap <- mean.urate-data$urate
  X <- cbind(approx.gap,data$relative.import.price.inflation,data$expected.yoy.inflation) 
  
  lower <- c(0,0,0)
  upper <- c(Inf,1,1)
  
  if (i == 1) {
    
    start <- list(x1 = 1, x2 = 0, x3 = 0)
  
    nls.out <- nls(data$inflation ~ (x1*approx.gap + x2*data$relative.import.price.inflation + x3*data$expected.inf + (1-x3)*data$inflation.yoy) ,
               data=data, start=start, lower=lower, upper=upper, algorithm="port")
  
    roll.coeff[,i] <-  summary(nls.out)$coefficients[1:3]
    roll.var[,i] <- diag(sum(summary(nls.out)$residuals^2)/(nrow(data)-3)* solve(crossprod(X)))
    roll.vare[i] <- sum(summary(nls.out)$residuals^2)/(nrow(data)-3)
  
  } else {
    
    start <- list(x1 = roll.coeff[1,i-1], x2 = roll.coeff[2,i-1], x3=roll.coeff[3,i-1])
    
    nls.out <- nls(data$inflation ~ (x1*approx.gap + x2*data$relative.import.price.inflation + x3*data$expected.inf + (1-x3)*data$inflation.yoy) ,
                   data=data, start=start, lower=lower, upper=upper, algorithm="port")
    
    roll.coeff[,i] <-  summary(nls.out)$coefficients[1:3]
    roll.var[,i] <- diag(sum(summary(nls.out)$residuals^2)/(nrow(data)-3)* solve(crossprod(X)))
    roll.vare[i] <- sum(summary(nls.out)$residuals^2)/(nrow(data)-3)
    
  }
}

##------------------------------------------------------------------------------##
## Step 2: ML hyper-parameters 
##------------------------------------------------------------------------------##

initial.parameters <- c(  mean(roll.vare), 0.9,
                         mean(roll.var[1,]),
                         mean(roll.var[2,]),
                         mean(roll.var[3,]),
                         var(diff(data$urate))*(1/(15+1)),
                         var(diff(data$urate))*(1-(1/(15+1))))

#function to be minimized 
f <- function(omega) {return(-log.likelihood.wrapper(omega)$ll.cum)}

#Constraints
bounds <- matrix(c(
  0,1,
  0,1,
  0,initial.parameters[3],
  0,initial.parameters[4],
  0,initial.parameters[5],
  0,1,
  0,1
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



#Get parameter estimates via maximum likelihood
f <- function(omega) {return(-log.likelihood.wrapper(omega)$ll.cum)}

lb <- c(0,0,0,0,0,0,0)
ub <- c(Inf,1,initial.parameters[3],initial.parameters[4],initial.parameters[5],Inf,Inf)

nloptr.out <- nloptr(initial.parameters, f, eval_grad_f=function(x) {gradient(f, x)},
                     lb=lb, ub=ub, opts=list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=200))

omega <- nloptr.out$solution


##------------------------------------------------------------------------------##
## Step 3: ML state variables 
##------------------------------------------------------------------------------##

states.fitered <- kalman.states.wrapper(omega)$x_post
states.smoothed <- kalman.states.wrapper(omega)$x_post_smooth

output <- format.output.1b(states.smoothed)


#kappa 
b <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = kappa,group = 1)) + theme_bw()
b + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#gamma 
c <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = gamma,group = 1)) + theme_bw()
c + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

#theta
d <- ggplot(output$x_post_smooth_data) + geom_line( aes(x = date, y = theta,group = 1)) + theme_bw()
d + geom_rect(data=output$recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="grey", alpha=0.2)

