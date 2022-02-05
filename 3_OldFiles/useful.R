####USEFUL CODE CHUNKS####

#seasonal adjustment ####
annual_inflation0<-ts(annual_inflation[,-1],frequency=12,start=c(1947,1))
annual_inflation0.seas<-seas(annual_inflation0)

importpricedeflator0<-ts(importpricedeflator[,-1],frequency=12,start=c(1960,1))
importpricedeflator0.seas<-seas(importpricedeflator0)
plot(importpricedeflator0.seas) 

inflationexpectation0<-ts(inflation_expectation[,-1],frequency=12,start=c(1982,1))
inflationexpectation0.seas<-seas(inflationexpectation0)
inflation_expectation <- final(inflationexpectation0.seas)
plot(inflationexpectation0.seas)
summary(inflationexpectation0.seas)
qs(inflationexpectation0.seas)

seas(unemp)
plot(seas(unemp))


#testing inflation expectation ts for seasonal adjustment ####

inflationexpectation0<-ts(inflation_expectation[,-1],frequency=12,start=c(1982,1))
inflationexpectation0.seas<-seas(inflationexpectation0)
qs(inflationexpectation0.seas)

#there is no evidence of seasonality in the original and hence the time series is not adjusted

#positive definiteness ####

mat <- matrix(c(2,-6,-6,2),2,2)
eigen(mat) #not pos. def. since mat has eigenvalues < 0 

library(matrixcalc)
is.positive.definite(mat, tol=1e-8)

#approximation of nearest positive definite matrix
library(Matrix)
mat1 <- nearPD(mat)
eigen(mat1$mat) 


#autocorrelated errors ####

library(lmtest)
bgtest(reg,order=4)
acf(reg0$residuals)
pacf(reg0$residuals)

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


