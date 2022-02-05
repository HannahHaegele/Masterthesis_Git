##------------------------------------------------------------------------------##
## File:        prepare.data.quarterly
##
## Description: This file (1) compiles and (2) prepares the data used.
##
##------------------------------------------------------------------------------##
rm(list = ls())

##------------------------------------------------------------------------------##
## Get Raw Data
##------------------------------------------------------------------------------##
filenames <-  list.files(path = here('0_Data_Q'), pattern="*.csv",full.names=TRUE)

data <- lapply(filenames, read.csv) 
data <- as.data.frame(Reduce(merge, data))
colnames(data) <- c("date", "imports","core.imports","headline.cpi.index", "expected.inf.spf10","expected.inf.spf1", "core.pce.index","urate","NAIRU")

#Unemployment Rate: Aged 15-64: All Persons for the United States (LRUN64TTUSQ156S)
#Personal Consumption Expenditures: Chain-type Price Index Less Food and Energy (JCXFE)
#Consumer Price Index: Total All Items for the United States (CPALTT01USQ661S)
#Imports of nonpetroleum goods (chain-type price index) (B187RG3Q086SBEA)
#Imports of all goods (implicit price deflator) (A255RD3Q086SBEA)
#10-Year Expected Inflation (EXPINF10YR) 

## Set the start and end dates of the data used in the estimation
data.start <- c(year(data$date[1]), quarter(data$date[1]))
data.end   <- c(year(data$date[length(data$date)]), quarter(data$date[length(data$date)]))

## Define variables as tis objects
for (var in colnames(data)[2:length(colnames(data))]) {
  assign(var, tis(data[,var], start = data.start, tif = 'quarterly'))
}


##------------------------------------------------------------------------------##
## Prepare Data
##------------------------------------------------------------------------------##

## Create annualized inflation series using the headline cpi index 
inflation <- 400*log(headline.cpi.index/Lag(headline.cpi.index, k=1))

## Create year-ended inflation (yoy-inflation,four-quarter-ended inflation term), lagged one quarter
inflation.yoy.unlagged <- 100*log(headline.cpi.index/Lag(headline.cpi.index, k=4))
inflation.yoy <- Lag(100*log(headline.cpi.index/Lag(headline.cpi.index, k=4)), k=1)

## ROBUSTNESS CHECK: pce index
inflation.1 <- 400*log(core.pce.index/Lag(core.pce.index, k=1))

## ROBUSTNESS CHECK:  average of past 4 quarters of inflation (Blanchard paper)
inflation.yoy.1 <- (Lag(inflation, k=1)+Lag(inflation, k=2)+Lag(inflation, k=3)+Lag(inflation, k=4))/4

## ---

## Import price inflation (yoy-inflation, all goods)
import.price.inflation <- 100*log(imports/Lag(imports, k=4))

## Create relative import price inflation  (yoy-inflation, all goods)
relative.import.price.inflation <- import.price.inflation  - inflation.yoy.unlagged 

## ROBUSTNESS CHECK:  quarterly rates, all goods
import.price.inflation.1 <- 400*log(imports/Lag(imports, k=1))

## ROBUSTNESS CHECK: quarterly rates, all goods 
relative.import.price.inflation.1 <- import.price.inflation - inflation

## ROBUSTNESS CHECK: quarterly rates, non-petroleum goods
core.import.price.inflation <- 400*log(core.imports/Lag(core.imports, k=1))

##  ROBUSTNESS CHECK: quarterly rates, non-petroleum goods
relative.core.import.price.inflation <- core.import.price.inflation - inflation

## ---

## Splice expected inflation in 1991:Q3
#beginning of data - 1991 Q3: INFPGDP1YR (one-year-ahead expectations of inflation measured by the GNP/GDP price index)
#1991 Q4 - end of data: INFCPI10YR (annual-average inflation over the next 10 years measured by CPI)
expected.inf <- mergeSeries(window(expected.inf.spf1, end = c(1991,3)),
                         window(expected.inf.spf10, start = c(1991,4)))

## Create expected-yoy.inflation variable 
expected.yoy.inflation <- expected.inf-inflation.yoy

## ---

## Create original gap estimate 
gap <- urate-NAIRU

## ---

## Create lagged unemployment
l.urate <- Lag(urate, k=1)


## ---


## Create Date and index
date <- (seq(as.Date("1971-07-01"), as.Date("2021-07-01"), by="quarters"))
t <- 1:length(date) 

##------------------------------------------------------------------------------##
## Output Data
##------------------------------------------------------------------------------##
data.out <- window(cbind(t,date,inflation,
                         inflation.yoy,
                         relative.import.price.inflation,
                         expected.inf,
                         expected.yoy.inflation,
                         urate,l.urate, NAIRU,gap),
                   start = data.start)

write.table(data.out,file = here('2_Files/model_input_data.csv'), sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)


