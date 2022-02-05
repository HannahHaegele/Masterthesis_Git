
library(readxl)
Inflation <- read_excel(here("0_Data_Q/Inflation.xlsx"),  col_types = c("text", "text", "text", "text", "text", "text"))

Inflation$DATE <- as.Date(as.yearqtr(paste0(Inflation$YEAR, "-", Inflation$QUARTER),"%Y-%q"))
Inflation$INFPGDP1YR <- as.numeric(Inflation$INFPGDP1YR)
Inflation$INFCPI1YR <- as.numeric(Inflation$INFCPI1YR)
Inflation$INFCPI10YR <- as.numeric(Inflation$INFCPI10YR)

Inflation$INFCPI10YR[is.na(Inflation$INFCPI10YR)] <- 0

Inflation$INFPGDP1YR[19] <- (Inflation$INFPGDP1YR[18]+Inflation$INFPGDP1YR[20])/2

Inflation <- Inflation[-1 ,] %>% select(c(DATE,INFCPI10YR,INFPGDP1YR))
write_csv(Inflation, here('0_Data_Q/inflation_spf.csv'))



# inflation_expectation <- ts(as.data.frame(read_csv(here('0_Data/EXPINF10YR.csv'))))
# inflation_expectation <- aggregate(inflation_expectation, nfrequency = 4)
# 
# date <- as.data.frame((seq(as.Date("1970-01-01"), as.Date("1981-10-01"), by="quarters")))
# date <- cbind(date,rep(0,48))
# colnames(date) <- c("DATE","EXPINF10YR")
# 
# date <- rbind(date,inflation_expectation)
# write_csv(date, here('0_Data_Q/inflation_fed.csv'))

