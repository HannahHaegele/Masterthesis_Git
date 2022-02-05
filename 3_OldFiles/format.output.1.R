#preparing output ####

format.output.1 <- function(states.smoothed) {
  
#data for smoothed estimates  
x_post_smooth_data <- as.data.frame(t(states.smoothed))

x_post_smooth_data$date <- c(as.character(data1$date))[-(length(data1$date))]
x_post_smooth_data$date <- as.Date(x_post_smooth_data$date, format="%Y-%m-%d")

colnames(x_post_smooth_data) <- c("unemploymentgap","kappa","gamma","theta","date")
x_post_smooth_data$NAIRU <- (x_post_smooth_data$unemploymentgap - data1$urate[-(length(data1$date))])*(-1)
x_post_smooth_data$urate <- data1$urate[-(length(data1$date))]
x_post_smooth_data$inflation <- data1$inflation[-(length(data1$date))]
x_post_smooth_data$inflation.yoy <- data1$inflation.yoy[-(length(data1$date))]
x_post_smooth_data$gap <- data1$gap[-(length(data1$date))]

x_post_smooth_data$inflation.predicted <- c()
for (i in 1:nrow(x_post_smooth_data)) {
  x_post_smooth_data$inflation.predicted[i] <- (x_post_smooth_data[i,1])*(-x_post_smooth_data[i,2]) + x_post_smooth_data[i,3]*data1$relative.core.import.price.inflation[i] + x_post_smooth_data[i,4]*data1$expected.inf[i] + (1-x_post_smooth_data[i,4])*data1$inflation.yoy[i]
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

recessions.trim = as.data.frame(subset(recessions.df, Trough >= min(data1$date)))

return(list("x_post_smooth_data"=x_post_smooth_data,"recessions.trim"=recessions.trim))
}



