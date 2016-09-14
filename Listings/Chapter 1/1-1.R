# Checks if quantmod is installed, installs it if unavailable,
# loads it and turns off needless warning messages
if(!("quantmod" %in% as.character(installed.packages()[,1])))
  { install.packages("quantmod") } 
library(quantmod)
options("getSymbols.warning4.0"=FALSE, 
        "getSymbols.auto.assign"=FALSE)

# Loads S&P 500 ETF data, stores closing prices as a vector
SPY <- suppressWarnings(
  getSymbols(c("SPY"),from = "2012-01-01"))
SPY <- as.numeric(SPY$SPY.Close)[1:987]

