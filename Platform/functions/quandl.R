# Source : https://docs.quandl.com/docs/time-series-1
# Examples
# You can get the same data in a dataframe: data <- Quandl("FRED/GDP", type="raw")
# In ts format: data_ts <- Quandl("FRED/GDP", type="ts")
# In xts format: data_xts <- Quandl("FRED/GDP", type="xts")
# In zoo format: data_zoo <- Quandl("FRED/GDP", type="zoo")
# data <- Quandl(c("FRED/GDP.1", "WIKI/AAPL.4"))
# AAPL <- Quandl("WIKI/AAPL")
# data <- Quandl("WIKI/AAPL.4")
# data_NSE_OIL <- Quandl('NSE/OIL', type = "raw")
# data_gdp_aapl <- Quandl(c("FRED/GDP.1", "WIKI/AAPL.4"))
# data_acn_aapl <- Quandl(c("WIKI/ACN", "WIKI/AAPL.4"))
# mydata = Quandl("FRED/GDP", start_date="2001-12-31", end_date="2005-12-31")
# mydata_columns <- Quandl(c("WIKI/AAPL.8", "WIKI/AAPL.9"), start_date="2017-01-01")

#quandl API
quandl_api = "MYAPIKEY"

#add my key
Quandl.api_key(quandl_api)

quandl_get <-
function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
        paste0("WIKI/", sym, ".8"),  #  Adj. Open
        paste0("WIKI/", sym, ".9"),  # Adj. High
        paste0("WIKI/", sym, ".10"), # Adj. Low
        paste0("WIKI/", sym, ".11"), # Adj. Close
        paste0("WIKI/", sym, ".12")), # Adj. Volume
        start_date = start_date,
        type = "zoo"
        ))
}
