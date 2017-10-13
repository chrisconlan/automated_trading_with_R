###########################################
### This script is a one-piece runnable ###
### example constructed from code       ###
### using Quandl API instead of yahoo!  ###
### It is Windows and	UNIX	        ###
### compatible.				            ###
###########################################


# The goal is use Quandl API instead of Yahoo! API
# for downloading and Yahoo! YQL for updating
# it is intended to reproduce the same same steps
# as stated on the text book.
# The end result is a DATA file similar to
# listings in Chapter 2
# Quandl https://www.quandl.com/
# Quandl API r https://www.quandl.com/tools/r
# Quandl doocs https://www.quandl.com/tools/r
# Quandl has the capability of downloading data frames as zoo objects
# therefore, we will re-name each column after downloading and writing as csv

# set root as working directory, change it to your wd
setwd("~")

# delete AutoTrading folder if exists. We will start fresh
if(file.exists("./AutoTrading")) {
    unlink("./AutoTrading", recursive=TRUE)
}
# create AutoTrading folders
dir.create("./AutoTrading")
dir.create("./AutoTrading/stockdata")
dir.create("./Autotrading/functions")

####Listing 2.1: Setting Path Variables
rootdir <- "~/Autotrading"
datadir <- "~/AutoTrading/stockdata/"
functiondir <- "~/AutoTrading/functions/"
####

####Listing 2.2 modified for quandl instead of Yahoo!
#insert your Quandl APO here
require(Quandl)
quandl_api = "MYAPIKEY"

#add my key to Quandl API
Quandl.api_key(quandl_api)

# this function downloads the columns needed as from start_date
quandl_get <- function(sym, start_date = "2017-01-01") {
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
####


# save quandl.R file in /functions with 'quandl'_get function
setwd(functiondir)
dump(list = c("quandl_get"), "quandl.R")
#S <- read.csv(url, header = FALSE, stringsAsFactors=F)[,1]

####Listing 2.3: List of S&P 500 Stocks####
# Up-to-date at time of writing

url <- "http://trading.chrisconlan.com/SPstocks.csv"

# Option A: Read S from url
# S <- as.character(read.csv(url, header = FALSE)[,1])

# Option B: for testing read a S-test.R CSV file with shorter stock tickers
S <- c("MMM", "ACN", "BLK", "HRB", "BWA", "BF-B")

#Change '-' for '_'. Quandl needs it
#Method 1, generic
S <- gsub("-", "_", S)

setwd(rootdir)
dump(list = "S", "S.R")
#####

#### 2.4
# Load "invalid.R" file if available
invalid <- character(0)
setwd(rootdir)
if("invalid.R" %in% list.files()) source("invalid.R")


# Find all symbols not in directory and not missing
setwd(datadir)
toload <- setdiff(S[!paste0(S, ".csv") %in% list.files()], invalid)

#load new column names
column_names <- c("Open", "High", "Low", "Close", "Volume")

# Fetch symbols with quandl_get function, save as .csv or missing
source(paste0(functiondir, "quandl.R"))
if(length(toload) != 0){
  for(i in 1:length(toload)){

  df <- quandl_get(toload[i])

  if(!is.null(df)) {
      #changing names
      colnames(df) <- column_names
      # as zoo objects downloaded, row names must be TRUE. Use write ZOO
    write.zoo(df, file = paste0(toload[i], ".csv"))
  } else {
    invalid <- c(invalid, toload[i])
  }

}
}

setwd(rootdir)
dump(list = c("invalid"), "invalid.R")

# Clears R environment except for path variables and functions
rm( list = setdiff( ls(), c(" rootdir", "functiondir", "datadir", "quandl_get", "column_names")))
gc()
####

#### 2.5
setwd(datadir)
S <- sub(".csv", "", list.files())

require(data.table)

DATA <- list()
for(i in S){
  suppressWarnings(
  # read as Zoo instead of fread
  # DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- read.zoo(paste0(i, ".csv"), header = TRUE)
  )
  # sort by index
  DATA[[i]] <- zoo(DATA[[i]], order.by = index(DATA[[i]]))
}
####

#### 2.6 update method with quandl_get function
# To prove that this works, at this point you might want to delete some rows in
# any of the csv files under stockdata.
# force system time to "EST"
Sys.setenv(TZ="EST")
currentTime <- Sys.time()

for(i in S){
  # Store greatest date within DATA for symbol i
  maxdate <- max(index(DATA[[i]])[nrow(DATA[[i]])])
  if(as.numeric(difftime(currentTime, maxdate, units = "hours")) >= 40.25){

    # Push the maxdate forward one day
    maxdate <- strptime(maxdate, "%Y-%m-%d") + 86400

    weekend <- sum(c("Saturday", "Sunday") %in%
                     weekdays(c(maxdate, currentTime))) == 2

    if(!weekend){
        # if !weekend then start_date for quandl = maxdate
        start_date = as.character(maxdate)
        df <- quandl_get(i, start_date = start_date)
        colnames(df) <- column_names
      if(!is.null(df)){
        if(all(!is.na(df)) & nrow(df) > 0){
          # df <- df[nrow(df):1] # not needed, is type = "zoo"
          # write csv file with new data, duplicates might exist
          write.zoo(df, file = paste0(i, ".csv"),
                        row.names = FALSE, col.names = FALSE, append = TRUE)
          DATA[[i]] <- rbind(DATA[[i]], df)
          # just in case, sort by index. Remove duplicates?
          DATA[[i]] <- zoo(DATA[[i]], order.by = index(DATA[[i]]))
        }
      }
    }
  }
}

#### 2.7 method not needed
####
