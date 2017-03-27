###########################################
### This script is a one-piece runnable ###
### example constructed from code in    ###
### the text. It is Windows and	UNIX	###
### compatible.				###
###########################################


# The goal is to generate a cross-validated
# equity curve based on Nelder-Mead optimization
# using the long-Only MACD strategy by
# using the most possible availabe data to
# optimize at each year. i.e. 2012 uses data
# from 2000 through 2011. This will take considerable
# time to run. It projects returns in excess of
# buy-and-hold returns for SPY over the approx.
# 11 years it trades.


####Listing 2.1: Setting Path Variables####
rootdir <- "~/AutoTrading/"
datadir <- "~/AutoTrading/stockdata/"
functiondir <- "~/AutoTrading/functions/"
####

####Listing 2.2: Yahoo! Finance CSV API Function####
yahoo <- function(sym, current = TRUE,
                  a = 0, b = 1, c = 2000, d, e, f,
                  g = "d")
{
  if(current){
    f <- as.numeric(substr(as.character(Sys.time()), start = 1, stop = 4))
    d <- as.numeric(substr(as.character(Sys.time()), start = 6, stop = 7)) - 1
    e <- as.numeric(substr(as.character(Sys.time()), start = 9, stop = 10))
  }
  require(data.table)
  tryCatch(
    suppressWarnings(
      fread(paste0("http://ichart.yahoo.com/table.csv",
                   "?s=", sym,
                   "&a=", a,
                   "&b=", b,
                   "&c=", c,
                   "&d=", d,
                   "&e=", e,
                   "&f=", f,
                   "&g=", g,
                   "&ignore=.csv"), sep = ",")),
    error = function(e) NULL
  )
}
setwd(functiondir)
dump(list = c("yahoo"), "yahoo.R")
#####



####Listing 2.3: List of S&P 500 Stocks####
# Up-to-date at time of writing
url <- "http://trading.chrisconlan.com/SPstocks.csv"
S <- as.character(read.csv(url, header = FALSE)[,1])

#S <- read.csv(url, header = FALSE, stringsAsFactors=F)[,1]
#S2 = fread(url, header = FALSE)
setwd(rootdir)
dump(list = "S", "S.R")
#####



####Listing 2.4: Initial Directory Loader####
# Load "invalid.R" file if available
invalid <- character(0)
setwd(rootdir)
if("invalid.R" %in% list.files()) source("invalid.R")

# Find all symbols not in directory and not missing
setwd(datadir)
toload <- setdiff(S[!paste0(S, ".csv") %in% list.files()], invalid)

# Fetch symbols with yahoo function, save as .csv or missing
source(paste0(functiondir, "yahoo.R"))
if(length(toload) != 0){
  for(i in 1:length(toload)){
    df <- yahoo(toload[i])
    if(!is.null(df)) {
      write.csv(df[nrow(df):1], file = paste0(toload[i], ".csv"),
                row.names = FALSE)
    } else {
      invalid <- c(invalid, toload[i])
    }
  }}
setwd(rootdir)
dump(list = c("invalid"), "invalid.R")
################


# Clears R environment except for path variables and functions
rm(list = setdiff(ls(), c("rootdir", "functiondir", "datadir", "yahoo")))


####Listing 2.5: Loading Data into Memory####
setwd(datadir)
S <- sub(".csv", "", list.files())
require(data.table)
DATA <- list()
for(i in S){
  suppressWarnings(
    DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- (DATA[[i]])[order(DATA[[i]][["Date"]], decreasing = FALSE)]
}
#####


####Listing 2.6: CSV Update Method####
for(i in S){
  maxdate <- DATA[[i]][["Date"]][nrow(DATA[[i]])]
  if(as.numeric(difftime(Sys.time(), maxdate, units = "hours")) >= 40.25){
    maxdate <- strptime(maxdate, "%Y-%m-%d") + 86400
    weekend <- sum(c("Saturday", "Sunday") %in%
                     weekdays(c(maxdate, Sys.time()))) == 2
    span <- as.numeric(difftime(Sys.time(), maxdate, units = "hours")) < 48
    if(!weekend & !span){
      c <- as.numeric(substr(maxdate, start = 1, stop = 4))
      a <- as.numeric(substr(maxdate, start = 6, stop = 7)) - 1
      b <- as.numeric(substr(maxdate, start = 9, stop = 10))
      df <- yahoo(i, a = a, b = b, c = c)
      if(!is.null(df)){
        if(all(!is.na(df)) & nrow(df) > 0){
          df <- df[nrow(df):1]
          write.table(df, file = paste0(i, ".csv"), sep = ",",
                      row.names = FALSE, col.names = FALSE, append = TRUE)
          DATA[[i]] <- rbind(DATA[[i]], df)
        }
      }
    }
  }
}
#######


############################


####Listing 2.7: YQL Update Method####
setwd(datadir)
library(XML)
batchsize <- 101
# i in 1:5 for this example
for(i in 1:(ceiling(length(S) / batchsize)) ){
  midQuery <- " ("
  maxdate <- character(0)
  startIndex <- ((i - 1) * batchsize + 1)
  endIndex <- min(i * batchsize, length(S))
  
  # find earliest date and build query
  for(s in S[startIndex:(endIndex - 1)]){
    maxdate <- c(maxdate, DATA[[s]][[1]][nrow(DATA[[s]])])
    midQuery <- paste0(midQuery, "", s, ", ")
  }
  maxdate <- c(maxdate, DATA[[S[endIndex]]][[1]]
               [nrow(DATA[[S[endIndex]]])])
  startDate <- max(maxdate)
  if( startDate <
      substr(strptime(substr(Sys.time(), 0, 10), "%Y-%m-%d")
             - 28 * 86400, 0, 10) ){
    cat("Query is greater than 20 trading days. Download with csv method.")
    break
  }
  
  # Adds a day (86400 seconds) to the earliest date to avoid duplicates
  startDate <- substr(as.character(strptime(startDate, "%Y-%m-%d") + 86400), 0, 10)
  endDate <- substr(Sys.time(), 0, 10)
  
  # Yahoo! updates at 4:15 EST at earliest, check if it is past 4:15 day after last
  isUpdated <- as.numeric(difftime(Sys.time(), startDate, units = "hours")) >= 40.25
  
  # If both days fall in the same weekend, we will not attempt to update
  weekend <- sum(c("Saturday", "Sunday") %in%
                   weekdays(c(strptime(endDate, "%Y-%m-%d"),
                              c(strptime(startDate, "%Y-%m-%d"))))) == 2
  
  span <- as.numeric(difftime(Sys.time(), startDate, units = "hours")) < 48
  
  if( startDate <= endDate &
      !weekend &
      !span &
      isUpdated ){
    
    # Piece this extremely long URL together
    base <- "http://query.yahooapis.com/v1/public/yql?"
    begQuery <- "q=select * from yahoo.finance.historicaldata where symbol in "
    midQuery <- paste0(midQuery, "", S[min(i * batchsize, length(S))], ") ")
    endQuery <- paste0("and startDate = ", startDate,
                       " and endDate = ", endDate, "")
    endParams <- "&diagnostics=true&env=store://datatables.org/alltableswithkeys"
    urlstr <- paste0(base, begQuery, midQuery, endQuery, endParams)
    urlstr=gsub("", "'", urlstr)
    
    # Fetch data and arrange in XML tree
    doc <- xmlParse(urlstr)
    
    # The next few lines rely heavily and XPath and quirks
    # of S4 objects in the XML package in R.
    # We retrieve every node (or branch) on //query/results/quote
    # and retrieve the values Date, Open, High, etc. from the branch
    df <- getNodeSet(doc, c("//query/results/quote"),
                     fun = function(v) xpathSApply(v,
                                                   c("./Date",
                                                     "./Open",
                                                     "./High",
                                                     "./Low",
                                                     "./Close",
                                                     "./Volume",
                                                     "./Adj_Close"),
                                                   xmlValue))
    
    # If the URL found data we organize and update
    if(length(df) != 0){
      
      # We get the atrributes from the same tree, which happen
      # to be dates we need
      symbols <- unname(sapply(
        getNodeSet(doc, c("//query/results/quote")), xmlAttrs))
      df <- cbind(symbols, data.frame(t(data.frame(df, stringsAsFactors = FALSE)),
                                      stringsAsFactors = FALSE, row.names = NULL))
      names(df) <- c("Symbol", "Date",
                     "Open", "High", "Low", "Close", "Volume", "Adj Close")
      df[,3:8] <- lapply(df[,3:8], as.numeric)
      df <- df[order(df[,1], decreasing = FALSE),]
      sym <- as.character(unique(df$Symbol))
      for(s in sym){
        temp <- df[df$Symbol == s, 2:8]
        temp <- temp[order(temp[,1], decreasing = FALSE),]
        startDate <- DATA[[s]][["Date"]][nrow(DATA[[s]])]
        DATA[[s]] <- DATA[[s]][order(DATA[[s]][[1]], decreasing = FALSE)]
        DATA[[s]] <- rbind(DATA[[s]], temp[temp$Date > startDate,])
        write.table(DATA[[s]][DATA[[s]][["Date"]] > startDate],
                    file = paste0(s, ".csv"), sep = ",",
                    row.names = FALSE, col.names = FALSE, append = TRUE)
      }}}}
######


####Listing 2.8: Organizing as Date-Uniform zoo Object####
library(zoo)

# Compute the date template as a column of a data.frame for merging
datetemp <- sort(unique(unlist(sapply(DATA, function(v) v[["Date"]]))))
datetemp <- data.frame(datetemp, stringsAsFactors = FALSE)
names(datetemp) <- "Date"

# Double-check that our data is unique and in ascending-date order
DATA <- lapply(DATA, function(v) unique(v[order(v$Date),]))

# Create 6 new objects that will hold our re-orgainzed data
DATA[["Open"]] <- DATA[["High"]] <- DATA[["Low"]] <-
  DATA[["Close"]] <- DATA[["Adj Close"]] <- DATA[["Volume"]] <- datetemp

# This loop will sequentially append the columns of each symbol
# to the appropriate Open, High, Low, etc. object
for(s in S){
  for(i in c("Open", "High", "Low", "Close", "Adj Close", "Volume")){
    temp <- data.frame(cbind(DATA[[s]][["Date"]], DATA[[s]][[i]]),
                       stringsAsFactors = FALSE)
    names(temp) <- c("Date", s)
    temp[,2] <- as.numeric(temp[,2])
    if(!any(!DATA[[i]][["Date"]][(nrow(DATA[[i]]) - nrow(temp)+1):nrow(DATA[[i]])]
            == temp[,1])){
      temp <- rbind(t(matrix(nrow = 2, ncol = nrow(DATA[[i]]) - nrow(temp),
                             dimnames = list(names(temp)))), temp)
      DATA[[i]] <- cbind(DATA[[i]], temp[,2])
    } else {
      DATA[[i]] <- merge(DATA[[i]], temp, all.x = TRUE, by = "Date")
    }
    names(DATA[[i]]) <- c(names(DATA[[i]])[-(ncol(DATA[[i]]))], s)
  }
  DATA[[s]] <- NULL
  if(which(S==s) %% 25 == 0 ){ print(paste(which(S==s),"/", length(S))) }
}


# Declare them as zoo objects for use with time-series functions
DATA <- lapply(DATA, function(v) zoo(v[,2:ncol(v)], strptime(v[,1], "%Y-%m-%d")))
# Remove extra variables

rm(list = setdiff(ls(), c("DATA", "datadir", "functiondir", "rootdir")))
####

##############

####Listing 3.1: Eliminating pre-S&P Data####
setwd(rootdir)
if( "SPdates.R" %in% list.files() ){
  source("SPdates.R")
} else {
  url <- "http://trading.chrisconlan.com/SPdates.csv"
  S <- read.csv(url, header = FALSE, stringsAsFactors = FALSE)
  dump(list = "S", "SPdates.R")
}
names(S) <- c("Symbol", "Date")
S$Date <- strptime(S$Date, "%m/%d/%Y")
for(s in names(DATA[["Close"]])){
  for(i in c("Open", "High", "Low", "Close", "Adj Close", "Volume")){
    Sindex <- which(S[,1] == s)
    if(S[Sindex, "Date"] != "1900-01-01 EST" &
       S[Sindex, "Date"] >= "2000-01-01 EST"){
      DATA[[i]][index(DATA[[i]]) <= S[Sindex, "Date"], s] <- NA
    }
  }
  if(which(names(DATA[["Close"]])==s) %% 25 == 0 ){ print(paste(which(names(DATA[["Close"]])==s),"/", nrow(S))) }
}
######




####Listing 3.6: Adjusting OHLC Data####
# Declare new zoo data frame of adjustment factors
MULT <- DATA[["Adj Close"]] / DATA[["Close"]]

# Store Close and Open Prices in new variable "Price" and "OpenPrice"
DATA[["Price"]] <- DATA[["Close"]]
DATA[["OpenPrice"]] <- DATA[["Open"]]

# Adjust Open, High, and Low
DATA[["Open"]] <- DATA[["Open"]] * MULT
DATA[["High"]] <- DATA[["High"]] * MULT
DATA[["Low"]] <- DATA[["Low"]] * MULT

# Copy Adjusted Close to Close
DATA[["Close"]] <- DATA[["Adj Close"]]

# Delete Adjusted Close
DATA[["Adj Close"]] <- NULL
######


####Listing 3.7: Forward Replacement on Inactive Symbols####
for(s in names(DATA[["Close"]]) ){
  if(is.na(DATA[["Close"]][nrow(DATA[["Close"]]), s])){
    maxInd <- max(which(!is.na(DATA[["Close"]][,s])))
    for( i in c("Close", "Open", "High", "Low")){
      DATA[[i]][(maxInd+1):nrow(DATA[["Close"]]),s] <- DATA[["Close"]][maxInd,s]
    }
    for( i in c("Price", "OpenPrice") ){
      DATA[[i]][(maxInd+1):nrow(DATA[["Close"]]),s] <- DATA[["Price"]][maxInd,s]
    }
    DATA[["Volume"]][(maxInd+1):nrow(DATA[["Close"]]),s] <- 0
  }
}
#######



####Listing 3.8: Computing Return Matrices####
# Pad with NAs to perserver dimension equality
NAPAD <- zoo(matrix(NA, nrow = 1, ncol = ncol(DATA[["Close"]])),
             order.by = index(DATA[["Close"]])[1])
names(NAPAD) <- names(DATA[["Close"]])

# Compute Daily Close-to-Close Returns
RETURN <- rbind( NAPAD, ( DATA[["Close"]] / lag(DATA[["Close"]], k = -1) ) - 1 )

# Compute Overnight Returns (Close-to-Open)
OVERNIGHT <- rbind( NAPAD, ( DATA[["Open"]] / lag(DATA[["Close"]], k = -1) ) - 1 )
######



####Listing 6.7: Registering Parallel Backend in Windows####
library(doParallel)
workers <- 4
registerDoParallel( cores = workers )
#stopImplicitCluster()
#########


####Listing 6.9: Integer Mapping for Multicore Time Series Computations####
delegate <- function( i = i, n = n, k = k, p = workers ){
  nOut <- n - k + 1
  nProc <- ceiling( nOut / p )
  return( (( i - 1 ) * nProc + 1) : min(i * nProc + k - 1, n) )
}
#########


####Listing 6.12: Wrapper Function for Multicore Time Series Computations####
mcTimeSeries <- function( data, tsfunc, byColumn, windowSize, workers, ...){
  # On windows, objects in the global environment are not attached when
  # foreach is called from within a function. Only the arguments of the
  # function call are attached. So we will first get a list of the arguments
  # and all objects in the global environment. Then we will remove the 
  # duplicates.
  args <- names(mget(ls()))
  exports <- ls(.GlobalEnv)
  exports <- exports[!exports %in% args]
  
  SERIES <- foreach( i = 1:workers, .combine = rbind, 
                     .packages="zoo", .export=exports) %dopar% {
                       jRange <- delegate( i = i, n = nrow(data), k = windowSize, p = workers)
                       rollapply(data[jRange,],
                                 width = windowSize,
                                 FUN = tsfunc,
                                 align = "right",
                                 by.column = byColumn)
                     }
  names(SERIES) <- gsub("\\..+", "", names(SERIES))
  
  if( windowSize > 1){
    PAD <- zoo(matrix(nrow = windowSize-1, ncol = ncol(SERIES), NA),
               order.by = index(data)[1:(windowSize-1)])
    names(PAD) <- names(SERIES)
    SERIES <- rbind(PAD, SERIES)
  }
  if(is.null(names(SERIES))){
    names(SERIES) <- gsub("\\..+", "", names(data)[1:ncol(SERIES)])
  }
  return(SERIES)
}
#######



####Listing 6.13: Computing Indicators with our Multicore Wrapper####
# Computing the return matrix
tsfunc <- function(v) (v[2,] / v[1,]) - 1
RETURN <- mcTimeSeries( DATA[["Close"]], tsfunc, FALSE, 2, workers )
####


rm(list = setdiff(ls(), c("datadir", "functiondir", "rootdir",
                          "DATA", "OVERNIGHT", "RETURN",
                          "delegate", "mcTimeSeries", "workers")))

##################
##################
##################


equNA <- function(v){
  o <- which(!is.na(v))[1]
  return(ifelse(is.na(o), length(v)+1, o))
}


##############



####Listing 7.1: Simulating Perfomance####
simulate <- function(OPEN, CLOSE,
                     ENTRY, EXIT, FAVOR,
                     maxLookback, maxAssets, startingCash,
                     slipFactor, spreadAdjust, flatCommission, perShareCommission,
                     verbose = FALSE, failThresh = 0,
                     initP = NULL, initp = NULL){
  
  t0=Sys.time()
  
  timer=matrix(0, nrow=16)
  t1=proc.time()[3]
  # Step 1
  if( any( dim(ENTRY) != dim(EXIT) ) |
      any( dim(EXIT) != dim(FAVOR) ) |
      any( dim(FAVOR) != dim(CLOSE) ) |
      any( dim(CLOSE) != dim(OPEN)) )
    stop( "Mismatching dimensions in ENTRY, EXIT, FAVOR, CLOSE, or OPEN.")
  
  if( any( names(ENTRY) != names(EXIT)) |
      any( names(EXIT) != names(FAVOR) ) |
      any( names(FAVOR) != names(CLOSE) ) |
      any( names(CLOSE) != names(OPEN) ) |
      is.null(names(ENTRY)) | is.null(names(EXIT)) |
      is.null(names(FAVOR)) | is.null(names(CLOSE)) |
      is.null(names(OPEN)) )
    stop( "Mismatching or missing column names in ENTRY, EXIT, FAVOR, CLOSE, or OPEN.")
  
  FAVOR <- zoo(t(apply(FAVOR, 1, function(v) ifelse(is.nan(v) | is.na(v), 0, v) )),
               order.by = index(CLOSE))
  
  timer[1]=timer[1] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  
  t10=proc.time()[3]
  # Step 2
  K <- maxAssets
  k <- 0
  C <- rep(startingCash, times = nrow(CLOSE))
  S <- names(CLOSE)
  P <- p <- zoo( matrix(0, ncol=ncol(CLOSE), nrow=nrow(CLOSE)),
                 order.by = index(CLOSE) )
  timer[12]=timer[12] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  if( !is.null( initP ) & !is.null( initp ) ){
    P[1:maxLookback,] <-
      matrix(initP, ncol=length(initP), nrow=maxLookback, byrow = TRUE)
    p[1:maxLookback,] <-
      matrix(initp, ncol=length(initp), nrow=maxLookback, byrow = TRUE)
  }
  
  names(P) <- names(p) <- S
  equity <- rep(NA, nrow(CLOSE))
  timer[13]=timer[13] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  rmNA <- foreach(i = 1:3, .packages="zoo", 
                  .export=c("FAVOR","ENTRY", "EXIT", "equNA")) %dopar% {
                    unlist(lapply(get(c("FAVOR", "ENTRY", "EXIT")[i]), equNA))
                  }
  rmNA <- pmax(rmNA[[1]], rmNA[[2]], rmNA[[3]])
  
  timer[14]=timer[14] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  
  for( j in 1:ncol(ENTRY) ){
    if( rmNA[j] > (maxLookback + 1) &
        rmNA[j] < nrow(ENTRY) ){
      sel          <- 1:(rmNA[j]-1)
      FAVOR[sel,j] <- NA
      ENTRY[sel,j] <- NA
      EXIT[sel,j]  <- NA
    }
  }
  timer[15]=timer[15] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  timer[16]=timer[16] + proc.time()[3] - t1;   t1=proc.time()[3]
  
  timer[2]=timer[2] + proc.time()[3] - t10
  
  # Step 3
  for( i in maxLookback:(nrow(CLOSE)-1) ){
    
    t1=proc.time()[3]
    # Step 4
    C[i+1] <- C[i]
    P[i+1,] <- as.numeric(P[i,])
    p[i+1,] <- as.numeric(p[i,])
    longS <- S[which(P[i,] > 0)]
    shortS <- S[which(P[i,] < 0)]
    k <- length(longS) + length(shortS)
    
    timer[3]=timer[3] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 5    
    longTrigger <- setdiff(S[which(ENTRY[i,] == 1)], longS)
    shortTrigger <- setdiff(S[which(ENTRY[i,] == -1)], shortS)
    
    trigger <- c(longTrigger, shortTrigger)
    
    if( length(trigger) > K ) {
      
      keepTrigger <- trigger[order(c(as.numeric(FAVOR[i,longTrigger]),-as.numeric(FAVOR[i,shortTrigger])), decreasing = TRUE)][1:K]
      
      longTrigger <- longTrigger[longTrigger %in% keepTrigger]
      shortTrigger <- shortTrigger[shortTrigger %in% keepTrigger]
      trigger <- c(longTrigger, shortTrigger)
    }
    triggerType <- c(rep(1, length(longTrigger)), rep(-1, length(shortTrigger)))
    
    timer[4]=timer[4] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 6
    longExitTrigger <- longS[longS %in%
                               S[which(EXIT[i,] == 1 | EXIT[i,] == 999)]]
    shortExitTrigger <- shortS[shortS %in%
                                 S[which(EXIT[i,] == -1 | EXIT[i,] == 999)]]
    exitTrigger <- c(longExitTrigger, shortExitTrigger)
    
    timer[5]=timer[5] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 7
    needToExit <- max( (length(trigger) - length(exitTrigger)) - (K - k), 0)
    if( needToExit > 0 ){
      toExitLongS <- setdiff(longS, exitTrigger)
      toExitShortS <- setdiff(shortS, exitTrigger)
      toExit <- character(0)
      for( counter in 1:needToExit ){
        if( length(toExitLongS) > 0 & length(toExitShortS) > 0 ){
          if( min(FAVOR[i,toExitLongS]) < min(-FAVOR[i,toExitShortS]) ){
            pullMin <- which.min(FAVOR[i,toExitLongS])
            toExit <- c(toExit, toExitLongS[pullMin])
            toExitLongS <- toExitLongS[-pullMin]
          } else {
            pullMin <- which.min(-FAVOR[i,toExitShortS])
            toExit <- c(toExit, toExitShortS[pullMin])
            toExitShortS <- toExitShortS[-pullMin]
          }
        } else if( length(toExitLongS) > 0 & length(toExitShortS) == 0 ){
          pullMin <- which.min(FAVOR[i,toExitLongS])
          toExit <- c(toExit, toExitLongS[pullMin])
          toExitLongS <- toExitLongS[-pullMin]
        } else if( length(toExitLongS) == 0 & length(toExitShortS) > 0 ){
          pullMin <- which.min(-FAVOR[i,toExitShortS])
          toExit <- c(toExit, toExitShortS[pullMin])
          toExitShortS <- toExitShortS[-pullMin]
        }
      }
      longExitTrigger <- c(longExitTrigger, longS[longS %in% toExit])
      shortExitTrigger <- c(shortExitTrigger, shortS[shortS %in% toExit])
    }
    timer[6]=timer[6] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 8
    exitTrigger <- c(longExitTrigger, shortExitTrigger)
    exitTriggerType <- c(rep(1, length(longExitTrigger)),
                         rep(-1, length(shortExitTrigger)))
    
    timer[7]=timer[7] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 9
    if( length(exitTrigger) > 0 ){
      for( j in 1:length(exitTrigger) ){
        exitPrice <- as.numeric(OPEN[i+1,exitTrigger[j]])
        effectivePrice <- exitPrice * (1 - exitTriggerType[j] * slipFactor) -
          exitTriggerType[j] * (perShareCommission + spreadAdjust)
        if( exitTriggerType[j] == 1 ){
          C[i+1] <- C[i+1] +
            ( as.numeric( P[i,exitTrigger[j]] ) * effectivePrice )
          - flatCommission
        } else {
          C[i+1] <- C[i+1] -
            ( as.numeric( P[i,exitTrigger[j]] ) *
                ( 2 * as.numeric(p[i, exitTrigger[j]]) - effectivePrice ) )
          - flatCommission
        }
        P[i+1, exitTrigger[j]] <- 0
        p[i+1, exitTrigger[j]] <- 0
        k <- k - 1
      }
    }
    
    timer[8]=timer[8] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 10
    if( length(trigger) > 0 ){
      for( j in 1:length(trigger) ){
        entryPrice <- as.numeric(OPEN[i+1,trigger[j]])
        effectivePrice <- entryPrice * (1 + triggerType[j] * slipFactor) +
          triggerType[j] * (perShareCommission + spreadAdjust)
        
        P[i+1,trigger[j]] <- triggerType[j] *
          floor( ( (C[i+1] - flatCommission) / (K - k) ) / effectivePrice )
        
        p[i+1,trigger[j]] <- effectivePrice
        
        C[i+1] <- C[i+1] -
          ( triggerType[j] * as.numeric(P[i+1,trigger[j]]) * effectivePrice )
        - flatCommission
        
        k <- k + 1
      }
    }
    
    timer[9]=timer[9] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 11
    equity[i] <- C[i+1]
    for( s in S[which(P[i+1,] > 0)] ){
      equity[i] <- equity[i] +
        as.numeric(P[i+1,s]) *
        as.numeric(OPEN[i+1,s])
    }
    for( s in S[which(P[i+1,] < 0)] ){
      equity[i] <- equity[i] -
        as.numeric(P[i+1,s]) *
        ( 2 * as.numeric(p[i+1,s]) - as.numeric(OPEN[i+1,s]) )
    }
    if( equity[i] < failThresh ){
      warning("\n*** Failure Threshold Breached ***\n")
      break
    }
    
    timer[10]=timer[10] +  proc.time()[3] - t1; t1=proc.time()[3]
    
    # Step 12
    if( verbose ){
      if( i %% 21 == 0 ){
        cat(paste0("################################## ",
                   round(100 * (i - maxLookback) /
                           (nrow(CLOSE) - 1 - maxLookback), 1), "%",
                   " ##################################\n"))
        cat(paste0("$", signif(equity[i], 5), "m"))
        cat("\n")
        cat(paste0("CAGR: ",
                   round(100 * ((equity[i] / (equity[maxLookback]))^
                                  (252/(i - maxLookback + 1)) - 1), 2),
                   "%"))
        cat("\n")
        cat(S[which(P[i+1,]!=0)])
        cat("\n")
        cat(paste("Current Simulation Date",as.character(index(CLOSE)[i])))
        cat("\n")
        print(Sys.time() - t0)
        cat("\n\n")
      }
    }
    
    timer[11]=timer[11] +  proc.time()[3] - t1; t1=proc.time()[3]
  }
  
  # Step 13
  return(list(equity = equity, C = C, P = P, p = p, timer=timer))
}
######

#########################



####Listing 8.1: Declaring the Evaluator Function####
# Declare entry function for use inside evaluator
entryfunc <- function(v, shThresh, INDIC){
  nc <- ncol(v)/2
  return(
    as.numeric(v[1,1:nc] <= 0 &
                 v[2,1:nc] > 0 &
                 v[2,(nc+1):(2*nc)] >
                 quantile(v[2,(nc+1):(2*nc)],
                          shThresh, na.rm = TRUE)
    )
  )
}

evaluate <- function(PARAM, minVal = NA, maxVal = NA, y = 2014,
                     continuous = TRUE, verbose = FALSE,
                     negative = FALSE, transformOnly = FALSE,
                     returnData = FALSE, accountParams = NULL, 
                     entryfunc){
  
  print(rbind(PARAM, minVal, maxVal))
  # Convert and declare parameters if they exist on continuous (-inf,inf) domain
  if( continuous | transformOnly ){
    PARAM <- minVal +
      (maxVal - minVal) * unlist(lapply( PARAM, function(v) (1 + exp(-v))^(-1) ))
    if( transformOnly ){
      return(PARAM)
    }
  }
  
  # Max shares to hold
  K <- 10
  
  # Declare n1 as itself, n2 as a multiple of n1 defined by nFact,
  # and declare the length and threshold in sharpe ratio for FAVOR
  n1 <- max(round(PARAM[["n1"]]), 2)
  n2 <- max(round(PARAM[["nFact"]] * PARAM[["n1"]]), 3, n1+1)
  nSharpe <- max(round(PARAM[["nSharpe"]]), 2)
  shThresh <- max(0, min(PARAM[["shThresh"]], .99))
  maxLookback <- max(n1, n2, nSharpe) + 1

  max(n2-n1+1,1)
  
  # Subset data according to year, y
  #period <-
  #index(DATA[["Close"]]) >= strptime(paste0("01-01-", y), "%d-%m-%Y") &
  #index(DATA[["Close"]]) < strptime(paste0("01-01-", y+1), "%d-%m-%Y")
  
  # Subset data according to years, y
  period <-
    index(DATA[["Close"]]) >= strptime(paste0("01-01-", y[1]), "%d-%m-%Y") &
    index(DATA[["Close"]]) < strptime(paste0("01-01-", y[length(y)]+1), "%d-%m-%Y")
  
  
  period <- period |
    ((1:nrow(DATA[["Close"]]) > (which(period)[1] - maxLookback)) &
       (1:nrow(DATA[["Close"]]) <= (which(period)[sum(period)]) + 1))
  


  CLOSE <- DATA[["Close"]][period,]
  OPEN <- DATA[["Open"]][period,]
  SUBRETURN <- RETURN[period,]
  
  print(rbind(PARAM,cbind(n1,n2, nSharpe, shThresh)))
  # Compute inputs for long-only MACD as in Listing 7.2
  INDIC <- mcTimeSeries( CLOSE,
                         function(v)
                           colMeans(v[max(n2-n1+1,1):n2,], na.rm = T)
                           #colMeans(v[(n2-n1+1):n2,], na.rm = T) #May get less than 1
                         - colMeans(v, na.rm = T),
                         FALSE, n2, workers )
  
  RMEAN <- mcTimeSeries( SUBRETURN, function(v) colMeans(v, na.rm = T),
                         FALSE, nSharpe, workers )
  
  FAVOR <- RMEAN / mcTimeSeries( (SUBRETURN - RMEAN) ^ 2,
                                 function(v) colMeans(v, na.rm = T),
                                 FALSE, nSharpe, workers )
  
  ENTRY <- mcTimeSeries(cbind(INDIC, FAVOR),
                        function(v) entryfunc(v, shThresh, INDIC),
                        FALSE, 2, workers, entryfunc, shThresh)
  
  EXIT  <- zoo(matrix(0, ncol=ncol(CLOSE), nrow=nrow(CLOSE)),
               order.by = index(CLOSE))
  names(EXIT) <- names(CLOSE)
  
  
  # Simulate and store results
  if( is.null(accountParams) ){
    RESULTS <- simulate(OPEN, CLOSE,
                        ENTRY, EXIT, FAVOR,
                        maxLookback, K, 100000,
                        0.001, 0.01, 3.5, 0,
                        verbose, 0)
  } else {
    RESULTS <- simulate(OPEN, CLOSE,
                        ENTRY, EXIT, FAVOR,
                        maxLookback, K, accountParams[["C"]],
                        0.001, 0.01, 3.5, 0,
                        verbose, 0,
                        initP = accountParams[["P"]], initp = accountParams[["p"]])
  }
  
  
  if(!returnData){
    # Compute and return sharpe ratio
    v <- RESULTS[["equity"]]
    returns <- ( v[-1] / v[-length(v)] ) - 1
    out <- mean(returns, na.rm = T) / sd(returns, na.rm = T)
    if(!is.nan(out)){
      if( negative ){
        return( -out )
      } else {
        return( out )
      }
    } else {
      return(0)
    }
    
  } else {
    return(RESULTS)
  }
}

###########



####Listing 8.5: Nelder-Mead Optimization####
optimize <- function(y, minVal, maxVal, entryfunc=entryfunc, maxIter=3, PARAMNaught=NULL, continuous=TRUE){
  
  #K <- maxIter <-10
  K <- maxIter
  
  
  # Vector theta_0
  initDelta   <- 6
  deltaThresh <- 0.05
  
  if(is.null(PARAMNaught)){
    PARAM <- PARAMNaught <-
      c(n1 = 0, nFact = 0, nSharpe = 0, shThresh = 0) - initDelta/2
  }else{
    #continuous=FALSE
    PARAM <- PARAMNaught
    
  }
  
  # Optimization parameters
  alpha <- 1
  gamma <- 2
  rho   <- .5
  sigma <- .5
  
  randomInit <- FALSE
  
  np <- length(PARAM)
  
  OPTIM <- data.frame(matrix(NA, ncol = np + 1, nrow = maxIter * (2 * np + 2)))
  o     <- 1
  
  SIMPLEX        <- data.frame(matrix(NA, ncol = np + 1, nrow = np + 1))
  names(SIMPLEX) <- names(OPTIM) <- c(names(PARAM), "obj")
  
  
  # Print function for reporting progress in loop
  printUpdate <- function(){
    cat("Iteration: ", k, "of", K, "\n")
    cat("\t\t", paste0(strtrim(names(OPTIM), 6), "\t"), "\n")
    cat("Global Best:\t",
        paste0(round(unlist(OPTIM[which.min(OPTIM$obj),]),3), "\t"), "\n")
    cat("Simplex Best:\t",
        paste0(round(unlist(SIMPLEX[which.min(SIMPLEX$obj),]),3), "\t"), "\n")
    cat("Simplex Size:\t",
        paste0(max(round(simplexSize,3)), "\t"), "\n\n\n")
  }
  
  # Initialize SIMPLEX
  for( i in 1:(np+1) ) {
    SIMPLEX[i,1:np] <- PARAMNaught + initDelta * as.numeric(1:np == (i-1))
    SIMPLEX[i,np+1] <- evaluate(SIMPLEX[i,1:np], minVal, maxVal, negative = TRUE,
                                y = y, entryfunc=entryfunc, continuous=continuous)
    OPTIM[o,] <- SIMPLEX[i,]
    o         <- o + 1
  }
  
  
  # Optimization loop
  for( k in 1:K ){
    
    SIMPLEX  <- SIMPLEX[order(SIMPLEX[,np+1]),]
    centroid <- colMeans(SIMPLEX[-(np+1),-(np+1)])
    
    cat("Computing Reflection...\n")
    reflection    <- centroid + alpha * (centroid - SIMPLEX[np+1,-(np+1)])
    
    reflectResult <- evaluate(reflection, minVal, maxVal, negative = TRUE, y = y, entryfunc=entryfunc, 
                              continuous=continuous)
    OPTIM[o,]     <- c(reflection, obj = reflectResult)
    o             <- o + 1
    
    if( reflectResult > SIMPLEX[1,np+1] &
        reflectResult < SIMPLEX[np, np+1] ){
      
      SIMPLEX[np+1,] <- c(reflection, obj = reflectResult)
      
    } else if( reflectResult < SIMPLEX[1,np+1] ) {
      
      cat("Computing Expansion...\n")
      expansion <- centroid + gamma * (reflection - centroid)
      expansionResult <- evaluate(expansion,
                                  minVal, maxVal, negative = TRUE, y = y, entryfunc=entryfunc, 
                                  continuous=continuous)
      
      OPTIM[o,] <- c(expansion, obj = expansionResult)
      o         <- o + 1
      
      if( expansionResult < reflectResult ){
        SIMPLEX[np+1,] <- c(expansion, obj = expansionResult)
      } else {
        SIMPLEX[np+1,] <- c(reflection, obj = reflectResult)
      }
      
    } else if( reflectResult > SIMPLEX[np, np+1] ) {
      
      cat("Computing Contraction...\n")
      contract <- centroid + rho * (SIMPLEX[np+1,-(np+1)] - centroid)
      contractResult <- evaluate(contract, minVal, maxVal, negative = TRUE, y = y, entryfunc=entryfunc, 
                                 continuous=continuous)
      
      
      OPTIM[o,] <- c(contract, obj = contractResult)
      o          <- o + 1
      
      if( contractResult < SIMPLEX[np+1, np+1] ){
        
        SIMPLEX[np+1,] <- c(contract, obj = contractResult)
        
      } else {
        cat("Computing Shrink...\n")
        for( i in 2:(np+1) ){
          SIMPLEX[i,1:np] <- SIMPLEX[1,-(np+1)] +
            sigma * (SIMPLEX[i,1:np] - SIMPLEX[1,-(np+1)])
          SIMPLEX[i,np+1] <- c(obj = evaluate(SIMPLEX[i,1:np],
                                              minVal, maxVal,
                                              negative = TRUE, y = y, entryfunc=entryfunc, 
                                              continuous=continuous))
        }
        
        OPTIM[o:(o+np-1),] <- SIMPLEX[2:(np+1),]
        o                  <- o + np
      }
    }
    
    centroid    <- colMeans(SIMPLEX[-(np+1),-(np+1)])
    simplexSize <- rowMeans(t(apply(SIMPLEX[,1:np], 1,
                                    function(v) abs(v - centroid))))
    
    if( max(simplexSize) < deltaThresh ){
      
      cat("Size Threshold Breached: Restarting with Random Initiate\n\n")
      
      for( i in 1:(np+1) ) {
        
        SIMPLEX[i,1:np] <- (PARAMNaught * 0) +
          runif(n = np, min = -initDelta, max = initDelta)
        
        SIMPLEX[i,np+1] <- evaluate(SIMPLEX[i,1:np],
                                    minVal, maxVal, negative = TRUE, y = y, entryfunc=entryfunc, 
                                    continuous=continuous)
        OPTIM[o,]       <- SIMPLEX[i,]
        o               <- o + 1
        
        SIMPLEX     <- SIMPLEX[order(SIMPLEX[,np+1]),]
        centroid    <- colMeans(SIMPLEX[-(np+1),-(np+1)])
        simplexSize <- rowMeans(t(apply(SIMPLEX[,1:np], 1, function(v) abs(v - centroid))))
      }
      
    }
    printUpdate()
  }
  
  #Pruning excess rows
  OPTIM <- OPTIM[!is.na(OPTIM[,1]),]
  
  # Return the best optimization in untransformed parameters
  return(
    evaluate(OPTIM[which.min(OPTIM$obj),1:np], minVal, maxVal, transformOnly = TRUE, entryfunc=entryfunc)
  )
}
###########




####Listing 8.6: Generating Valid Performance Projections with Cross Validation####
set.seed(1234)
minVal  <- c(n1 = 1,   nFact = 1,  nSharpe = 1,   shThresh = .01)
maxVal  <- c(n1 = 250, nFact = 5,  nSharpe = 200, shThresh = .99)
#minVal <- c(n1 = 1,   nFact = 1,  nSharpe = 1,   shThresh = 0.01)
#maxVal <- c(n1 = 250, nFact = 10, nSharpe = 250, shThresh = .99)

RESULTS       <- list()
accountParams <- list()
testRange     <- 2004:2015
maxIter       <- 3
ResetPARAM    <- FALSE #If TRUE reset the model parameters after each year of data is tested
YearByYear    <- TRUE  #If TRUE optimize only for previous year of data (vs all years up to that point)

# As defined in heuristic with delta_O = delta_P = 1 year
for( yf in testRange ){
  
  
  if(YearByYear){ y <- yf }else{ y <- testRange[1]:yf }
  if( yf == testRange[1] | ResetPARAM ){ PARAM0=NULL }else{ PARAM0=PARAM }
  
  PARAM <- optimize(y = y, minVal = minVal, maxVal = maxVal, entryfunc=entryfunc, 
                    maxIter=maxIter, PARAMNaught=PARAM0)

  print("Opt Done")

  if( yf == testRange[1] ){
    RESULTS[[as.character(yf+1)]] <-
      evaluate(PARAM, y = yf + 1, minVal = minVal, maxVal = maxVal, continuous = TRUE,
               returnData = TRUE, verbose = TRUE, entryfunc=entryfunc )
  } else {
    
    # Pass account parameters to next simulation after first year
    strYear <- as.character(yf)
    aLength <- length(RESULTS[[strYear]][["C"]])
    accountParams[["C"]] <- (RESULTS[[strYear]][["C"]])[aLength]
    accountParams[["P"]] <- (RESULTS[[strYear]][["P"]])[aLength]
    accountParams[["p"]] <- (RESULTS[[strYear]][["p"]])[aLength]
    
    RESULTS[[as.character(yf+1)]] <-
      evaluate(PARAM, y = yf + 1, minVal = minVal, maxVal = maxVal, continuous = TRUE,
               returnData = TRUE, verbose = TRUE,
               accountParams = accountParams, entryfunc=entryfunc)
  }
  
  # extract equity curve
  for( y2 in (testRange[1]:yf + 1) ){
    strYear <- as.character(y2)
    inYear  <- substr(index(RESULTS[[strYear]][["P"]]), 1, 4) == strYear
    equity  <- (RESULTS[[strYear]][["equity"]])[inYear]
    date    <- (index(RESULTS[[strYear]][["P"]]))[inYear]
    if( y2 == (testRange[1] + 1) ){
      equitySeries <- zoo(equity, order.by = date)
    } else {
      equitySeries <- rbind(equitySeries, zoo(equity, order.by = date))
    }
  }
  
  plot(equitySeries, main=yf)
  grid(); abline(h=100000, lty=2, lwd=3)
}
#####




#####
# extract equity curve
for( y in (testRange + 1) ){
  strYear <- as.character(y)
  inYear <- substr(index(RESULTS[[strYear]][["P"]]), 1, 4) == strYear
  equity <- (RESULTS[[strYear]][["equity"]])[inYear]
  date <- (index(RESULTS[[strYear]][["P"]]))[inYear]
  if( y == (testRange[1] + 1) ){
    equitySeries <- zoo(equity, order.by = date)
  } else {
    equitySeries <- rbind(equitySeries, zoo(equity, order.by = date))
  }
}

plot(equitySeries)
###############



plot(equitySeries, main = "Figure 8.12: Cross-Validated Equity Curve for Long-Only MACD",
ylab = "Account Equity ($)", xlab = "")

#cont to transform






