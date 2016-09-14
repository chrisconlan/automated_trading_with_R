library(zoo)

# Compute the date template as a column of a data.frame for merging
# Considers date are strings in YYYY-MM-DD format
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
  for(i in rev(c("Open", "High", "Low", "Close", "Adj Close", "Volume"))){
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
  
  # Update user on progress
  if( which( S == s ) %% 25 == 0 ){
    cat( paste0(round(100 * which( S == s ) / length(S), 1), "% Complete\n") )
  }
  
}

# Declare them as zoo objects for use with time-series functions
DATA <- lapply(DATA, function(v) zoo(v[,2:ncol(v)], strptime(v[,1], "%Y-%m-%d")))

# Remove extra variables
rm(list = setdiff(ls(), c("DATA", "datadir", "functiondir", "rootdir")))
