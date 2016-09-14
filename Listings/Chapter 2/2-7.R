setwd(datadir)
library(XML)

currentTime <- Sys.time()

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
  midQuery <- paste0(midQuery, "'", s, "', ")
}


maxdate <- c(maxdate, DATA[[S[endIndex]]][[1]]
             [nrow(DATA[[S[endIndex]]])]) 

startDate <- max(maxdate)

if( startDate < 
    substr(strptime(substr(currentTime, 0, 10), "%Y-%m-%d")
           - 28 * 86400, 0, 10) ){
  cat("Query is greater than 20 trading days. Download with csv method.")
  break
}


# Adds a day (86400 seconds) to the earliest date to avoid duplicates
startDate <- substr(as.character(strptime(startDate, "%Y-%m-%d") + 86400), 0, 10)
endDate <- substr(currentTime, 0, 10)

# Yahoo! updates at 4:15 EST at earliest, check if it is past 4:15 day after last
isUpdated <- as.numeric(difftime(currentTime, startDate, units = "hours")) >=
  40.25

# If both days fall in the same weekend, we will not attempt to update
weekend <- sum(c("Saturday", "Sunday") %in% 
                weekdays(c(strptime(endDate, "%Y-%m-%d"),
                           c(strptime(startDate, "%Y-%m-%d"))))) == 2

span <- FALSE
if( weekend ){
  span <- as.numeric(difftime(currentTime, startDate, units = "hours")) < 48
}


if( startDate <= endDate &
    !weekend & 
    !span &
    isUpdated ){

# Piece this extremely long URL together
base <- "http://query.yahooapis.com/v1/public/yql?"
begQuery <- "q=select * from yahoo.finance.historicaldata where symbol in "
midQuery <- paste0(midQuery, "'", S[min(i * batchsize, length(S))], "') ")
endQuery <- paste0("and startDate = '", startDate,
                   "' and endDate = '", endDate, "'")
endParams <- "&diagnostics=true&env=store://datatables.org/alltableswithkeys"

urlstr <- paste0(base, begQuery, midQuery, endQuery, endParams)

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

}
}
}
}
