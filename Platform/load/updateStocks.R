# Listings 2.6 and 2.7
setwd(DIR[["data"]])
library(XML)

batchsize <- 51

redownload <- character(0)

for(i in 1:(ceiling(length(S) / batchsize)) ){
  
  midQuery <- " ("
  maxdate <- character(0)

startIndex <- ((i - 1) * batchsize + 1)
endIndex <- min(i * batchsize, length(S))


for(s in S[startIndex:(endIndex - 1)]){
  maxdate <- c(maxdate, DATA[[s]][[1]][nrow(DATA[[s]])])
  midQuery <- paste0(midQuery, "'", s, "', ")
}


maxdate <- c(maxdate, DATA[[S[endIndex]]][[1]]
             [nrow(DATA[[S[endIndex]]])]) 

startDate <- max(maxdate)

useCSV <- FALSE
if( startDate < 
    substr(strptime(substr(Sys.time(), 0, 10), "%Y-%m-%d")
           - 20 * 86400, 0, 10) ){
  cat("Query is greater than 20 days. Updating with csv method.")
  useCSV <- TRUE
  break
}

startDate <- substr(as.character(strptime(startDate, "%Y-%m-%d") + 86400), 0, 10)
endDate <- substr(Sys.time(), 0, 10)


isUpdated <- as.numeric(difftime(Sys.time(), startDate, units = "hours")) >= 40.25

weekend <- sum(c("Saturday", "Sunday") %in% 
                weekdays(c(strptime(endDate, "%Y-%m-%d"),
                           c(strptime(startDate, "%Y-%m-%d"))))) == 2


span <- as.numeric(difftime(Sys.time(), startDate, units = "hours")) < 48

runXMLupdate <- startDate <= endDate & !weekend & !span & isUpdated

# Push back query date to validate extra days against adj. close
startDateQuery <- substr(as.character(
  strptime(startDate, "%Y-%m-%d") - 7 * 86400
  ), 0, 10)



if( runXMLupdate ){

base <- "http://query.yahooapis.com/v1/public/yql?"
begQuery <- "q=select * from yahoo.finance.historicaldata where symbol in "
midQuery <- paste0(midQuery, "'", S[min(i * batchsize, length(S))], "') ")
endQuery <- paste0("and startDate = '", startDateQuery,
                   "' and endDate = '", endDate, "'")
endParams <- "&diagnostics=true&env=store://datatables.org/alltableswithkeys"

urlstr <- paste0(base, begQuery, midQuery, endQuery, endParams)

doc <- xmlParse(urlstr)

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

if(length(df) != 0){

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
  
  # Check if the Adj. Close data is equal for matching dates
  # if not, save symbol to redownload later
  if(any( !DATA[[s]][DATA[[s]][["Date"]] %in% temp[,1]]$"Adj Close" ==
    temp[temp[,1] %in% DATA[[s]][["Date"]],7] ))
  {
    
    redownload <- c(redownload, s)
    
  } else {
    
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
}

if( useCSV ){
for(i in S){
  maxdate <- DATA[[i]][["Date"]][nrow(DATA[[i]])]
  isUpdated <- as.numeric(difftime(Sys.time(), maxdate, units = "hours")) >= 40.25
  if( isUpdated ){
    
    maxdate <- strptime(maxdate, "%Y-%m-%d") + 86400
    
    weekend <- sum(c("Saturday", "Sunday") %in% 
                     weekdays(c(maxdate, Sys.time()))) == 2
    
    span <- FALSE
    if( weekend ){
      span <- as.numeric(difftime(Sys.time(), maxdate, units = "hours")) < 48
    }
    
    # Push back query date to validate extra days against adj. close
    startDateQuery <- maxdate - 7 * 86400
    
    if(!weekend & !span){
      c <- as.numeric(substr(startDateQuery, start = 1, stop = 4))
      a <- as.numeric(substr(startDateQuery, start = 6, stop = 7)) - 1
      b <- as.numeric(substr(startDateQuery, start = 9, stop = 10))
      df <- yahoo(i, a = a, b = b, c = c)
      if(!is.null(df)){
        if(all(!is.na(df)) & nrow(df) > 0){
          
          df <- df[nrow(df):1]
          
          if( any(!DATA[[i]][DATA[[i]][["Date"]] %in% df[["Date"]]]$"Adj Close" == 
            df[["Adj Close"]][df[["Date"]] %in% DATA[[i]][["Date"]]]) )
            {
            
              redownload <- c(redownload, i)
            
            } else {
              write.table(df, file = paste0(i, ".csv"), sep = ",",
                row.names = FALSE, col.names = FALSE, append = TRUE) 
              DATA[[i]] <- rbind(DATA[[i]], df)
            }
          
        }
      } 
    }
  }
}
}



# Re-download, store, and load into memory the symbols with
# altered adj. close data
setwd(DIR[["data"]])
if(length(redownload) != 0){
  for( i in redownload ){
    
  df <- yahoo(i)
  if(!is.null(df)) {
    write.csv(df[nrow(df):1], file = paste0(i, ".csv"),
              row.names = FALSE) 
  }
  
  suppressWarnings(
  DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- (DATA[[i]])[order(DATA[[i]][["Date"]], decreasing = FALSE)]
  
}
}


rm(list = setdiff(ls(), c("S", "DATA", "DIR", "CONFIG")))
gc()
