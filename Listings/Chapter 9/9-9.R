library(RJSONIO)

base <- "http://query.yahooapis.com/v1/public/yql?"
begQuery <- "q=select * from yahoo.finance.historicaldata where symbol in "
midQuery <- "('YHOO', 'AAPL') "
endQuery <- "and startDate = '2016-01-11' and endDate = '2016-01-12'"

# Supply "format=json" argument to URL
endParams <- 
  "&diagnostics=false&format=json&env=store://datatables.org/alltableswithkeys"

urlstr <- paste0(base, begQuery, midQuery, endQuery, endParams)

# Encode URL before requesting
# This is normally handled automatically by the XML package
jdoc <- fromJSON(URLencode(urlstr))

# Format and output data frame as in Listing 9.6
df <- data.frame(t(data.frame(jdoc[["query"]][["results"]][["quote"]])),
                 stringsAsFactors = FALSE)
df[,3:8] <- lapply(df[3:8], as.numeric)
df[,1] <- as.character(df[,1])
rownames(df) <- NULL
