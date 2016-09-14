# Listing 3.1
setwd(DIR[["root"]])

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
}
