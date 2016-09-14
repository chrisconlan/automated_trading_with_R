# Listing 3.7
for( s in names(DATA[["Close"]]) ){
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
