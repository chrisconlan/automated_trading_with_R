minVal <- c(n1 = 1, nFact = 1, nSharpe = 1, shThresh = .01)
maxVal <- c(n1 = 150, nFact = 5, nSharpe = 200, shThresh = .99)

RESULTS <- list()
accountParams <- list ()

testRange <- 2004:2015

# As defined in heuristic with delta_O = delta_P = 1 year
for( y in testRange ){
  
  PARAM <- optimize(y = y, minVal = minVal, maxVal = maxVal)
  
  if( y == testRange[1] ){
    
    RESULTS[[as.character(y+1)]] <- 
      evaluate(PARAM, y = y + 1, minVal = minVal, maxVal = maxVal,
                transform = TRUE, returnData = TRUE, verbose = TRUE )   
    
  } else {
    
    # Pass account parameters to next simulation after first year
    strYear <- as.character(y)
    aLength <- length(RESULTS[[strYear]][["C"]])
    accountParams[["C"]] <-(RESULTS[[strYear]][["C"]])[aLength]
    accountParams[["P"]] <- (RESULTS[[strYear]][["P"]])[aLength]
    accountParams[["p"]] <- (RESULTS[[strYear]][["p"]])[aLength]
    
    RESULTS[[as.character(y+1)]] <- 
      evaluate(PARAM, y = y + 1, minVal = minVal, maxVal = maxVal,
               transform = TRUE, returnData = TRUE, verbose = TRUE,
               accountParams = accountParams) 
    
  }
  
}


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
