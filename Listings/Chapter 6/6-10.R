k <- 2

# Using for a for loop, pre-allocated
RETURN <- foreach( i = 1:workers, .combine = rbind,
                   .packages = "zoo" ) %dopar% {
  
  CLOSE <- as.matrix(DATA[["Close"]])
  
  jRange <- delegate( i = i, n = nrow(DATA[["Close"]]), k = k, p = workers)
  
  subRETURN <- zoo(
    matrix(numeric(),
           ncol = ncol(DATA[["Close"]]),
           nrow = length(jRange) - k + 1),
    order.by = (index(DATA[["Close"]])[jRange])[-(1:(k-1))])
  
  names(subRETURN) <- names(DATA[["Close"]])
  
  for( j in jRange[-1] ){
    jmod <- j - jRange[1]
    subRETURN[jmod, ] <- (CLOSE[j,] / CLOSE[j-1,]) - 1
  }
  
  subRETURN
  
}
# Completes in 6.99 seconds

