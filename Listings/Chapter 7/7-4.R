SUBDATA <- lapply(DATA, function(v) v[-(1:3500),])
SUBRETURN <- RETURN[-(1:3500),]

truerangefunc <- function(v, cols){
  pmax(v[2, (cols+1):(2*cols)] - v[2,1:cols],
    abs(v[2, 1:cols]-v[1, (2*cols + 1):(3*cols)]),
    abs(v[1, (cols+1):(2*cols)]-v[2, (2*cols + 1):(3*cols)]))
}

cols <- ncol(SUBDATA[["Close"]])
TR <- mcTimeSeries(cbind(SUBDATA[["Low"]], SUBDATA[["High"]], SUBDATA[["Close"]]),
                    function(v) truerangefunc(v, cols), FALSE, 2, workers)

# Calculate ATR with SMA method
ATR <- mcTimeSeries(TR, mean, TRUE, 20, workers)

ROLLMIN <- mcTimeSeries(SUBDATA[["Close"]], min, TRUE, 100, workers)
ROLLMAX <- mcTimeSeries(SUBDATA[["Close"]], max, TRUE, 100, workers)

m_plus <- (ROLLMAX - SUBDATA[["Close"]]) / ATR
m_minus <- (SUBDATA[["Close"]] - ROLLMIN) / ATR


RS <- mcTimeSeries(SUBRETURN,
                   function(v) mean(v[v>0], na.rm = T) / mean(v[v<0], na.rm = T),
                   TRUE, 20, workers)

RSI <- mcTimeSeries( RS, function(v) 100 - (100 / (1 + v)), FALSE, 1, workers)



entryfunc <- function(v, cols){
  
  goshort <- v[2,1:cols] <= 2 &
    (v[1,(2*cols+1):(3*cols)] > 70 &
      v[2,(2*cols+1):(3*cols)] <= 70 ) 

  golong <- v[2,(cols+1):(2*cols)] <= 2 &
  (v[1,(2*cols+1):(3*cols)] < 30 &
    v[2,(2*cols+1):(3*cols)] >= 30 ) 
  
  return( as.numeric(golong) - as.numeric(goshort) )
  
}

ENTRY <- mcTimeSeries(cbind(m_plus, m_minus, RSI),
                      function(v) entryfunc(v, cols), FALSE, 2, workers)


FAVOR <- mcTimeSeries(SUBRETURN, mean, TRUE, 20, workers)

exitfunc <- function(v){
  cols <- ncol(SUBDATA[["Close"]])
  exitlong <- as.numeric(v > 70 | v < 15)
  exitshort <- as.numeric(v < 30 | v > 85)
  return( exitlong - exitshort )
}

EXIT <- mcTimeSeries(RSI, exitfunc, FALSE, 1, workers)

K <- 20

RESULTS <- simulate(SUBDATA[["Open"]], SUBDATA[["Close"]],
                    ENTRY, EXIT, FAVOR,
                    maxLookback, K, 100000,
                    0.0005, 0.01, 3.5, 0,
                    TRUE, 0)
