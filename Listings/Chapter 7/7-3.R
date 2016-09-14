SUBDATA <- lapply(DATA, function(v) v[-(1:3500),])
SUBRETURN <- RETURN[-(1:3500),]

n1 <- 20
n2 <- 100
maxLookback <- max(n2, n1) + 1

SD <- mcTimeSeries(SUBDATA[["Close"]],
                   function(v) sd(v, na.rm = TRUE),
                   TRUE, n1, workers)

MOVAVG <- mcTimeSeries(SUBDATA[["Close"]], 
                       function(v) mean(v, na.rm = TRUE),
                       TRUE, n1, workers)

LONGMOVAVG <- mcTimeSeries(SUBDATA[["Close"]], 
                           function(v) mean(v, na.rm = TRUE),
                           TRUE, n2, workers)

bt <- (SUBDATA[["Close"]] - MOVAVG) / SD
Bt <- (MOVAVG - LONGMOVAVG) / SD


triggerfunc <- function(v, columns){
  
  goLong <- as.numeric(
    ((v[2,1:columns] >= 1 & v[2,1:columns] < 3) | v[2,1:columns] <= -3) &
    (v[1,(columns+1):(2*columns)] >= -2 & v[2,(columns+1):(2*columns)] < -2)
  )
  
  goShort <- as.numeric(
    ((v[2,1:columns] > -3 & v[2,1:columns] <= -1) | v[2,1:columns] >= 3) &
    (v[1,(columns+1):(2*columns)] <= 2 & v[2,(columns+1):(2*columns)] > 2)
  )
  
  return( goLong - goShort )

}


exitfunc <- function(v, columns){
  
  exitLong <- as.numeric(v[2,(columns+1):(2*columns)] >= 2 &
                           v[1,(columns+1):(2*columns)] < 2)
  
  exitShort <- -as.numeric(v[1,(columns+1):(2*columns)] >= -2 &
                             v[2,(columns+1):(2*columns)] < -2)
  
  exitAll <- 999 * as.numeric( (v[1,1:columns] >= 0 & v[2,1:columns] < 0) |
                         (v[1,1:columns] <= 0 & v[2,1:columns] > 0) )
  
  out <- exitLong + exitShort + exitAll
  
  out[out > 1] <- 999
  out[!out %in% c(-1,0,1,999)] <- 0
  
  return( out )

}


columns <- ncol(SUBDATA[["Close"]])

ENTRY <- mcTimeSeries(cbind(Bt, bt), function(v) triggerfunc(v, columns),
                      FALSE, 2, workers)

FAVOR <- mcTimeSeries(SUBRETURN, mean, TRUE, n1, workers)

EXIT <- mcTimeSeries(cbind(Bt, bt), function(v) exitfunc(v, columns),
                     FALSE, 2, workers)

K <- 20

RESULTS <- simulate(SUBDATA[["Open"]], SUBDATA[["Close"]],
                    ENTRY, EXIT, FAVOR,
                    maxLookback, K, 100000,
                    0.0005, 0.01, 3.5, 0,
                    TRUE, 0)

