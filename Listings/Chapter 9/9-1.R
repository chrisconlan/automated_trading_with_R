# Normally declared by your strategy
FAVOR <- rnorm(ncol(DATA[["Close"]]))
ENTRY <- rbinom(ncol(DATA[["Close"]]), 1, .005) -
  rbinom(ncol(DATA[["Close"]]), 1, .005)
EXIT <-  rbinom(ncol(DATA[["Close"]]), 1, .8) -
  rbinom(ncol(DATA[["Close"]]), 1, .8)

# Normally fetched from brokerage
currentlyLong <- c("AA", "AAL", "AAPL")
currentlyShort <- c("RAI", "RCL", "REGN")
S <- names(DATA[["Close"]])
initP <- (S %in% currentlyLong) - (S %in% currentlyShort)

names(initP) <-
  names(FAVOR) <-
  names(ENTRY) <-
  names(EXIT) <-
  names(DATA[["Close"]])


# At this point we have established everything normally
# taken care of by your trading strategy.
# Given named vectors of length ncol(DATA[["Close"]])
# initP, FAVOR, ENTRY, and EXIT, we proceed.

maxAssets <- 10
startingCash <- 100000

K <- maxAssets
k <- 0
C <- c(startingCash, NA)
S <- names(DATA[["Close"]])
P <- initP


# Step 4
longS <- S[which(P > 0)]
shortS <- S[which(P < 0)]
k <- length(longS) + length(shortS)

# Step 5
longTrigger <- setdiff(S[which(ENTRY == 1)], longS)
shortTrigger <- setdiff(S[which(ENTRY == -1)], shortS)
trigger <- c(longTrigger, shortTrigger)

if( length(trigger) > K ) {
  
  keepTrigger <- trigger[order(c(as.numeric(FAVOR[longTrigger]),
                                  -as.numeric(FAVOR[shortTrigger])),
                                decreasing = TRUE)][1:K]
  
  longTrigger <- longTrigger[longTrigger %in% keepTrigger]
  shortTrigger <- shortTrigger[shortTrigger %in% keepTrigger]
  
  trigger <- c(longTrigger, shortTrigger)
  
}

triggerType <- c(rep(1, length(longTrigger)), rep(-1, length(shortTrigger)))


# Step 6
longExitTrigger <- longS[longS %in% S[which(EXIT == 1 | EXIT == 999)]]

shortExitTrigger <- shortS[shortS %in% S[which(EXIT == -1 | EXIT == 999)]]

exitTrigger <- c(longExitTrigger, shortExitTrigger)


# Step 7
needToExit <- max( (length(trigger) - length(exitTrigger)) - (K - k), 0)

if( needToExit > 0 ){
  
  toExitLongS <- setdiff(longS, exitTrigger)
  toExitShortS <- setdiff(shortS, exitTrigger)
  
  toExit <- character(0)
  
  for( counter in 1:needToExit ){
    if( length(toExitLongS) > 0 & length(toExitShortS) > 0 ){
      if( min(FAVOR[toExitLongS]) < min(-FAVOR[toExitShortS]) ){
        pullMin <- which.min(FAVOR[toExitLongS])
        toExit <- c(toExit, toExitLongS[pullMin])
        toExitLongS <- toExitLongS[-pullMin]
      } else {
        pullMin <- which.min(-FAVOR[toExitShortS])
        toExit <- c(toExit, toExitShortS[pullMin])
        toExitShortS <- toExitShortS[-pullMin]
      }
    } else if( length(toExitLongS) > 0 & length(toExitShortS) == 0 ){
      pullMin <- which.min(FAVOR[toExitLongS])
      toExit <- c(toExit, toExitLongS[pullMin])
      toExitLongS <- toExitLongS[-pullMin]
    } else if( length(toExitLongS) == 0 & length(toExitShortS) > 0 ){
      pullMin <- which.min(-FAVOR[toExitShortS])
      toExit <- c(toExit, toExitShortS[pullMin])
      toExitShortS <- toExitShortS[-pullMin]
    }
  }
  
  longExitTrigger <- c(longExitTrigger, longS[longS %in% toExit])
  shortExitTrigger <- c(shortExitTrigger, shortS[shortS %in% toExit])
  
}

# Step 8
exitTrigger <- c(longExitTrigger, shortExitTrigger)
exitTriggerType <- c(rep(1, length(longExitTrigger)),
                     rep(-1, length(shortExitTrigger)))



  


# Output planned trades
setwd(rootdir)

# First exit these
write.csv(file = "stocksToExit.csv",
          data.frame(list(sym = exitTrigger, type = exitTriggerType)))

# Then enter these
write.csv(file = "stocksToEnter.csv",
          data.frame(list(sym = trigger, type = triggerType)))

