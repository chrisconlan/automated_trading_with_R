# Listing 9.1
setwd(DIR[["plan"]])


# Normally declared by your strategy.
# Long-only MACD is computed with rollapply()
# here for sake of example.
n1 <- 5
n2 <- 34
nSharpe <- 20
shThresh <- 0.50

INDIC <- rollapply(DATA[["Close"]][nrow(DATA[["Close"]]) - n2:0, ],
                   width = n2,
                   FUN = function(v) mean(v[(n2 - n1 + 1):n2]) - mean(v),
                   by.column = TRUE,
                   align = "right")



FAVOR <- rollapply(DATA[["Close"]][nrow(DATA[["Close"]]) - nSharpe:0, ],
                      FUN = function(v) mean(v, na.rm = TRUE)/sd(v, na.rm = TRUE),
                      by.column = TRUE, 
                      width = nSharpe,
                      align = "right")


entryfunc <- function(v, shThresh){
  cols <- ncol(v) / 2
  as.numeric(v[1,1:cols] <= 0 &
               v[2,1:cols] > 0 &
               v[2,(cols+1):(2*cols)] >
               quantile(v[2,(cols+1):(2*cols)],
                        shThresh, na.rm  = TRUE)
             )
}


cols <- ncol(INDIC)

ENTRY <- rollapply(cbind(INDIC, FAVOR),
                      function(v) entryfunc(v, cols),
                      by.column = FALSE, 
                      width = 2,
                      align = "right")


# ***IMPORTANT***
# The quick version used in the PLAN job accepts named vectors
# respresenting the most recent single row of ENTRY, FAVOR, and EXIT.
# These lines convert the zoo/data frame/matrix objects computed
# in the above lines to named vectors of the last row of data.

FAVOR <- as.numeric(FAVOR[nrow(FAVOR),])
names(FAVOR) <- names(DATA[["Close"]])

ENTRY <- as.numeric(ENTRY[nrow(ENTRY),])
names(ENTRY) <- names(DATA[["Close"]])

EXIT <- zoo(matrix(0, ncol=ncol(DATA[["Close"]]), nrow = 1),
            order.by = index(DATA[["Close"]]))
names(EXIT) <- names(DATA[["Close"]])



# Normally fetched from brokerage.
# These are arbitrarily declared here.
# Users need to fetch this information from the brokerage
# for production use.
currentlyLong <- c("AA", "AAL", "AAPL")
currentlyShort <- c("")
S <- names(DATA[["Close"]])
initP <- (S %in% currentlyLong) - (S %in% currentlyShort)
cashOnHand <- 54353.54



names(initP) <-
  names(FAVOR) <-
  names(ENTRY) <-
  names(EXIT) <-
  names(DATA[["Close"]])


# At this point we have established everything normally
# taken care of by your strategy.
# Given named vectors of length ncol(DATA[["Close"]])
# initP, FAVOR, ENTRY, and EXIT

maxAssets <- CONFIG[["maxAssets"]]

K <- maxAssets
k <- 0
C <- c(cashOnHand, NA)
S <- names(DATA[["Close"]])
P <- initP


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
# taken care of by your strategy.
# Given named vectors of length ncol(DATA[["Close"]])
# initP, FAVOR, ENTRY, and EXIT

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
  

setwd(DIR[["plan"]])

# First exit these
write.csv(file = "stocksToExit.csv",
          data.frame(list(sym = exitTrigger, type = exitTriggerType)))

# Then enter these
write.csv(file = "stocksToEnter.csv",
          data.frame(list(sym = trigger, type = triggerType)))
