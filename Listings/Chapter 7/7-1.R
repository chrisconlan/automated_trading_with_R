equNA <- function(v){
   o <- which(!is.na(v))[1]
   return(ifelse(is.na(o), length(v)+1, o))
}

simulate <- function(OPEN, CLOSE,
                     ENTRY, EXIT, FAVOR, 
                     maxLookback, maxAssets, startingCash,
                     slipFactor, spreadAdjust, flatCommission, perShareCommission,
                     verbose = FALSE, failThresh = 0,
                     initP = NULL, initp = NULL){
  

# Step 1
if( any( dim(ENTRY) != dim(EXIT)  ) |
    any( dim(EXIT)  != dim(FAVOR) ) |
    any( dim(FAVOR) != dim(CLOSE) ) |
    any( dim(CLOSE) != dim(OPEN)) ) 
  stop( "Mismatching dimensions in ENTRY, EXIT, FAVOR, CLOSE, or OPEN.")
  
if( any( names(ENTRY) != names(EXIT)) |
  any( names(EXIT)  != names(FAVOR) ) |
  any( names(FAVOR) != names(CLOSE) ) |
  any( names(CLOSE) != names(OPEN)  ) |
  is.null(names(ENTRY)) | is.null(names(EXIT)) |
  is.null(names(FAVOR)) | is.null(names(CLOSE)) |
  is.null(names(OPEN)) )
  stop( "Mismatching or missing column names in ENTRY, EXIT, FAVOR, CLOSE, or OPEN.")
  
  
FAVOR <- zoo(t(apply(FAVOR, 1, function(v) ifelse(is.nan(v) | is.na(v), 0, v) )),
             order.by = index(CLOSE))
  
  
# Step 2
K <- maxAssets
k <- 0
C <- rep(startingCash, times = nrow(CLOSE))
S <- names(CLOSE)

P <- p <- zoo( matrix(0, ncol=ncol(CLOSE), nrow=nrow(CLOSE)),
               order.by = index(CLOSE) )

if( !is.null( initP ) & !is.null( initp ) ){
  P[1:maxLookback,] <-
    matrix(initP, ncol=length(initP), nrow=maxLookback, byrow = TRUE)
  p[1:maxLookback,] <-
    matrix(initp, ncol=length(initp), nrow=maxLookback, byrow = TRUE)
}

names(P) <- names(p) <- S

equity <- rep(NA, nrow(CLOSE))



rmNA <- pmax(unlist(lapply(FAVOR, equNA)),
     unlist(lapply(ENTRY, equNA)),
     unlist(lapply(EXIT, equNA)))

for( j in 1:ncol(ENTRY) ){
  toRm <- rmNA[j]
  if( toRm > (maxLookback + 1) &
      toRm < nrow(ENTRY) ){
    FAVOR[1:(toRm-1),j] <- NA
    ENTRY[1:(toRm-1),j] <- NA
    EXIT[1:(toRm-1),j] <- NA
  }
}


# Step 3
for( i in maxLookback:(nrow(CLOSE)-1) ){
    
  # Step 4
  C[i+1] <- C[i]
  P[i+1,] <- as.numeric(P[i,])
  p[i+1,] <- as.numeric(p[i,])
  
  longS <- S[which(P[i,] > 0)]
  shortS <- S[which(P[i,] < 0)]
  k <- length(longS) + length(shortS)
  
  # Step 5
  longTrigger <- setdiff(S[which(ENTRY[i,] == 1)], longS)
  shortTrigger <- setdiff(S[which(ENTRY[i,] == -1)], shortS)
  trigger <- c(longTrigger, shortTrigger)
  
  if( length(trigger) > K ) {
    
    keepTrigger <- trigger[order(c(as.numeric(FAVOR[i,longTrigger]),
                                   -as.numeric(FAVOR[i,shortTrigger])),
                                 decreasing = TRUE)][1:K]
    
    longTrigger <- longTrigger[longTrigger %in% keepTrigger]
    shortTrigger <- shortTrigger[shortTrigger %in% keepTrigger]
    
    trigger <- c(longTrigger, shortTrigger)
    
  }
  
  triggerType <- c(rep(1, length(longTrigger)), rep(-1, length(shortTrigger)))
  
  
  # Step 6
  longExitTrigger <- longS[longS %in%
                             S[which(EXIT[i,] == 1 | EXIT[i,] == 999)]]
  
  shortExitTrigger <- shortS[shortS %in%
                               S[which(EXIT[i,] == -1 | EXIT[i,] == 999)]]
  
  exitTrigger <- c(longExitTrigger, shortExitTrigger)
  
  
  # Step 7
  needToExit <- max( (length(trigger) - length(exitTrigger)) - (K - k), 0)
  
  if( needToExit > 0 ){
    
    toExitLongS <- setdiff(longS, exitTrigger)
    toExitShortS <- setdiff(shortS, exitTrigger)
    
    toExit <- character(0)
    
    for( counter in 1:needToExit ){
      if( length(toExitLongS) > 0 & length(toExitShortS) > 0 ){
        if( min(FAVOR[i,toExitLongS]) < min(-FAVOR[i,toExitShortS]) ){
          pullMin <- which.min(FAVOR[i,toExitLongS])
          toExit <- c(toExit, toExitLongS[pullMin])
          toExitLongS <- toExitLongS[-pullMin]
        } else {
          pullMin <- which.min(-FAVOR[i,toExitShortS])
          toExit <- c(toExit, toExitShortS[pullMin])
          toExitShortS <- toExitShortS[-pullMin]
        }
      } else if( length(toExitLongS) > 0 & length(toExitShortS) == 0 ){
        pullMin <- which.min(FAVOR[i,toExitLongS])
        toExit <- c(toExit, toExitLongS[pullMin])
        toExitLongS <- toExitLongS[-pullMin]
      } else if( length(toExitLongS) == 0 & length(toExitShortS) > 0 ){
        pullMin <- which.min(-FAVOR[i,toExitShortS])
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
  
  
  # Step 9
  if( length(exitTrigger) > 0 ){
    for( j in 1:length(exitTrigger) ){
      
      exitPrice <- as.numeric(OPEN[i+1,exitTrigger[j]])
        
      effectivePrice <- exitPrice * (1 - exitTriggerType[j] * slipFactor) - 
        exitTriggerType[j] * (perShareCommission + spreadAdjust)
      
      if( exitTriggerType[j] == 1 ){
        
        C[i+1] <- C[i+1] + 
          ( as.numeric( P[i,exitTrigger[j]] ) * effectivePrice )
        - flatCommission
        
      } else {
        
        C[i+1] <- C[i+1] - 
          ( as.numeric( P[i,exitTrigger[j]] ) *
              ( 2 * as.numeric(p[i, exitTrigger[j]]) -  effectivePrice ) )
        - flatCommission
      }
      
      P[i+1, exitTrigger[j]] <- 0
      p[i+1, exitTrigger[j]] <- 0
      
      k <- k - 1
      
    }
  }
  
  
  # Step 10
  if( length(trigger) > 0 ){
    for( j in 1:length(trigger) ){
      
      entryPrice <- as.numeric(OPEN[i+1,trigger[j]]) 
      
      effectivePrice <- entryPrice * (1 + triggerType[j] * slipFactor) + 
        triggerType[j] * (perShareCommission + spreadAdjust) 
      
      P[i+1,trigger[j]] <- triggerType[j] * 
        floor( ( (C[i+1] - flatCommission) / (K - k) ) / effectivePrice )
      
      p[i+1,trigger[j]] <- effectivePrice
      
      C[i+1] <- C[i+1] - 
        ( triggerType[j] * as.numeric(P[i+1,trigger[j]]) * effectivePrice )
      - flatCommission
      
      k <- k + 1
      
    }
  }
  
  
  # Step 11
  equity[i] <- C[i+1]
  for( s in S[which(P[i+1,] > 0)] ){
    equity[i] <- equity[i] +
      as.numeric(P[i+1,s]) *  
      as.numeric(OPEN[i+1,s])
  }
  
  for( s in S[which(P[i+1,] < 0)] ){
    equity[i] <- equity[i] -
      as.numeric(P[i+1,s]) * 
      ( 2 * as.numeric(p[i+1,s]) - as.numeric(OPEN[i+1,s]) )
  }
  
  if( equity[i] < failThresh ){
    warning("\n*** Failure Threshold Breached ***\n")
    break
  }
  
  # Step 12
  if( verbose ){
    if( i %% 21 == 0 ){
      cat(paste0("################################## ",
                 round(100 * (i - maxLookback) /
                         (nrow(CLOSE) - 1 - maxLookback), 1), "%",
                 " ##################################\n"))
      cat(paste("Date:\t",as.character(index(CLOSE)[i])), "\n")
      cat(paste0("Equity:\t", " $", signif(equity[i], 5), "\n"))
      cat(paste0("CAGR:\t ",
                 round(100 * ((equity[i] / (equity[maxLookback]))^
                                (252/(i - maxLookback + 1)) - 1), 2),
                 "%"))
      cat("\n")
      cat("Assets:\t", S[P[i+1,] != 0])
      cat("\n\n")
    }
  }
  


}

# Step 13
return(list(equity = equity, C = C, P = P, p = p))

}
