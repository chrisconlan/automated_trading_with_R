y <- 2014

minVal <- c(n1 = 1, nFact = 1, nSharpe = 1, shThresh = .01)
maxVal <- c(n1 = 150, nFact = 5, nSharpe = 200, shThresh = .99)

PARAM <- c(n1 = -2, nFact = -2, nSharpe = -2, shThresh = 0)  

# Declare entry function for use inside evaluator
entryfunc <- function(v, shThresh){
  cols <- ncol(v)/2
  as.numeric(v[1,1:cols] <= 0 &
               v[2,1:cols] > 0 &
               v[2,(cols+1):(2*cols)] >
               quantile(v[2,(cols+1):(2*cols)],
                        shThresh, na.rm  = TRUE)
             )
}



evaluate <- function(PARAM, minVal = NA, maxVal = NA, y = 2014,
                     transform = FALSE, verbose = FALSE,
                     negative = FALSE, transformOnly = FALSE,
                     returnData = FALSE, accountParams = NULL){
  
  # Step 1
  # Convert and declare parameters if they exist on unbounded (-inf,inf) domain
  if( transform | transformOnly ){
    PARAM <- minVal +
      (maxVal - minVal) * unlist(lapply( PARAM, function(v) (1 + exp(-v))^(-1) ))
    if( transformOnly ){
    return(PARAM)
    }
  }

  # Step 2  
  # Declare n1 as itself, n2 as a multiple of n1 defined by nFact,
  # and declare the length and threshold in sharpe ratio for FAVOR.
  # This section should handle rounding and logical bounding
  # in moving
  n1 <- max(round(PARAM[["n1"]]), 2)
  n2 <- max(round(PARAM[["nFact"]] * PARAM[["n1"]]), 3, n1+1)
  nSharpe <- max(round(PARAM[["nSharpe"]]), 2)
  shThresh <- max(0, min(PARAM[["shThresh"]], .99))
  maxLookback <- max(n1, n2, nSharpe) + 1
  

  # Step 3
  # Subset data according to range of years y
  period <- 
    index(DATA[["Close"]]) >= strptime(paste0("01-01-", y[1]), "%d-%m-%Y") &
    index(DATA[["Close"]]) < strptime(paste0("01-01-", y[length(y)]+1), "%d-%m-%Y")
  
  period <- period |
    ((1:nrow(DATA[["Close"]]) > (which(period)[1] - maxLookback)) &
    (1:nrow(DATA[["Close"]]) <= (which(period)[sum(period)]) + 1))
  
  
  # Step 4
  CLOSE <- DATA[["Close"]][period,]
  OPEN <- DATA[["Open"]][period,]
  SUBRETURN <- RETURN[period,]
  
  
  # Step 5
  # Compute inputs for long-only MACD as in Listing 7.2
  # Code is optimized for speed using functions from caTools and zoo
  require(caTools)
  
  INDIC <- zoo(runmean(CLOSE, n1, endrule = "NA", align = "right") -
                 runmean(CLOSE, n2, endrule = "NA", align = "right"),
               order.by = index(CLOSE))
  names(INDIC) <- names(CLOSE)
  
  
  RMEAN <- zoo(runmean(SUBRETURN, n1, endrule = "NA", align = "right"),
               order.by = index(SUBRETURN))
  
  FAVOR <- RMEAN / runmean( (SUBRETURN - RMEAN)^2, nSharpe,
                            endrule = "NA", align = "right" )
  names(FAVOR) <- names(CLOSE)
  
  
  ENTRY <- rollapply(cbind(INDIC, FAVOR),
                     FUN = function(v) entryfunc(v, shThresh),
                     width = 2,
                     fill = NA,
                     align = "right",
                     by.column = FALSE)
  names(ENTRY) <- names(CLOSE)
  
  EXIT <- zoo(matrix(0, ncol=ncol(CLOSE), nrow=nrow(CLOSE)),
              order.by = index(CLOSE))
  names(EXIT) <- names(CLOSE)
  
  
  # Step 6
  # Max shares to hold
  K <- 10
  
  # Simulate and store results
  if( is.null(accountParams) ){
    RESULTS <- simulate(OPEN, CLOSE,
            ENTRY, EXIT, FAVOR,
            maxLookback, K, 100000,
            0.001, 0.01, 3.5, 0,
            verbose, 0)
  } else {
    RESULTS <- simulate(OPEN, CLOSE,
        ENTRY, EXIT, FAVOR,
        maxLookback, K, accountParams[["C"]],
        0.001, 0.01, 3.5, 0,
        verbose, 0,
        initP = accountParams[["P"]], initp = accountParams[["p"]])
  }

  # Step 7
  if(!returnData){
    
    # Compute and return sharpe ratio
    v <- RESULTS[["equity"]]
    returns <- ( v[-1] / v[-length(v)] ) - 1
    out <- mean(returns, na.rm = T) / sd(returns, na.rm = T)
    if(!is.nan(out)){
      if( negative ){
        return( -out )
      } else {
        return( out )
      }    
    } else {
      return(0)
    }

  } else {
    return(RESULTS)
  }
  
}

# To test value of objective function
objective <- evaluate(PARAM, minVal, maxVal, y)

