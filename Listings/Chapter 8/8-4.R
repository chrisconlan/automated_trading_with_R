# Maximum iterations
# Max possible calls to evaluator is K * (4 * n + 1)
K <- 100

# Restart with random init when delta is below threshold
deltaThresh <- 0.05

# Set initial delta
delta <- deltaNaught <- 1

# Scale factor
sigma <- 2


# Vector theta_0
PARAM <- PARAMNaught <- c(n1 = 0, nFact = 0, nSharpe = 0, shThresh = 0)

# bounds
minVal <- c(n1 = 1, nFact = 1, nSharpe = 1, shThresh = 0.01)
maxVal <- c(n1 = 250, nFact = 10, nSharpe = 250, shThresh = .99)

np <- length(PARAM)

OPTIM <- data.frame(matrix(NA, nrow = K * (4 * np + 1), ncol = np + 1))
names(OPTIM) <- c(names(PARAM), "obj"); o <- 1

fmin <- fminNaught <- evaluate(PARAM, minVal, maxVal, negative = TRUE, y = y)
OPTIM[o,] <- c(PARAM, fmin); o <- o + 1


# Print function for reporting progress in loop
printUpdate <- function(step){
  if(step == "search"){
    cat(paste0("Search step: ", k,"|",l,"|",m, "\n"))
  } else if (step == "poll"){
    cat(paste0("Poll step: ", k,"|",l,"|",m, "\n"))
  }
  names(OPTIM)
  cat("\t", paste0(strtrim(names(OPTIM), 6), "\t"), "\n")
  cat("Best:\t",
      paste0(round(unlist(OPTIM[which.min(OPTIM$obj),]),3), "\t"), "\n")
  cat("Theta:\t",
      paste0(round(unlist(c(PARAM, fmin)),3), "\t"), "\n")
  cat("Trial:\t",
      paste0(round(as.numeric(OPTIM[o-1,]), 3), "\t"), "\n")
  cat(paste0("Delta: ", round(delta,3) , "\t"), "\n\n")
}

for( k in 1:K ){
  
  # SEARCH subroutine
  for( l in 1:np ){
    net <- (2 * rbinom(np, 1, .5) - 1) * runif(np, delta, sigma * delta)
    for( m in c(-1,1) ){
      
      testpoint <- PARAM + m * net
      ftest <- evaluate(testpoint,  minVal, maxVal, negative = TRUE, y = y)
      OPTIM[o,] <- c(testpoint, ftest); o <- o + 1
      printUpdate("search")
      
    }
  }
  
  if( any(OPTIM$obj[(o-(2*np)):(o-1)] < fmin ) ){
    
    minPos <- which.min(OPTIM$obj[(o-(2*np)):(o-1)])
    PARAM <- (OPTIM[(o-(2*np)):(o-1),1:np])[minPos,]
    fmin <- (OPTIM[(o-(2*np)):(o-1),np+1])[minPos]
    delta <- sigma * delta
    
  } else {
    
    # POLL Subroutine
    for( l in 1:np ){
      net <- delta * as.numeric(1:np == l)
      for( m in c(-1,1) ){
        
        testpoint <- PARAM + m * net
        ftest <- evaluate(testpoint,  minVal, maxVal, negative = TRUE, y = y)
        OPTIM[o,] <- c(testpoint, ftest); o <- o + 1
        printUpdate("poll")
        
      }
    }
    
    if( any(OPTIM$obj[(o-(2*np)):(o-1)] < fmin ) ){
      
      minPos <- which.min(OPTIM$obj[(o-(2*np)):(o-1)])
      PARAM <- (OPTIM[(o-(2*np)):(o-1),1:np])[minPos,]
      fmin <- (OPTIM[(o-(2*np)):(o-1),np+1])[minPos]
      delta <- sigma * delta
      
    } else {
      
      delta <- delta / sigma
      
    }
    
    
  }
  
  cat(paste0("\nCompleted Full Iteration: ", k, "\n\n"))
  
  # Restart with random initiate
  if( delta < deltaThresh ) {
    
    delta <- deltaNaught
    fmin <- fminNaught
    PARAM <- PARAMNaught + runif(n = np, min = -delta * sigma,
                                 max = delta * sigma)
    
    ftest <- evaluate(PARAM,  minVal, maxVal,
                      negative = TRUE, y = y)
    OPTIM[o,] <- c(PARAM, ftest); o <- o + 1
    
    cat("\nDelta Threshold Breached, Restarting with Random Initiate\n\n")
    
  }
  
}

# Return the best optimization in untransformed parameters
evaluate(OPTIM[which.min(OPTIM$obj),1:np], minVal, maxVal, transformOnly = TRUE)
