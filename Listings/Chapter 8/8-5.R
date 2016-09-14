K <- maxIter <- 200

# Vector theta_0
initDelta <- 6
deltaThresh <- 0.05
PARAM <- PARAMNaught <-
  c(n1 = 0, nFact = 0, nSharpe = 0, shThresh = 0) - initDelta/2

# bounds
minVal <- c(n1 = 1, nFact = 1, nSharpe = 1, shThresh = 0.01)
maxVal <- c(n1 = 250, nFact = 10, nSharpe = 250, shThresh = .99)

# Optimization parameters
alpha <- 1
gamma <- 2
rho <- .5
sigma <- .5


randomInit <- FALSE

np <- length(initVals)


OPTIM <- data.frame(matrix(NA, ncol = np + 1, nrow = maxIter * (2 * np + 2)))
o <- 1

SIMPLEX <- data.frame(matrix(NA, ncol = np + 1, nrow = np + 1))
names(SIMPLEX) <- names(OPTIM) <- c(names(initVals), "obj")


# Print function for reporting progress in loop
printUpdate <- function(){
  cat("Iteration: ", k, "of", K, "\n")
  cat("\t\t", paste0(strtrim(names(OPTIM), 6), "\t"), "\n")
  cat("Global Best:\t",
      paste0(round(unlist(OPTIM[which.min(OPTIM$obj),]),3), "\t"), "\n")
  cat("Simplex Best:\t",
      paste0(round(unlist(SIMPLEX[which.min(SIMPLEX$obj),]),3), "\t"), "\n")
  cat("Simplex Size:\t",
      paste0(max(round(simplexSize,3)), "\t"), "\n\n\n")
}


# Initialize SIMPLEX
for( i in 1:(np+1) ) {
    
  SIMPLEX[i,1:np] <- PARAMNaught + initDelta * as.numeric(1:np == (i-1))
  SIMPLEX[i,np+1] <- evaluate(SIMPLEX[i,1:np], minVal, maxVal, negative = TRUE,
                              y = y)
  OPTIM[o,] <- SIMPLEX[i,]
  o <- o + 1

}



# Optimization loop
for( k in 1:K ){
  
  SIMPLEX <- SIMPLEX[order(SIMPLEX[,np+1]),]
  centroid <- colMeans(SIMPLEX[-(np+1),-(np+1)])
  
  cat("Computing Reflection...\n")
  reflection <- centroid + alpha * (centroid - SIMPLEX[np+1,-(np+1)])
  
  reflectResult <- evaluate(reflection, minVal, maxVal, negative = TRUE, y = y)
  OPTIM[o,] <- c(reflection, obj = reflectResult)
  o <- o + 1
  
  if( reflectResult > SIMPLEX[1,np+1] & 
      reflectResult < SIMPLEX[np, np+1] ){
    
    SIMPLEX[np+1,] <- c(reflection, obj = reflectResult)
    
  } else if( reflectResult < SIMPLEX[1,np+1] ) {
    
    cat("Computing Expansion...\n")
    expansion <- centroid + gamma * (reflection - centroid)
    expansionResult <- evaluate(expansion,
                                minVal, maxVal, negative = TRUE, y = y)
    
    OPTIM[o,] <-  c(expansion, obj = expansionResult)
    o <- o + 1
    
    if( expansionResult < reflectResult ){
      SIMPLEX[np+1,] <- c(expansion, obj = expansionResult)
    } else {
      SIMPLEX[np+1,] <- c(reflection, obj = reflectResult)
    }
    
  } else if( reflectResult > SIMPLEX[np, np+1] ) {
    
    cat("Computing Contraction...\n")
    contract <- centroid + rho * (SIMPLEX[np+1,-(np+1)] - centroid)
    contractResult <- evaluate(contract, minVal, maxVal, negative = TRUE, y = y)
    
    OPTIM[o,] <- c(contract, obj = contractResult)
    o <- o + 1
    
    if( contractResult < SIMPLEX[np+1, np+1] ){
      
      SIMPLEX[np+1,] <- c(contract, obj = contractResult)
      
    } else {
      cat("Computing Shrink...\n")
      for( i in 2:(np+1) ){
        SIMPLEX[i,1:np] <- SIMPLEX[1,-(np+1)] +
          sigma * (SIMPLEX[i,1:np] - SIMPLEX[1,-(np+1)])
        SIMPLEX[i,np+1] <- c(obj = evaluate(SIMPLEX[i,1:np],
                                            minVal, maxVal,
                                            negative = TRUE, y = y))
      } 
      
      OPTIM[o:(o+np-1),] <- SIMPLEX[2:(np+1),]
      o <- o + np
      
    }
    
  }
  
  centroid <- colMeans(SIMPLEX[-(np+1),-(np+1)])
  simplexSize <- rowMeans(t(apply(SIMPLEX[,1:np], 1,
                                  function(v) abs(v - centroid))))
  
  if( max(simplexSize) < deltaThresh ){
    
    cat("Size Threshold Breached: Restarting with Random Initiate\n\n")
    
    for( i in 1:(np+1) ) {
    
    SIMPLEX[i,1:np] <- (PARAMNaught * 0) +
      runif(n = np, min = -initDelta, max = initDelta)
    
    SIMPLEX[i,np+1] <- evaluate(SIMPLEX[i,1:np],
                                minVal, maxVal, negative = TRUE, y = y)
    OPTIM[o,] <- SIMPLEX[i,]
    o <- o + 1

    SIMPLEX <- SIMPLEX[order(SIMPLEX[,np+1]),]
    centroid <- colMeans(SIMPLEX[-(np+1),-(np+1)])
    simplexSize <- rowMeans(t(apply(SIMPLEX[,1:np], 1, function(v) abs(v - centroid))))
    
    }
    
  }
  
  printUpdate()
  
}


# Return the best optimization in untransformed parameters
evaluate(OPTIM[which.min(OPTIM$obj),1:np], minVal, maxVal, transformOnly = TRUE)
