mcTimeSeries <- function( data, tsfunc, byColumn, windowSize, workers, ... ){
  
  # For Windows compatability
  args <- names(mget(ls()))
  export <- ls(.GlobalEnv)
  export <- export[!export %in% args]
  
  # foreach powerhouse
  SERIES <- foreach( i = 1:workers, .combine = rbind,
                     .packages = loadedNamespaces(), .export = export) %dopar% {
    
    jRange <- delegate( i = i, n = nrow(data), k = windowSize, p = workers)
  
    rollapply(data[jRange,],
      width = windowSize,
      FUN = tsfunc,
      align = "right",
      by.column = byColumn)

  }
  
  # Correct formatting of column names and dimensions
  names(SERIES) <- gsub("\\..+", "", names(SERIES))
  
  if( windowSize > 1){
    PAD <- zoo(matrix(nrow = windowSize-1, ncol = ncol(SERIES), NA),
                order.by = index(data)[1:(windowSize-1)])
    names(PAD) <- names(SERIES)
    SERIES <- rbind(PAD, SERIES)
  }
  
  if(is.null(names(SERIES))){
    names(SERIES) <-  gsub("\\..+", "", names(data)[1:ncol(SERIES)])
  } 
  
  # Return results
  return(SERIES)
  
}
