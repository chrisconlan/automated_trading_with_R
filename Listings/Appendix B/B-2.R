mcTimeSeries <- function( data, tsfunc, byColumn, windowSize, workers ){
  
  SERIES <- foreach( i = 1:workers, .combine = rbind ) %dopar% {
    
  jRange <- delegate( i = i, n = nrow(data), k = windowSize, p = workers)

  rollapply(data[jRange,],
    width = windowSize,
    FUN = tsfunc,
    align = "right",
    by.column = byColumn)

  }
  
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
  
  return(SERIES)
  
}
