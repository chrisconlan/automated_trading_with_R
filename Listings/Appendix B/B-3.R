exitfunc <- function(v) { 
  # Body of developer's new exit function  
}

evaluate(...) <- function(...){
  
  # Body of the evaluate function
  
  EXIT <- mcTimeSeries(CLOSE, exitfunc, TRUE, 20, workers)
    
  # Remainder of the evaluate function
  
}
