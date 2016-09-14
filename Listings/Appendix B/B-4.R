# Declare parameter alpha as function parameter
exitfunc <- function(v, alpha) { 
  # Body of developer's new exit function  
}

# Declare function object exitfunc as
# function parameter to evaluator
evaluate <- function(... , exitfunc){
  
  # Body of the evaluate function
  
  # alpha exists in the function scope
  # of the evaluator
  alpha <- 0.5
  
  # Dynamically declare function object in
  # mcTimeSeries. Pass exitfunc and alpha
  # in the ellipses of the call because
  # the second argument depends on them.
  EXIT <- mcTimeSeries(CLOSE,
                       function(v) exitfunc(v, alpha),
                       TRUE, 20, workers,
                       exitfunc, alpha)
    
  # Remainder of the evaluate function
  
}


optimize <- function(... , exitfunc){
  
  # Alter all calls to evaluate to include
  # new function object parameter exitfunc
  
  # Body of the optimzer
  
  evaluate( ... , exitfunc )
  
  # Body of the optimzer
  
  evaluate( ... , exitfunc )
  
  # And so on. There are typically many calls
  # to evaluate() within the optimizer.
  
}
