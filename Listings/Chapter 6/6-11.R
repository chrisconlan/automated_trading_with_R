# Using rollapply(), automatically pre-allocated
RETURN <- foreach( i = 1:workers, .combine = rbind,
                   .packages = "zoo") %dopar% {
  
  jRange <- delegate( i = i, n = nrow(DATA[["Close"]]), k = k, p = workers)
  
  rollapply(DATA[["Close"]][jRange,],
      width = k,
      FUN = function(v) (v[2,]/v[1,]) - 1,
      align = "right",
      by.column = FALSE,
      na.pad = FALSE)
  
}
# Completes in 22.58 seconds

