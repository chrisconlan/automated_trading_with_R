delegate <- function( i = i, n = n, k = k, p = workers ){
  nOut <- n - k + 1
  nProc <- ceiling( nOut / p )
  return( (( i - 1 ) * nProc + 1) : min(i * nProc + k - 1, n) )
}

# Test i as 1 through 4 to verify it matches our example
lapply(1:4, function(i) delegate(i, n = 100, k = 5, p = 4))

