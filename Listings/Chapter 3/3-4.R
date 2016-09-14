# Linearly Smoothed Replacement Function
linearfun <- function(v, n){
  m <- (n + 1)/2
  if(is.na(v[m])){
    a <- max(which(!is.na(v) & seq(1:n) < m))
    b <- min(which(!is.na(v) & seq(1:n) > m))
    return(((b - m)/(b - a)) * v[a] +
            ((m - a)/(b - a)) * v[b])
  } else {
    return(v[m])
  }
}

maxconsec <- 5
linearrep <- rollapply(temp,
          width = maxconsec,
          FUN = linearfun,
          n = maxconsec,
          by.column = TRUE,
          align = "center") 
