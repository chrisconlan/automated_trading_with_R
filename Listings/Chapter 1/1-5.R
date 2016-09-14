MD <- function(curve, n = 1){
  
  time <- length(curve) 
  v <- rep(NA, (time * (time - 1)) / 2)
  k <- 1
  for(i in 1:(length(curve)-1)){
    for(j in (i+1):length(curve)){
      v[k] <- curve[i] - curve[j]
      k <- k + 1
    }
  }
  
  m <- rep(NA, length(n))
  for(i in 1:n){
    m[i] <- max(v)
    v[which.max(v)] <- -Inf
  }
  
  return(m)
  
}
