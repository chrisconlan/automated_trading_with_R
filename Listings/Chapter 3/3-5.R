voltemp <-
  c(DATA[["Volume"]][index(DATA[["Close"]]) %in% c(index(temp)[1:3]), "KORS"],
      zoo(NA, order.by = index(temp)[4]),
      DATA[["Volume"]][index(DATA[["Close"]]) %in% c(index(temp)[5]), "KORS"],
      zoo(NA, order.by = index(temp)[6:7]),
      DATA[["Volume"]][index(DATA[["Close"]]) %in% c(index(temp[8:10])), "KORS"])

# Volume-Weighted Smoothed Replacement Function
volfun <- function(v, n, vol){
  m <- (n + 1)/2
  if(is.na(v[m])){
    a <- max(which(!is.na(v) & seq(1:n) < m))
    b <- min(which(!is.na(v) & seq(1:n) > m))
    return(((v[a] + ((m-a-1)/(b-a)) * (v[b] - v[a])) * vol[a] +
            (v[a] + ((m-a+1)/(b-a)) * (v[b] - v[a])) * vol[b]) /
             (vol[a] + vol[b]))
  } else {
    return(v[m])
  }
}

maxconsec <- 5
volrep <- rollapply(cbind(temp, voltemp),
          width = maxconsec,
          FUN = function(v) volfun(v[,1], n = maxconsec, v[,2]),
          by.column = FALSE,
          align = "center") 
