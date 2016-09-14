n1 <- 5
n2 <- 34
MACDseries <-
rollapply(DATA[["Close"]][,exampleset],
          width = n2,
          FUN = function(v) mean(v[(n2 - n1 + 1):n2]) - mean(v),
          by.column = TRUE,
          fill = NA,
          align = "right")
