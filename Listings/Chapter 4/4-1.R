n <- 20
meanseries <-
rollapply(DATA[["Close"]][,exampleset],
          width = n,
          FUN = mean,
          by.column = TRUE,
          fill = NA,
          align = "right")
