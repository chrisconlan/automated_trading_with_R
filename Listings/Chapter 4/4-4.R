n <- 10
customseries <-
  rollapply(DATA[["Close"]][,exampleset],
            width = n,
            FUN = function(v) cor(v, n:1)^2 * ((v[n] - v[1])/n),
            by.column = TRUE,
            fill = NA,
            align = "right")
