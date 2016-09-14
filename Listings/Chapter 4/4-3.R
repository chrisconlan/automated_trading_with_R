n <- 20
rollsd <- rollapply(DATA[["Close"]][,exampleset],
          width = n,
          FUN = sd,
          by.column = TRUE,
          fill = NA,
          align = "right")

upperseries <- meanseries + 2 * rollsd
lowerseries <- meanseries + 2 - rollsd
