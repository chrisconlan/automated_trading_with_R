CMFfunc <- function(close, high, low, volume){
  apply(((2 * close - high - low) / (high - low)) * volume,
        MARGIN = 2,
        FUN = sum) /
  apply(volume,
        MARGIN = 2,
        FUN = sum)
}


n <- 20
k <- length(exampleset)
CMFseries <-
rollapply(cbind(DATA[["Close"]][,exampleset],
                DATA[["High"]][,exampleset],
                DATA[["Low"]][,exampleset],
                DATA[["Volume"]][,exampleset]),
          FUN = function(v) CMFfunc(v[,(1:k)],
                                    v[,(k+1):(2*k)],
                                    v[,(2*k + 1):(3*k)],
                                    v[,(3*k + 1):(4*k)]),
          by.column = FALSE,
          width = n,
          fill = NA,
          align = "right")

names(CMFseries) <- exampleset

