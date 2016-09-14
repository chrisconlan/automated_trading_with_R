# Sequentially re-allocating space in a for loop
RETURN <- NULL
for(i in 2:nrow(DATA[["Close"]])){
  RETURN <- rbind(RETURN, t((matrix(DATA[["Close"]][i, ]) /
                               matrix(DATA[["Close"]][i-1, ])) - 1))
}
RETURN <- zoo( RETURN, order.by = index(DATA[["Close"]])[-1])
# 99.68 seconds



# Pre-allocating space and computing in a for loop
RETURN <- zoo(matrix(ncol = ncol(DATA[["Close"]]),
                         nrow = nrow(DATA[["Close"]])),
              order.by = index(DATA[["Close"]]))

for(i in 2:nrow(DATA[["Close"]])){
  RETURN[i,] <- t((matrix(DATA[["Close"]][i, ]) / matrix(DATA[["Close"]][i-1, ])) - 1)
}
# 54.34 seconds

