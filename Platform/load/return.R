# Listing 3.8
NAPAD <- zoo(matrix(NA, nrow = 1, ncol = ncol(DATA[["Close"]])), order.by = index(DATA[["Close"]])[1])
names(NAPAD) <- names(DATA[["Close"]])

RETURN <- rbind( NAPAD, ( DATA[["Close"]] / lag(DATA[["Close"]], k = -1) ) - 1 )

OVERNIGHT <- rbind( NAPAD, ( DATA[["Open"]] / lag(DATA[["Close"]], k = -1) ) - 1 )
