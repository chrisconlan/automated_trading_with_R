# Pad with NA's to perserver dimension equality
NAPAD <- zoo(matrix(NA, nrow = 1, ncol = ncol(DATA[["Close"]])),
             order.by = index(DATA[["Close"]])[1])
names(NAPAD) <- names(DATA[["Close"]])

# Compute Daily Close-to-Close Returns
RETURN <- rbind( NAPAD, ( DATA[["Close"]] / lag(DATA[["Close"]], k = -1) ) - 1 )

# Compute Overnight Returns (Close-to-Open)
OVERNIGHT <- rbind( NAPAD, ( DATA[["Open"]] / lag(DATA[["Close"]], k = -1) ) - 1 )

