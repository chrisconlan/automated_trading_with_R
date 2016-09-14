# Listing 3.6
MULT <- DATA[["Adj Close"]] / DATA[["Close"]]

DATA[["Price"]] <- DATA[["Close"]]
DATA[["OpenPrice"]] <- DATA[["Open"]]

DATA[["Open"]] <- DATA[["Open"]] * MULT
DATA[["High"]] <- DATA[["High"]] * MULT
DATA[["Low"]] <- DATA[["Low"]] * MULT
DATA[["Close"]] <- DATA[["Adj Close"]]

DATA[["Adj Close"]] <- NULL
