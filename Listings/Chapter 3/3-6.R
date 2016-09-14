# Declare new zoo data frame of adjustment factors
MULT <- DATA[["Adj Close"]] / DATA[["Close"]]

# Store Close and Open Prices in new variable "Price" and "OpenPrice"
DATA[["Price"]] <- DATA[["Close"]]
DATA[["OpenPrice"]] <- DATA[["Open"]]

# Adjust Open, High, and Low
DATA[["Open"]] <- DATA[["Open"]] * MULT
DATA[["High"]] <- DATA[["High"]] * MULT
DATA[["Low"]] <- DATA[["Low"]] * MULT

# Copy Adjusted Close to Close
DATA[["Close"]] <- DATA[["Adj Close"]]

# Delete Adjusted Close
DATA[["Adj Close"]] <- NULL

