# Using the "lag" method introduced in Listing 3.8
RETURN <- ( DATA[["Close"]] / lag(DATA[["Close"]], k = -1) ) - 1
# 0.459 seconds
