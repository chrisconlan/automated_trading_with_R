changeInEquity <- c(NA,  RESULTS[["equity"]][-1] -
                      RESULTS[["equity"]][-length(RESULTS[["equity"]])])

# Return Series as defined in Chapter 1
R <- zoo(changeInEquity / (RESULTS[["equity"]]), order.by = index(SUBDATA[["Close"]]))

plot(100 * R, type = "l", main = "Figure 7.1: Return Series for Long-Only MACD",
     ylab = "Percent Return", xlab = "")
grid()
abline( h = 0, col = 8 )

# Equity Cruve
plot(y = RESULTS[["equity"]],  x = index(SUBDATA[["Close"]]),
     type = "l", main = "Figure 7.2: Equity Curve for Long-Only MACD",
     ylab = "Account Equity ($)", xlab = "")
abline(h = RESULTS[["C"]][1])
grid()



# Sharpe Ratio
sharpeRatio <- mean(R, na.rm = T) / sd(R, na.rm = T)


# Daily percent portfolio turnover
changeP <- RESULTS[["P"]] - lag(RESULTS[["P"]], k = -1)
percentTurnover <- 100 * (sum(changeP > 0) / nrow(DATA[["Close"]])) / K
