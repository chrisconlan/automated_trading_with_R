plot(y = Et, x = t, type = "l", col = 1,
     xlab = "",
     ylab= "Equity ($)",
     main = "Figure 1.4: Sharpe Ratios")
grid()  
abline(h = 10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2,
       legend = c(paste0("SR = ", round(SR, 3)),
                  paste0("SR = ", round(SR2, 3)),
                  paste0("SR = ", round(SRb, 3))))

