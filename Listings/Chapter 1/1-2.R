# Set Random Seed
set.seed(123)

# Create Time Index
t <- 1:(length(SPY)-1)
 
# Tradable Capital Vector
Vt <- c(rep(10000, length(t)))

# Benchmark Return Series
Rb <- rep(NA, length(t))
for(i in 2:length(t)) { Rb[i] <- (SPY[i] / SPY[i - 1]) - 1 }

# Benchmark Equity Curve
Eb <- rep(NA, length(t))
Eb[1] <- Vt[1]
for(i in 2:length(t)) { Eb[i] <- Eb[i-1] * (1 + Rb[i]) }

# Randomly Simulated Return Series 1
Rt <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt[i] <- Rb[i] + rnorm(n = 1,
                         mean = 0.24/length(t),
                         sd = 2.5 * sd(Rb, na.rm = TRUE))
} 

# Randomly Simulated Return Series 2
Rt2 <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt2[i] <- Rb[i] + rnorm(n = 1,
                         mean = 0.02/length(t),
                         sd = .75 * sd(Rb, na.rm = TRUE))
} 

# Randomly Simulated Equity Curve 1
Et <- rep(NA, length(t))
Et <- Vt[1]
for(i in 2:length(t)) { Et[i] <- Et[i-1] * (1 + Rt[i]) }

# Randomly Simulated Equity Curve 2
Et2 <- rep(NA, length(t))
Et2 <- Vt[1]
for(i in 2:length(t)) { Et2[i] <- Et2[i-1] * (1 + Rt2[i]) }

# Plot of Et1 against the SPY Portfolio
plot(y = Et, x = t, type = "l", col = 1,
     xlab = "Time",
     ylab= "Equity ($)",
     main = "Figure 1.3: Randomly Generated Equity Curves")
grid()
abline(h = 10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2, legend = c("Curve 1",
                                                          "Curve 2",
                                                          "SPY"))
  
