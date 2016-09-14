# Scatterplot of Rt against Rb 
plot(y = Rt, x = Rb,
     pch = 20,
     cex = 0.5,
     xlab = "SPY Returns",
     ylab= "Return Series 1",
     main = "Figure 1.7: Return Series 1 vs. SPY")
grid() 
abline(h = 0)
abline(v = 0)

# Compute and store the regression model
model <- lm(Rt ~ Rb)

# Plot the regression line
abline(model, col = 2)

# Display alpha and beta
legend(x = "topleft", col = c(0,2), lwd = 2,
       legend = c("Alpha   Beta   R^2",
                  paste0(round(model$coefficients[1], 4), "   ",
                         round(model$coefficients[2], 2), "   ",
                         round(summary(model)$r.squared, 2))))
