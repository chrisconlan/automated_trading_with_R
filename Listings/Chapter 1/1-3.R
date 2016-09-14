# Use na.rm = TRUE to ignore NA's at position 1 in return series
SR <- mean(Rt, na.rm = TRUE) / sd(Rt, na.rm = TRUE)
SR2 <- mean(Rt2, na.rm = TRUE) / sd(Rt2, na.rm = TRUE) 
SRb <- mean(Rb, na.rm = TRUE) / sd(Rb, na.rm = TRUE)
