# Create linearized equity curve and run regression
y <- Et / Vt
model <- lm(y ~ t)

# Compute PPS by pulling "r.squared" value from summary function
PPS <- ((Et[length(Et)] - Vt[1]) / Vt[1]) * summary(model)$r.squared
