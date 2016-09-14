library(lattice)
wireframe(obj ~ n1*shThresh, data = OPTIM,
          xlab = "n1", ylab = "shThresh",
          main = "Long-Only MACD Exhaustive Optimization",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = 15, x = -60)
)

levelplot(obj ~ n1*shThresh, data = OPTIM,
          xlab = "n1", ylab = "shThresh",
          main = "Long-Only MACD Exhaustive Optimization"
)
