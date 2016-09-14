# Declare 10mil random numbers in a data frame
df <- data.frame(matrix(nrow = 10000, ncol = 1000, runif(n = 10000 * 1000)))


# Compute the sum of each row with a for loop
# Completes in 96.692 seconds
v1 <- rep(NA, 10000)
for( i in 1:10000 ) {
  v1[i] <- sum(df[i,])
}


# Use rowSums() binary
# Completes in 0.053 seconds
v2 <- rowSums(df)


# Results are exactly the same
# Expression evaluates to TRUE
all.equal(v1, v2)
