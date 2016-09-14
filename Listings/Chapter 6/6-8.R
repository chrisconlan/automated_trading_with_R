library(foreach)

# Returns a list
foreach( i = 1:4 ) %dopar% {
  j <- i + 1
  sqrt(j)
}


# Returns a vector
foreach( i = 1:4, .combine = c ) %dopar% {
  j <- i + 1
  sqrt(j)
} 


# Returns a matrix
foreach( i = 1:4, .combine = rbind ) %dopar% {
  j <- i + 1
  matrix(c(i, j, sqrt(j)), nrow = 1) 
} 


# Returns a data frame
foreach( i = 1:4, .combine = rbind ) %dopar% {
  j <- i + 1
  data.frame( i = i, j = j, sqrt.j = sqrt(j))
} 
