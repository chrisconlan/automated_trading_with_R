timeLapse <- proc.time()[3]
for( i in 1:1000000) v <- runif(1)
proc.time()[3] - timeLapse
