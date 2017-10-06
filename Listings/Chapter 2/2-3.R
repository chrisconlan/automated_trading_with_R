# Up-to-date at time of writing (May 2016)
url <- "http://trading.chrisconlan.com/SPstocks.csv"
S <- as.character(read.csv(url, header = FALSE)[,1])

# save S.R with stock list from URL
setwd(rootdir)
dump(list = "S", "S.R")
