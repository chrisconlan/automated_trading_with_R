# Up-to-date at time of writing (May 2016)
url <- "http://trading.chrisconlan.com/SPstocks.csv"
S <- as.character(read.csv(url, header = FALSE)[,1])

# Changing stocks names. Because Quandl reads stocks with "_" instead of "-"
S <- gsub("-", "_", S)

# save S.R with stock list from url
setwd(rootdir)
dump(list = "S", "S.R")
