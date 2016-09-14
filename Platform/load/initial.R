# Listing 2.4
setwd(DIR[["function"]])
source("yahoo.R")

setwd(DIR[["root"]])
if("S.R" %in% list.files()) {
  source("S.R")
} else {
  url <- "http://trading.chrisconlan.com/SPstocks.csv"
  S <- as.character(read.csv(url, header = FALSE)[,1])
  dump(list = "S", "S.R")
}

invalid <- character(0)
if("invalid.R" %in% list.files()) source("invalid.R")

setwd(DIR[["data"]])
toload <- setdiff(S[!paste0(S, ".csv") %in% list.files()], invalid)

if(length(toload) != 0){
  for(i in 1:length(toload)){
    
  df <- yahoo(toload[i])
  
  if(!is.null(df)) {
    write.csv(df[nrow(df):1], file = paste0(toload[i], ".csv"),
              row.names = FALSE) 
  } else {
    invalid <- c(invalid, toload[i])
  }
    
}
}

setwd(DIR[["root"]])
dump(list = c("invalid"), "invalid.R")

rm(list = setdiff(ls(), c("CONFIG", "DIR", "yahoo")))
gc()
