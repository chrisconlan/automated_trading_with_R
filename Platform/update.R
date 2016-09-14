source("~/Platform/config.R")

setwd(DIR[["load"]])
cat("initial.R\n\n")
source("initial.R")

setwd(DIR[["load"]])
cat("loadToMemory.R\n\n")
source("loadToMemory.R")

setwd(DIR[["load"]])
cat("updateStocks.R\n\n")
source("updateStocks.R")

cat("\n")
