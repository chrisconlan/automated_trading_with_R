source("~/Platform/config.R")


setwd(DIR[["root"]])
cat("load.R\n\n")
source("load.R")


setwd(DIR[["compute"]])
cat("MCinit.R\n\n")
source("MCinit.R")

cat("functions.R\n\n")
source("functions.R")

setwd(DIR[["plan"]])
cat("decisionGen.R\n\n")
source("decisionGen.R")


cat("\n")
