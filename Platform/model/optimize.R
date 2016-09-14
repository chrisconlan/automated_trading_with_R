setwd(DIR[["model"]])

minVal <- CONFIG[["minVal"]]
maxVal <- CONFIG[["maxVal"]]
PARAM <- CONFIG[["PARAMnaught"]]

source("evaluateFunc.R")
source("optimizeFunc.R")

PARAMout <- optimize(y = CONFIG[["y"]], minVal, maxVal)

setwd(DIR[["plan"]])

write.csv(data.frame(PARAMout), "stratParams.csv")
