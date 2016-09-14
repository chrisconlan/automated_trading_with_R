DIR <- list()
DIR[["root"]] <- "~/Platform/"
DIR[["data"]] <- "~/Platform/stockdata/"
DIR[["function"]] <- "~/Platform/functions/"
DIR[["load"]] <- "~/Platform/load/"
DIR[["compute"]] <- "~/Platform/compute/"
DIR[["plan"]] <- "~/Platform/plan/"
DIR[["model"]] <- "~/Platform/model/"

CONFIG <- list()

# Windows users should set to FALSE
CONFIG[["isUNIX"]] <- TRUE

# Set to the desired number of multicore
# processes. Windows users need to be conscious
# of memory requirements of these processes.
CONFIG[["workers"]] <- 4

# Max assets to be held in simulation, optimization,
# and potentially trade execution.
CONFIG[["maxAssets"]] <- 10

# Max iterations in optimization function
# for MODEL job. All users need to be conscious of
# time constraints.
CONFIG[["maxIter"]] <- 100

# Range or scalar value of years
# to train strategy on for MODEL job
CONFIG[["y"]] <- 2016

CONFIG[["minVal"]] <- c(n1 = 1, nFact = 1, nSharpe = 1, shThresh = .01)
CONFIG[["maxVal"]] <- c(n1 = 150, nFact = 5, nSharpe = 200, shThresh = .99)

CONFIG[["PARAMnaught"]] <- c(n1 = -2, nFact = -2, nSharpe = -2, shThresh = 0)  


setwd(DIR[["root"]])

