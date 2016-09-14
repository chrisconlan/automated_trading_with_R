if( CONFIG[["isUNIX"]] ){
  library(doMC)
  workers <- CONFIG[["workers"]]
  registerDoMC( cores = workers )
} else {
  library(doParallel)
  workers <- CONFIG[["workers"]]
  registerDoParallel( cores = workers )
}
