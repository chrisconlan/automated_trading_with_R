setwd(datadir) 
S <- sub(".csv", "", list.files())

require(data.table)

DATA <- list()
for(i in S){
  suppressWarnings(
  DATA[[i]] <- fread(paste0(i, ".csv"), sep = ","))
  DATA[[i]] <- (DATA[[i]])[order(DATA[[i]][["Date"]], decreasing = FALSE)]
} 

