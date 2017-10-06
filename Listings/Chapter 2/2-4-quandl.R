# Load "invalid.R" file if available
invalid <- character(0)
setwd(rootdir)
if("invalid.R" %in% list.files()) source("invalid.R")


# Find all symbols not in directory and not missing
setwd(datadir)
toload <- setdiff(S[!paste0(S, ".csv") %in% list.files()], invalid)

# Fetch symbols with quandl function (instead of yahoo), save as .csv or missing
source(paste0(functiondir, "quandl.R"))
if(length(toload) != 0){
  for(i in 1:length(toload)){

  df <- quandl_get(toload[i])

  if(!is.null(df)) {
    write.csv(df[nrow(df):1], file = paste0(toload[i], ".csv"),
              row.names = FALSE)
  } else {
    invalid <- c(invalid, toload[i])
  }

}
}

setwd(rootdir)
dump(list = c("invalid"), "invalid.R")
