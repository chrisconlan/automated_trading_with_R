# Listing 2.2
yahoo <- function(sym, current = TRUE,
                  a = 0, b = 1, c = 2000, d, e, f,
                  g = "d")
{

  if(current){
    f <- as.numeric(substr(as.character(Sys.time()), start = 1, stop = 4))
    d <- as.numeric(substr(as.character(Sys.time()), start = 6, stop = 7)) - 1
    e <- as.numeric(substr(as.character(Sys.time()), start = 9, stop = 10))
  }
  
  require(data.table)

  tryCatch(
  suppressWarnings(
  fread(paste0("http://ichart.yahoo.com/table.csv",
               "?s=", sym,
               "&a=", a,
               "&b=", b,
               "&c=", c,
               "&d=", d,
               "&e=", e,
               "&f=", f,
               "&g=", g,
               "&ignore=.csv"), sep = ",")),
  error = function(e) NULL
  )
}

