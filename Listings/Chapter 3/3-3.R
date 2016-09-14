# Forward replacement function
forwardfun <- function(v, n) {
  if(is.na(v[n])){
    return(v[max(which(!is.na(v)))])
  } else {
    return(v[n])
  }
}

maxconsec <- 3

# We pass maxconsec to rollapply() in "width = "
# and pass it again to forwardfun() in "n = "
forwardrep <- rollapply(temp,
           width = maxconsec,
           FUN = forwardfun,
           n = maxconsec,
           by.column = TRUE,
           align = "right") 
