currentTime <- Sys.time()

for(i in S){
  # Store greatest date within DATA for symbol i
  maxdate <- DATA[[i]][["Date"]][nrow(DATA[[i]])]
  if(as.numeric(difftime(currentTime, maxdate, units = "hours")) >= 40.25){
    
    # Push the maxdate forward one day
    maxdate <- strptime(maxdate, "%Y-%m-%d") + 86400
    
    weekend <- sum(c("Saturday", "Sunday") %in% 
                     weekdays(c(maxdate, currentTime))) == 2
    
    span <- FALSE
    if( weekend ){ 
      span <- as.numeric(difftime(currentTime, maxdate, units = "hours")) >= 48
    }
    
    if(!weekend & !span){
      c <- as.numeric(substr(maxdate, start = 1, stop = 4))
      a <- as.numeric(substr(maxdate, start = 6, stop = 7)) - 1
      b <- as.numeric(substr(maxdate, start = 9, stop = 10))
      df <- yahoo(i, a = a, b = b, c = c)
      if(!is.null(df)){
        if(all(!is.na(df)) & nrow(df) > 0){
          df <- df[nrow(df):1]
          write.table(df, file = paste0(i, ".csv"), sep = ",",
                        row.names = FALSE, col.names = FALSE, append = TRUE) 
          DATA[[i]] <- rbind(DATA[[i]], df)
        }
      } 
    }
  }
}
