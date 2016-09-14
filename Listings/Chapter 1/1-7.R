PM <- function(Rt, upper = FALSE, n = 2, Rb = 0){
  if(n != 0){
    if(!upper) return(mean(pmax(Rb - Rt, 0, na.rm = TRUE)^n))
    if(upper) return(mean(pmax(Rt - Rb, 0, na.rm = TRUE)^n))
  } else {
    if(!upper) return(mean(Rb >= Rt))
    if(upper) return(mean(Rt > Rb))
  }
}
