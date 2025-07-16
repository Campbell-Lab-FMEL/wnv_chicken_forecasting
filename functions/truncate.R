
truncate <- function(obj, lower, upper){
  if(missing(upper)){
    obj[obj > stats::quantile(obj, lower, na.rm = T)] <- stats::quantile(obj, lower, na.rm = T)
  }
  if(missing(lower)){
    obj[obj > stats::quantile(obj, upper, na.rm = T)] <- stats::quantile(obj, upper, na.rm = T)
  } 
  else(
    obj[obj > stats::quantile(obj, c(lower, upper, na.rm = T))] <- stats::quantile(obj, c(lower, upper, na.rm = T))
  )
  return(obj)
}
