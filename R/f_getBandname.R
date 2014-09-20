substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrBand <- function(x){
  as.numeric(substr(x, nchar(x)-4, nchar(x)-4)) 
}
