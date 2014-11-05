f_epsgcode <- function(x){
  i.pstrg <- proj4string(x)
  i.epsg <- substr(i.pstrg,
                   (gregexpr(pattern =':',i.pstrg)[[1]][1] + 1),
                   (gregexpr(pattern =' ',i.pstrg)[[1]][1] - 1))
  return(i.epsg)
  
}