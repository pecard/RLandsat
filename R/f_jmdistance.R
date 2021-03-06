#' Jeffries-Matusita distance - JM Separability Index
#'# Follows the contribution of jlhoward in SO
#'# http://stackoverflow.com/a/24763982/640783
#'# with the function from Richards and Jia 2006
#' Incomplete
f_jmdist <- function (sig1 , sig2) {
  # this function was adapted from: 
  # http://stats.stackexchange.com/questions/78849/measure-for-separability
  Matrix.1 <- as.matrix (sig1)
  Matrix.2 <- as.matrix (sig2)
  mean.Matrix.1 <- mean ( Matrix.1 )
  mean.Matrix.2 <- mean ( Matrix.2 )
  mean.difference <- mean.Matrix.1 - mean.Matrix.2
  cv.Matrix.1 <- cov ( Matrix.1 )
  cv.Matrix.2 <- cov ( Matrix.2 )
  p <- ( cv.Matrix.1 + cv.Matrix.2 ) / 2
  # calculate the Bhattacharryya index
  bh.distance <- 0.125 *t ( mean.difference ) * p^ ( -1 ) * mean.difference +
    0.5 * log (det ( p ) / sqrt (det ( cv.Matrix.1 ) * det ( cv.Matrix.2 )))
  # calculate Jeffries-Matusita
  # following formula is bound between 0 and 2.0
  jm.distance <- 2 * ( 1 - exp ( -bh.distance ) )
  # also found in the bibliography:
  # jm.distance <- 1000 * sqrt (   2 * ( 1 - exp ( -bh.distance ) )   )
  # the latter formula is bound between 0 and 1414.0
  return(jm.distance)
}

dsigs <- reshape2::dcast(qsigs, class+newcl ~ band, value.var = 'value')[ ,-2]

dist(df, method=jm.dist,by_rows=FALSE)