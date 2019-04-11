###############################################################################################
##### information ratio
###############################################################################################

#' @name pt.info
#' @aliases pt.info
#' @title Information ratio
#' @description The information ratio of asset's returns versus benchmark returns, is the
#' quotient of the annualized excess return and the annualized standard deviation of the excess
#' return.
#' @usage pt.info(ar,br,n)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @param n :number of years
#' @examples brtn <- runif(100, -1, 1)
#' artn <- runif(100, 0, 1)
#' pt.info(artn,brtn,100)

pt.info <- function(ar,br,n){

  er <- ar - br

  aart <- pt.annrtn(ar,n)

  abrt <- pt.annrtn(br,n)

  info <- (aart - abrt)/sd(er)

  return(info)
}
