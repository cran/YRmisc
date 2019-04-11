###############################################################################################
##### annualized excess return
###############################################################################################

#' @name pt.annexrtn
#' @aliases pt.annexrtn
#' @title Annualized excess return
#' @description Annualized excess return is the difference between the annualized and cumulative
#' return of the two series. Usually, one series are portfolio returns and the other is a benchmark
#' returns.
#' @usage pt.annexrtn(ar,br)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @examples artn <- runif(100, -1, 1)
#' brtn <- runif(100, -1, 1)
#' pt.annexrtn(artn, brtn)

pt.annexrtn <- function(ar,br){

  n <- 1/length(ar)

  assetreturn <- prod(1 + ar)^n-1
  benckmarkreturn <- prod(1 + br)^n-1

  exrtn <- assetreturn - benckmarkreturn

  return(exrtn)
}
