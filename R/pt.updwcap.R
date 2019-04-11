###############################################################################################
##### up and down capture
###############################################################################################

#' @name pt.updwcap
#' @aliases pt.updwcap
#' @title Up and down capture
#' @description The up and down capture is a measure of how an asset was able to improve on benchmark returns
#' or how it underperforms over the benchmark.
#' @usage pt.updwcap(ar,br,n)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @param n :number of years of asset return, used to calculate annualized return
#' @examples artn <- runif(12, -1, 1)
#' brtn <- runif(12,-1,1)
#' pt.updwcap(artn,brtn,1)

pt.updwcap <- function(ar,br,n){

  returns <-data.frame(ar,br)

  upreturns <- returns[returns$br > 0, ]
  downreturns <- returns[returns$br < 0, ]


  upcap <- pt.annrtn(upreturns$ar,n)/pt.annrtn(upreturns$br,n)
  downcap <- pt.annrtn(downreturns$ar,n)/pt.annrtn(downreturns$br,n)

  cap <- data.frame(upcap,downcap)
  return(cap)
}
