###############################################################################################
##### treynor ratio
###############################################################################################

#' @name pt.treynor
#' @aliases pt.treynor
#' @title Treynor ratio
#' @description The Treynor ratio is an analog to the sharp ratio, with standard deviation replaced
#' by the asset beta to benchmark.
#' @usage pt.treynor(ar,br,n,rf)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @param n :number of years of asset return, used to calculate annualized return
#' @param rf :risk free rate
#' @examples rtn <- runif(24, -1, 1)
#' brtn <- runif(24,-1,1)
#' pt.treynor(rtn,brtn,2,0.024)

pt.treynor <- function(ar,br,n,rf){

  nu <- pt.annrtn(ar,n) - rf
  de <- pt.beta(ar,br)

  treynor <- nu/de


  return(treynor)
}
