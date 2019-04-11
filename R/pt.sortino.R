###############################################################################################
##### sortino ratio
###############################################################################################

#' @name pt.sortino
#' @aliases pt.sortino
#' @title Sortino ratio
#' @description The Sortino ratio is an analog to the sharp ratio, with standard deviation replaced
#' by the downside deviation.
#' @usage pt.sortino(r,p,n,rf)
#' @param r :a vector of a risk asset return
#' @param p :target return, aka minimum acceptable return(MAR)
#' @param n :number of years of asset return, used to calculate annualized return
#' @param rf :risk free rate
#' @examples rtn <- runif(12, -1, 1)
#' pt.sortino(rtn,0.3,1,0.024)

pt.sortino <- function(r,p,n,rf){

  nu <- pt.annrtn(r,n) - rf
  de <- pt.semivar(r,p)

  sortino <- nu/de


  return(sortino)
}
