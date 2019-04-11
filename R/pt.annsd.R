###############################################################################################
##### annualized standard deviation
###############################################################################################

#' @name pt.annsd
#' @aliases pt.annsd
#' @title Annualized standard deviation
#' @description The annualized standard deviation is the standard deviation multiplied by the
#' square root of the number of periods in one year.
#' @usage pt.annsd(r,n)
#' @param r :a vector of a risk asset return
#' @param n :number of periods in a year
#' @examples rtn <- runif(30, -1, 1)
#' n <- 30
#' pt.annsd(rtn,n)

pt.annsd <- function(r,n){

  s <- sd(r)

  asd <- s * sqrt(n)

  return(asd)
}
