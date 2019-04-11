###############################################################################################
##### sharp ratio
###############################################################################################

#' @name pt.sharp
#' @aliases pt.sharp
#' @title Sharp ratio
#' @description The Sharpe Ratio of an asset return is the quotient of the annualized excess
#' return of the asset minus the annualized risk-free rate over the annualized standard deviation of the
#' asset return.
#' @usage pt.sharp(r,n,m,rf)
#' @param r :a vector of asset returns
#' @param n :number of years
#' @param m :number of periods in a year; m = 12 if r is monthly returns
#' @param rf :annulized risk-free rate
#' @examples set.seed(20)
#' rtn <- runif(12,-0.5,1) # generate random number to simulate monthly returns
#' rfr <- 0.024 # set risk free rate at 2.4% annual
#' pt.sharp(rtn,1,12,rfr) # the return is for one year

pt.sharp <- function(r,n,m,rf){

  sharp <- (pt.annrtn(r,n) - rf)/pt.annsd(r,m)

  return(sharp)

}

