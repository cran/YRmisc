###############################################################################################
##### Annualized return
###############################################################################################

#' @name pt.annrtn
#' @aliases pt.annrtn
#' @title Annualized return
#' @description This function takes a series of annual returns and calculate the annualized return.
#' @usage pt.annrtn(r,n)
#' @param r :annual returns
#' @param n :number of years
#' @examples r <- runif(100,-1,1) # generate random number to simulate returns
#' annualizedreturn <- pt.annrtn(r,100)

pt.annrtn <- function(r,n){

  po <- 1/n

  art <- prod(1+r)^po - 1

  return(art)
}
