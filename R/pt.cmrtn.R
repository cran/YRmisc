###############################################################################################
##### cumulative return
###############################################################################################

#' @name pt.cmrtn
#' @aliases pt.cmrtn
#' @title Cumulative return
#' @description Cumulative return is the compounded return in a given period.
#' @usage pt.cmrtn(r)
#' @param r :a vector of periodic returns
#' @examples rt <- runif(12,-1,1) # generate random number to simulate returns
#' pt.cmrtn(rt)

pt.cmrtn <- function(r){

  rr <- prod(1+r) - 1

  return(rr)
}
