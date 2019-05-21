###############################################################################################
##### bias ratio
###############################################################################################

#' @name pt.bias
#' @aliases pt.bias
#' @title Bias ratio
#' @description The bias ratio is an indicator used in finance analyze the returns of a portfolio, and
#' in performing due diligence.
#' @usage pt.bias(r)
#' @param r :a vector of a risk asset return
#' @examples r <- runif(100,0,1) # generate random number to simulate returns
#' pt.bias(r)

pt.bias <- function(r){

  mid <- mean(r)
  std <- sd(r)

  up <- length(r[r >= mid & r < mid + std])

  down <- length(r[r < mid & r > mid - std])

  ba <- up/(down + 1)

  return(ba)
}
