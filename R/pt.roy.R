###############################################################################################
##### Roy's safety-first criterion
###############################################################################################

#' @name pt.roy
#' @aliases pt.roy
#' @title Roy's safety-first criterion
#' @description Roy's safety-first criterion is a risk management technique that allows to choose
#' a portfolio based on the criterion that the probability of the portfolio's return falling below
#' a minimum desired threshold is minimized.
#' @usage pt.roy(r,mar)
#' @param r :a vector of a risk asset return
#' @param mar :minimum acceptable return
#' @examples r <- runif(100,0,1) # generate random number to simulate returns
#' pt.roy(r,0.024)

pt.roy <- function(r,mar){

  roy <- (mean(r) - mar) / sd(r)

  return(roy)
}
