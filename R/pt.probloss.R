###############################################################################################
##### probability of loss
###############################################################################################

#' @name pt.probloss
#' @aliases pt.probloss
#' @title Probability of loss
#' @description This function give the probability of loss of given asset returns.
#' @usage pt.probloss(r,p)
#' @param r :a vector of periodic returns
#' @param p :target return
#' @examples rt <- runif(12,-1,1) # generate random number to simulate returns
#' pt.probloss(rt,0)
#' pt.probloss(rt,0.05)

pt.probloss <- function(r,p){

  n <- length(r)

  down <- length(r[r < p])

  return(down/n)

}
