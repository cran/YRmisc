###############################################################################################
##### expected loss
###############################################################################################

#' @name pt.exploss
#' @aliases pt.exploss
#' @title Expected loss
#' @description This function give the expected loss of given asset returns.
#' @usage pt.exploss(r,p)
#' @param r :a vector of periodic returns
#' @param p :target return
#' @examples rt <- runif(12,-1,1) # generate random number to simulate returns
#' pt.exploss(rt,0)
#' pt.exploss(rt,1)

pt.exploss <- function(r,p){

  n <- length(r)

  down <- sum(r[r < p])

  return(down/n)

}
