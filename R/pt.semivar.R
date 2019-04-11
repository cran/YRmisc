###############################################################################################
##### semivariance of loss
###############################################################################################

#' @name pt.semivar
#' @aliases pt.semivar
#' @title Semivariance of loss
#' @description This function give the semivariance of a losing scenario.
#' @usage pt.semivar(r,p)
#' @param r :a vector of periodic returns
#' @param p :target return
#' @examples rt <- runif(12,-1,1) # generate random number to simulate returns
#' pt.semivar(rt,0)
#' pt.semivar(rt,0.03)

pt.semivar <- function(r,p){

  n <- length(r)

  if (p != 0 ){

    down <- sum((r[r < p] - mean(r))^2)

    re <- down/n

  }else{

    down <- sum((r[r < 0]^2))
    re <- down/n

  }
  return(re)
}
