###############################################################################################
##### Average up and down returns
###############################################################################################

#' @name pt.udrtn
#' @aliases pt.udrtn
#' @title Average up and down returns
#' @description This function calculates the average up and down returns from a series of returns.
#' @usage pt.udrtn(r)
#' @param r :a vector of periodic returns
#' @examples r <- runif(100,-1,1) # generate random number to simulate returns
#' pt.udrtn(r)

pt.udrtn <- function(r){

  up <- r[r > 0]
  down <- r[r < 0]

  nup <- length(up)
  ndown <- length(down)

  avgup <- sum(up)/nup
  avgdown <- sum(down)/ndown

  case <- c("Up", "Down")
  n <- c(nup,ndown)
  avgrtn <- c(avgup, avgdown)

  returndf <- data.frame(case,n,avgrtn)

  return(returndf)
}
