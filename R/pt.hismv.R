###############################################################################################
##### mean-variance model
###############################################################################################

#' @name pt.hismv
#' @aliases pt.hismv
#' @title Mean-variance model with historical average returns and standard deviations
#' @description This function will perform portfolio simulation with historical average returns and standard deviatoins.
#' Mean-variance model, or modern portfolio theory, is a mathmatical framework for
#' accessing a portfolio. It uses the variance of asset returns as a risk proxy. This function will
#' return a number of simulated portfolio with different weights.
#' @usage pt.hismv(r,n,mini)
#' @param r :a data frame of asset returns
#' @param n :number of portfolio simulated
#' @param mini :minimal weight; choose 0 if long only; choose 1 for possible short position
#' @examples set.seed(20)
#' rtn <- data.frame(runif(120,-1,1),runif(120,-1,1),runif(120,-1,1),runif(120,-1,1))
#' names(rtn) <- c("asset1","asset2","asset3","asset4")
#' portfolio <- pt.hismv(rtn,1000,0)
#' plot(portfolio[,6], portfolio[,5], xlab = "standart deviation", ylab = "expected return")


pt.hismv <- function(r,n,mini){

  m <- dim(r)[2]

  # average returns
  returns <- apply(r, 2, mean)
  # covariance matrix
  covm <- cov(r)

  port <- as.data.frame(matrix(nrow = n, ncol = m + 2))

  namesv <- as.character()

  for (i in 1:m) {

    namesv[i] <- paste0("weight",as.character(i))

    namesv[m+1] <- as.character("return")

    namesv[m+2] <- as.character("stdev")

  }

  names(port) <- namesv

  for (i in 1:n) {

    w <- runif(m,-mini,1)

    w <- w/sum(w)

    port[i,1 : m] <- w

    port[i,m + 1] <- sum(w * returns)

    port[i,m + 2] <- t(as.matrix(w)) %*% covm %*% as.matrix(w)
  }

  return(port)

}

