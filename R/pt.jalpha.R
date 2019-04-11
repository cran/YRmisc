###############################################################################################
##### jensen's alpha
###############################################################################################

#' @name pt.jalpha
#' @aliases pt.jalpha
#' @title Jensen's alpha
#' @description Jensen's alpha is a financial statistic used to quantify the abnormal return of
#' a security or portfolio over the theoretical expected return. Unlike, standard alpha, it uses
#' theoretical performance return instead of a market return.
#' @usage pt.jalpha(pr,mr,rf,beta)
#' @param pr :portfolio return
#' @param mr :market return
#' @param rf :risk free rate
#' @param beta :portfolio beta
#' @examples prtn <- runif(24, -1, 1)
#' mrtn <- runif(24, -1, 1)
#' rf <- 0.024
#' pt.jalpha(mean(prtn), mean(mrtn), rf, pt.beta(prtn,mrtn))
#'

pt.jalpha <- function(pr,mr,rf,beta){

  alpha <- pr - (rf - beta * (mr - rf))

  return(alpha)
}
