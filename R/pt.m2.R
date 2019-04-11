###############################################################################################
##### modigliani risk-adjusted performance
###############################################################################################

#' @name pt.m2
#' @aliases pt.m2
#' @title Modigliani risk-adjusted performance
#' @description Modigliani risk-adjusted performance is a financial measure of risk-adjusted returns
#' of a portfolio. It measures the returns of the portfolio after adjusting it relative to some benchmark.
#' @usage pt.m2(pr,br,rf)
#' @param pr :portfolio return
#' @param br :benchmark return
#' @param rf :risk free rate
#' @examples prtn <- runif(12,-1,1)
#' brtn <- runif(12,-1,1)
#' rf <- 0.024
#' pt.m2(prtn,brtn,rf)

pt.m2 <- function(pr,br,rf){

  d <- pr - rf

  sharp <- mean(d) / sd(d)


  m2 <- sharp * sd(br) * mean(rf)

  return(m2)
}
