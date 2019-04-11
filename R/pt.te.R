###############################################################################################
##### tracking error
###############################################################################################

#' @name pt.te
#' @aliases pt.te
#' @title Tracking error
#' @description Tracking error, in finance, is a measure of risk in a portfolio that is due to active management
#' decisions made by the manager. It indicates how closely the portfolio follows the benchmark of choosing.
#' @usage pt.te(pr,br)
#' @param pr :portfolio return
#' @param br :benchmark return
#' @examples prtn <- runif(12,-1,1)
#' brtn <- runif(12,-1,1)
#' pt.te(prtn,brtn)

pt.te <- function(pr,br){

  te <- sqrt(var(pr - br))

  return(te)
}
