###############################################################################################
##### batting average
###############################################################################################

#' @name pt.btavg
#' @aliases pt.btavg
#' @title Batting average
#' @description The batting average of the asset is the ratio between the number of periods
#' where the asset outperforms a benchmark and the total number of periods.
#' @usage pt.btavg(ar,br)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of a benchmark return
#' @examples artn <- runif(100,-1,1)
#' brtn <- runif(100,-1,1)
#' pt.btavg(artn,brtn)

pt.btavg <- function(ar,br){

  n <- length(ar)

  dirt <- ar - br

  outperform <- length(dirt[dirt > 0])

  bta <- outperform/n

  return(bta)

}
