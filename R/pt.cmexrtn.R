###############################################################################################
##### cumulative excess return
###############################################################################################

#' @name pt.cmexrtn
#' @aliases pt.cmexrtn
#' @title Cumulative excess return
#' @description Cumulative return is the compounded return in a given period. The excess return is the
#' difference between the cumulative return of a risky asset and the cumulative return of a benchmark.
#' @usage pt.cmexrtn(ar,br)
#' @param ar :a vector of risky asset returns
#' @param br :a vector of benchmark returns
#' @examples brtn <- runif(12, -1, 1)
#' artn <- runif(12, -1, 1)
#' pt.cmexrtn(artn,brtn)

pt.cmexrtn <- function(ar,br){

  art <- prod(1 + ar) - 1
  brt <- prod(1 + br) - 1

  return(art - brt)
}
