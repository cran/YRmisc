###############################################################################################
##### standard deviation of excess return
###############################################################################################

#' @name pt.sdexrtn
#' @aliases pt.sdexrtn
#' @title Standard deviation of excess return
#' @description The standard deviation of excess return is simply the standard deviation of the asset
#' return over the benchmark return.
#' @usage pt.sdexrtn(ar,br)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @examples artn <- runif(12, -1, 1)
#' brtn <- runif(12,-1,1)
#' pt.sdexrtn(artn,brtn)

pt.sdexrtn <- function(ar,br){

  sdex <- sd(ar-br)


  return(sdex)
}
