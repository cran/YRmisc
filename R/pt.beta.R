###############################################################################################
##### beta
###############################################################################################

#' @name pt.beta
#' @aliases pt.beta
#' @title Stock return beta
#' @description Beta is the slope of a fitted line when dependent variable is the benchmark return
#' and independent variable is an asset return of the same period. It is a measure the risk arising
#' from exposure to general market movements.
#' @usage pt.beta(ar,br)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @examples brtn <- runif(100, -1, 1)
#' artn <- runif(100, -1, 1)
#' pt.beta(artn, brtn)

pt.beta <- function(ar,br){

  dat <- data.frame(br,ar)
  fit <- lm(ar~br, dat, na.action = na.omit)

  beta <- as.numeric(fit$coefficients[2])

  return(beta)
}
