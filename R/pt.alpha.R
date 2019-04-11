###############################################################################################
##### alpha
###############################################################################################

#' @name pt.alpha
#' @aliases pt.alpha
#' @title Stock return alpha
#' @description Alpha is the intercept of a fitted line when dependent variable is the benchmark return
#' and independent variable is a asset return of the same period. It is a measure of the active return
#' on an investment. Alpha, along with beta, is one of the two key coefficients in the CAPM used modern portfolio theory.
#' @usage pt.alpha(ar,br)
#' @param ar :a vector of a risk asset return
#' @param br :a vector of benchmark return
#' @examples brtn <- runif(100, -1, 1)
#' artn <- runif(100, -1, 1)
#' pt.alpha(artn,brtn)

pt.alpha <- function(ar,br){

  dat <- data.frame(br,ar)
  fit <- lm(ar~br, dat, na.action = na.omit)

  alpha <- as.numeric(fit$coefficients[1])

  return(alpha)
}
