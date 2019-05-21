###############################################################################################
##### Dual-alpha
###############################################################################################

#' @name pt.dalpha
#' @aliases pt.dalpha
#' @title Dual-alpha
#' @description Dual-alpha method is to divide market alpha into downside beta and upside alpha. The principle
#' behind is that upside and downside alphas are not the same.
#' @usage pt.dalpha(ar,mr,rf)
#' @param ar :a vector of a risk asset return
#' @param mr :a vector of market return
#' @param rf :risk free rate
#' @examples artn <- runif(24,0,1) # generate random number to simulate returns
#' mrtn <- runif(24,-1,1)
#' pt.dalpha(artn,mrtn,0.024)

pt.dalpha <- function(ar,mr,rf){


  df <- data.frame(ar, mr)

  dfup <- df[df$mr > 0, ]

  dfdown <- df[df$mr <= 0, ]

  upfit <- lm(ar~mr, dfup, na.action = na.omit)

  downfit <- lm(ar~mr, dfdown, na.action = na.omit)

  upalpha <- upfit$coefficients[1]
  upbeta <- upfit$coefficients[2]

  downalpha <- downfit$coefficients[1]
  downbeta <- downfit$coefficients[2]

  out <- data.frame(upalpha,downalpha)

  row.names(out) <- "Dual-alpha"

  return(out)
}
