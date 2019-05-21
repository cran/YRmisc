###############################################################################################
##### Dual-beta
###############################################################################################

#' @name pt.dbeta
#' @aliases pt.dbeta
#' @title Dual-beta
#' @description Dual-beta method is to divide market beta into downside beta and upside beta. The principle
#' behind is that upside and downside betas are not the same.
#' @usage pt.dbeta(ar,mr,rf)
#' @param ar :a vector of a risk asset return
#' @param mr :a vector of market return
#' @param rf :risk free rate
#' @examples artn <- runif(24,0,1) # generate random number to simulate returns
#' mrtn <- runif(24,-1,1)
#' pt.dbeta(artn,mrtn,0.024)

pt.dbeta <- function(ar,mr,rf){


  df <- data.frame(ar, mr)

  dfup <- df[df$mr > 0, ]

  dfdown <- df[df$mr <= 0, ]

  upfit <- lm(ar~mr, dfup, na.action = na.omit)

  downfit <- lm(ar~mr, dfdown, na.action = na.omit)

  upalpha <- upfit$coefficients[1]
  upbeta <- upfit$coefficients[2]

  downalpha <- downfit$coefficients[1]
  downbeta <- downfit$coefficients[2]

  out <- data.frame(upbeta,downbeta)

  row.names(out) <- "Dual-beta"

  return(out)
}
