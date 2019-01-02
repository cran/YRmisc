##################################################################################################
### Plotting Function
##################################################################################################


#' @name pl.tsgg
#' @aliases pl.tsgg
#' @title Plot times series plot for a data frame with ggplot2
#' @description Plotting time series plot for a data frame with 4 per page, with titles and label numbers automatically generated.
#' @usage pl.tsgg(x,l)
#' @param x :a data frame
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.tsgg(as.data.frame(EuStockMarkets),1)


pl.tsgg <- function(x,l = 1){

  varname <- names(x)
  n <- length(varname)
  x$obs <- 1:dim(x)[1]

  plots <- list()

  for(i in 1:n){

    plots[[i]] <- ggplot(data = x, aes_string(x = "obs", y = varname[i])) +
      geom_line() +
      ggtitle(paste("Fig.",as.character(i+l-1), "Time Series plot of", varname[i])) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = varname[i], x = "Time")

  }

marrangeGrob(plots, nrow = 2, ncol = 2)

}

