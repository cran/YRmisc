##################################################################################################
### Plotting Function
##################################################################################################


#' @name pl.hsdgg
#' @aliases pl.hsdgg
#' @title Plot histograms for a data frame with ggplot2
#' @description Plotting histograms for a data frame with 4 per page, with titles and label numbers automatically generated.
#' @param pl.hsdgg(x,l,bin)
#' @param x :a data frame
#' @param l :the beginning label number in the title (default set to 1)
#' @param bin :bin width of the graph
#' @examples pl.hsdgg(as.data.frame(EuStockMarkets),1,100)

pl.hsdgg <- function(x,l = 1, bin = 30){

  x <- x
  varname <- names(x)
  n <- length(varname)

  ..density.. <- NULL
  plots <- list()
  for(i in 1:n){

    plots[[i]] <- ggplot(x, aes_string(x = varname[i])) +
      geom_histogram(aes(y = ..density..), binwidth = bin, colour = "black") +
      geom_density(alpha = .2) +
      ggtitle(paste("Fig.",as.character(i+l-1), "Histogram of", varname[i])) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Frequency", x = varname[i])

  }

  marrangeGrob(plots, nrow = 2, ncol = 2)



}

