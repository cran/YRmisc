##################################################################################################
### Plotting Function
##################################################################################################


#' @name pl.histgg
#' @aliases pl.histgg
#' @title Plot histograms for a data frame with ggplot2
#' @description Plotting histograms for a data frame with 4 per page, with titles and label numbers automatically generated.
#' @usage pl.histgg(x,l,bin)
#' @param x :a data frame
#' @param l :the beginning label number in the title (default set to 1)
#' @param bin :bin width of histogram (default set to 30)
#' @examples pl.histgg(as.data.frame(EuStockMarkets),1)

pl.histgg <- function(x,l = 1, bin = 30){

  x <- x
  varname <- names(x)
  n <- length(varname)

  plots <- list()
      for(i in 1:n){

      plots[[i]] <- ggplot(data = x, aes_string(varname[i])) +
        geom_histogram(bins = bin) +
        ggtitle(paste("Fig.",as.character(i+l-1), "Histogram of", varname[i])) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = "Frequency", x = varname[i])

      }

 marrangeGrob(plots, nrow = 2, ncol = 2)

}

