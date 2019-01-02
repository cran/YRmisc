##################################################################################################
### Plotting Function
##################################################################################################


## dependent is the column name of the dependent variable
#' @name pl.sgg
#' @aliases pl.sgg
#' @title Plot scatter plots for a data frame using ggplot2
#' @description Plotting scatter plots for a data frame using ggplot2, with titles and label numbers. The output will be 4 graphs per page.
#' @usage pl.sgg(x,dependent,l)
#' @param x :a data frame, which includes the dependent variable
#' @param dependent :the dependent variable for scatter plot
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.sgg(mtcars,"mpg",1)

pl.sgg <- function(x,dependent,l = 1){

  var <- names(x)
  n <- length(var)

  plots <- list()

  for(i in 1:n){

    plots[[i]] <- ggplot(data = x, aes_string(x = var[i], y = dependent)) +
      geom_point() +
      ggtitle(paste("Fig.",as.character(i+l-1), "Scatterplt of", var[i], "v.", dependent)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = dependent, x = var[i])

  }

marrangeGrob(plots, nrow = 2, ncol = 2)


}
