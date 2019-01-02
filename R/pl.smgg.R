##################################################################################################
### Plotting Function
##################################################################################################


## dependent is the column name of the dependent variable
#' @name pl.smgg
#' @aliases pl.smgg
#' @title Plot scatter plots with smooth line for a data frame using ggplot2
#' @description Plotting scatter plots for a data frame using ggplot2, with titles and label numbers. A smooth line will be added using a chosen method. The output will be 4 graphs per page.
#' @usage pl.smgg(x,dependent,l,mtd)
#' @param x :a data frame, which includes the dependent variable
#' @param dependent :the dependent variable for scatter plot
#' @param l :the beginning label number in the title (default set to 1)
#' @param mtd :sommthing method to use, accepts either a character vector or a function, e.g. MASS::rlm, base::lm, base::loess, mgcv::gam
#' @examples pl.smgg(mtcars,"mpg",1,lm)
#' pl.smgg(mtcars,"mpg",1,loess)


pl.smgg <- function(x,dependent,l = 1, mtd = lm){

  var <- names(x)
  n <- length(var)

  plots <- list()

  for(i in 1:n){

    plots[[i]] <- ggplot(data = x, aes_string(x = var[i], y = dependent)) + geom_point() + geom_smooth(method = mtd) +
      ggtitle(paste("Fig.",as.character(i+l-1), "Scatterplt of", var[i], "v.", dependent)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = dependent, x = var[i])

  }

 marrangeGrob(plots, nrow = 2, ncol = 2)


}
