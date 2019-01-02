###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name pl.3txtgg
#' @aliases pl.3txtgg
#' @title Scatter plot with text overlay with ggplot2
#' @description Generate a scatter plot with text overlay with ggplot2. This plot is to better show the effect of
#' the text variable in the domain of x and y variable.
#' @usage pl.3txtgg(x,y,txt,size,title,xlab,ylab)
#' @param x :a numeric vector
#' @param y :a numeric vector
#' @param txt :a vector used as labels
#' @param title :title of the graph
#' @param size :text size, which default is set as 3
#' @param xlab :x-axis label
#' @param ylab :y-axis label
#' @examples pl.3txtgg(mtcars[,1], mtcars[,3], row.names(mtcars), 3,"mpg v. cyl", "mpg", "cyl")


pl.3txtgg <- function(x, y, txt, size = 3, title, xlab, ylab){

  df <- na.omit(data.frame(x,y,txt))

  ggplot(df, aes(x, y, label = df$txt)) +
    geom_text(size = size) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = ylab, x = xlab)

}
