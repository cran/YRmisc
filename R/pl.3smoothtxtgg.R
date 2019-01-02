###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name pl.3smoothtxtgg
#' @aliases pl.3smoothtxtgg
#' @title Scatter smooth plot with text overlay using ggplot2
#' @description Generate a scatter plot with text overlay, with a smooth curve fitted by loess.
#' @usage pl.3smoothtxtgg(x,y,txt,size,title,xlab,ylab)
#' @param x :a numeric vector
#' @param y :a numeric vector
#' @param txt :a vector used as labels
#' @param size :text size, which default is set as 3
#' @param title :graph title
#' @param xlab :x-axis label
#' @param ylab :y-axis label
#' @examples pl.3smoothtxtgg(mtcars[,1], mtcars[,3], row.names(mtcars), 3, "MPG v. DISP","mpg","disp")

pl.3smoothtxtgg <- function(x, y, txt, size = 3, title, xlab, ylab){

  df <- na.omit(data.frame(x,y,txt))

  ggplot(df, aes(x, y, label = df$txt)) + 
    geom_text(size = size) + 
    geom_smooth(method = loess) + 
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(y = ylab, x = xlab)
  
}


