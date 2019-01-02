

## dependent is the column name of the dependent variable
#' @name pl.2tsgg
#' @aliases pl.2tsgg
#' @title Time series plot for two variables with ggplot2
#' @description Plotting two time series in one plot, with title and label. If both variables are time series object, they will be merged by time.
#' If both variables are not time series object, they will be merged by order. The first variable is set to be a solid line and the second variable
#' is set to be a dashed line. If the variables are of different type a warning message will be given.
#' @usage pl.2tsgg(ts1,ts2,title,ylab)
#' @param ts1 :a time series variable or a numeric variable
#' @param ts2 :a time series variable or a numeric variable
#' @param title :title for the plot
#' @param ylab :y-axis label
#' @examples DAX <- EuStockMarkets[,1]
#' FTSE <- EuStockMarkets[,4]
#' pl.2tsgg(DAX,FTSE, "Times Series Plot of DAX and FTSE", "Index")

pl.2tsgg <- function(ts1, ts2, title, ylab){

  n <- NULL

  if(is.ts(ts1) & is.ts(ts2)){

      tss <- ts.union(ts1,ts2)

      date <- time(tss)
      tss <- as.data.frame(apply(tss[,1:2],2,as.numeric))
      tss$date <- as.numeric(date)

      ggplot(tss, aes(date)) +
        geom_line(aes(y = ts1)) +
        geom_line(aes(y = ts2), linetype = "dashed") +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = ylab)

  }else if(!is.ts(ts1) & !is.ts(ts2)){

     df <- as.data.frame(cbind(ts1,ts2))
      n <- 1:dim(df)[1]
      df$n <- n

      ggplot(df, aes(x = n)) +
        geom_line(aes(y = ts1)) +
        geom_line(aes(y = ts2), linetype = "dashed") +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = ylab)

  }else{

      cat("Data can't be plotted due to difference in type. Please make sure both variables are of the same type")

  }


}



