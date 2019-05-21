###############################################################################################
##### plot mean-variance simulation result
###############################################################################################

#' @name pl.mv
#' @aliases pl.mv
#' @title Plot mean-variance simulation result
#' @description This function is used to plot the result of portfolio simulation by pt.mv().
#' @usage pl.mv(port)
#' @param port :portfolio simulation result from pt.mv()
#' @examples set.seed(1)
#' rtn <- data.frame(runif(120,-1,1),runif(120,-1,1),runif(120,-1,1),runif(120,-1,1))
#' names(rtn) <- c("asset1","asset2","asset3","asset4")
#' portfolio <- pt.hismv(rtn,1000,0)
#' pl.mv(portfolio)


pl.mv <- function(port){

 ggplot(data = port, aes_string(x = "stdev", "return")) + geom_point() +
    ggtitle("Portfolio simulation") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Returns", x = "Standard Deviation")

}
