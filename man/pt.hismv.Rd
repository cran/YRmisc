% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt.hismv.R
\name{pt.hismv}
\alias{pt.hismv}
\title{Mean-variance model with historical average returns and standard deviations}
\usage{
pt.hismv(r,n,mini)
}
\arguments{
\item{r}{:a data frame of asset returns}

\item{n}{:number of portfolio simulated}

\item{mini}{:minimal weight; choose 0 if long only; choose 1 for possible short position}
}
\description{
This function will perform portfolio simulation with historical average returns and standard deviatoins.
Mean-variance model, or modern portfolio theory, is a mathmatical framework for
accessing a portfolio. It uses the variance of asset returns as a risk proxy. This function will
return a number of simulated portfolio with different weights.
}
\examples{
set.seed(20)
rtn <- data.frame(runif(120,-1,1),runif(120,-1,1),runif(120,-1,1),runif(120,-1,1))
names(rtn) <- c("asset1","asset2","asset3","asset4")
portfolio <- pt.hismv(rtn,1000,0)
plot(portfolio[,6], portfolio[,5], xlab = "standart deviation", ylab = "expected return")
}
