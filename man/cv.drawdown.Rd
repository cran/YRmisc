% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.drawdown.R
\name{cv.drawdown}
\alias{cv.drawdown}
\title{Largest draw down of returns}
\usage{
cv.drawdown(x)
}
\arguments{
\item{x}{: a numeric vector of returns}
}
\description{
Calculate largest draw down of a series of returns. This function calculates the maximum
decrease in percentage over time, which can be used to test portfolio returns.
}
\examples{
# rnorm() is used to simulate portfolio returns
returns <- rnorm(100)
cv.drawdown(returns)
}
