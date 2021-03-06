% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt.cmexrtn.R
\name{pt.cmexrtn}
\alias{pt.cmexrtn}
\title{Cumulative excess return}
\usage{
pt.cmexrtn(ar,br)
}
\arguments{
\item{ar}{:a vector of risky asset returns}

\item{br}{:a vector of benchmark returns}
}
\description{
Cumulative return is the compounded return in a given period. The excess return is the
difference between the cumulative return of a risky asset and the cumulative return of a benchmark.
}
\examples{
brtn <- runif(12, -1, 1)
artn <- runif(12, -1, 1)
pt.cmexrtn(artn,brtn)
}
