% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt.btavg.R
\name{pt.btavg}
\alias{pt.btavg}
\title{Batting average}
\usage{
pt.btavg(ar,br)
}
\arguments{
\item{ar}{:a vector of a risk asset return}

\item{br}{:a vector of a benchmark return}
}
\description{
The batting average of the asset is the ratio between the number of periods
where the asset outperforms a benchmark and the total number of periods.
}
\examples{
artn <- runif(100,-1,1)
brtn <- runif(100,-1,1)
pt.btavg(artn,brtn)
}
