% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pl.tsgg.R
\name{pl.tsgg}
\alias{pl.tsgg}
\title{Plot times series plot for a data frame with ggplot2}
\usage{
pl.tsgg(x,l)
}
\arguments{
\item{x}{:a data frame}

\item{l}{: the beginning label number in the title (default set to 1)}
}
\description{
Plotting time series plot for a data frame with 4 per page, with titles and label numbers automatically generated.
}
\examples{
pl.tsgg(as.data.frame(EuStockMarkets),1)
}
