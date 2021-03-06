% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt.dbeta.R
\name{pt.dbeta}
\alias{pt.dbeta}
\title{Dual-beta}
\usage{
pt.dbeta(ar,mr,rf)
}
\arguments{
\item{ar}{:a vector of a risk asset return}

\item{mr}{:a vector of market return}

\item{rf}{:risk free rate}
}
\description{
Dual-beta method is to divide market beta into downside beta and upside beta. The principle
behind is that upside and downside betas are not the same.
}
\examples{
artn <- runif(24,0,1) # generate random number to simulate returns
mrtn <- runif(24,-1,1)
pt.dbeta(artn,mrtn,0.024)
}
