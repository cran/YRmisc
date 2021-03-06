% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt.exploss.R
\name{pt.exploss}
\alias{pt.exploss}
\title{Expected loss}
\usage{
pt.exploss(r,p)
}
\arguments{
\item{r}{:a vector of periodic returns}

\item{p}{:target return}
}
\description{
This function give the expected loss of given asset returns.
}
\examples{
rt <- runif(12,-1,1) # generate random number to simulate returns
pt.exploss(rt,0)
pt.exploss(rt,1)
}
