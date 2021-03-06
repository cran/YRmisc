% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reg.bic.R
\name{reg.bic}
\alias{reg.bic}
\title{BIC for lm.fit}
\usage{
reg.bic(fit,w)
}
\arguments{
\item{fit}{:the outcome of lm.fit}

\item{w}{:wright}
}
\description{
Calculate BIC for the outcome of lm.fit This function is built for reg.linreg() for higher efficiency only.
It can't be used for calculating BIC in general operation.
}
\examples{
X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
Y <- as.data.frame(EuStockMarkets)$FTSE
fit <- lm.fit(x = X, y = Y)
w <- rep(1,length(Y))
reg.bic(fit,w)
}
