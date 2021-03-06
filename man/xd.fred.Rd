% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xd.fred.R
\name{xd.fred}
\alias{xd.fred}
\title{Download data from Federal Reserve Bank of St. Louis}
\usage{
xd.fred(tkr, start_date, end_date)
}
\arguments{
\item{tkr}{:one data ticker or a string of tickers used by the database}

\item{start_date}{:starting date of the data(default is set as 1900-01-01)}

\item{end_date}{:ending date of the data(default is set as 2018-01-01)}
}
\description{
This function returns a data from the Federal Reserve Bank of St. Louis database. It can take one ticker or a string of tickers, which will output a merged data frame with all observations.
}
\examples{
 cpi <- xd.fred("CPIAUCSL") # CPI data
head(cpi)
tail(cpi)

#Frequently used tickers:
#CPIAUCSL: Consumer Price Index for All Urban Consumers: All Items
#A191RL1Q225SBEA: Real Gross Domestic Product
#DGS10: 10-Year Treasury Constant Maturity Rate
#UNRATE: Civilian Unemployment Rate
}
