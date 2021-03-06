% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pl.hs.R
\name{pl.hs}
\alias{pl.hs}
\title{Plot histograms and scatter plots for a data frame}
\usage{
pl.hs(x,a,dependent,l)
}
\arguments{
\item{x}{:a data frame}

\item{a}{:the type of graph you want; a = 1 for histograms; a = 2 for scatter plots; a = 0 for both}

\item{dependent}{:the dependent variable for scatterplots}

\item{l}{: the beginning label number in the title (default set to 1)}
}
\description{
Plotting histograms or scatter plots of your choice for a data frame. Also the function will name the graphs and number them.
The purpose of the function is to save time when plotting graphs for a regression analysis or other usage.
The function can plot, name and number the graphs at one step.
}
\examples{
pl.hs(mtcars,0,"mpg",1)
}
