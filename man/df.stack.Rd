% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.stack.R
\name{df.stack}
\alias{df.stack}
\title{Stack data frame by one classifier}
\usage{
df.stack(df,name)
}
\arguments{
\item{df}{: a data frame used to stack}

\item{name}{: new variable names of the data frame}
}
\description{
Stack data frame by one classifier. This function takes the first column as a ordering variable.
Then it take the variables names and repeat as the second column. The last column will be data under each variable name. This
function is created to enable easier investigation with apply functions.
}
\examples{
df <- data.frame(matrix(nrow=100,ncol=100))
for(i in 1:100){
 df[,i] <- rep(runif(1,1,100),100)
}
dim(df)
hdf <- df.stack(df,c("date","tkr","price"))
}
