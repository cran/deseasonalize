\name{Saugeen}
\alias{Saugeen}
\docType{data}

\title{Saugeen river, Walkerton, monthly from Jan 1915 to December 1976}

\description{Flows in cms}

\usage{data(Saugeen)}

\format{
  The format is:
 Time-Series [1:744] from 1915 to 1977: 16 30.3 35.4 41.9 14.7 ...
}

\details{Hipel and McLeod (1976, p.476) found the optimal deseasonalization for this data with an ARMA(1,1)
was with Fm=5 and Fs=4.}

\source{Environment Canada}

\references{
K. W. Hipel and A. I. McLeod (1994). 
Time Series Modelling of Water Resources and Environmental Systems.
Elsevier.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/default.htm}
}

\examples{
#time series plot
plot(Saugeen)
#
}

\keyword{datasets}
