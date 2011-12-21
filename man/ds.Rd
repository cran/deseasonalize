\name{ds}
\alias{ds}
\title{
deseasonalize a time series
}
\description{
tba
}
\usage{
ds(z, Fm = 1, Fs = 0, ic = c("BIC", "AIC"), type = c("daily", "monthly"))
}
\arguments{
  \item{z}{
vector or time series
}
  \item{Fm}{
Number of frequency components for the mean
}
  \item{Fs}{
Number of frequency components for the standard deviation
}
  \item{ic}{
information criterion "AIC" or "BIC"
}
  \item{type}{
"daily" or "monthly"
}
}
\details{
tba
}
\value{
A list with two components. The first component 'out' is the vector c(Fm, Fs, p, IC),
where p=AR order selected and IC is the value of the information criterion.
The second component is the deseasonalized time series.
}

\references{
K. W. Hipel and A. I. McLeod (1994). 
Time Series Modelling of Water Resources and Environmental Systems.
Elsevier.
}

\author{
A. I. McLeod (aimcleod@uwo.ca)
}

\examples{
ds(nottem, Fm=1, Fs=1, type="monthly")
}

\keyword{ ts }
