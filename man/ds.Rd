\name{ds}
\alias{ds}
\title{deseasonalize a time series}
\description{Deseasonalization method for monthly and annual}

\usage{
ds(z, Fm = 6, Fs = 6, type = c("daily", "monthly"), searchQ=TRUE, lag.max=20, ic=c("BIC","AIC"), standardizeQ=TRUE)
}

\arguments{

\item{z}{vector or time series}

\item{Fm}{Number of frequency components for the mean}

\item{Fs}{Number of frequency components for the standard deviation}

\item{type}{"daily" or "monthly"}

\item{searchQ}{TRUE, search for best BIC/AIC model using harmonic reqressions of maximum orders Fm and Fs
             for seasonal means and standard deviations. If FALSE, just use input values.}

\item{lag.max}{maximum order for the fitted autoregression}

\item{ic}{ "BIC" or "AIC" model selection}

\item{standardizeQ}{TRUE, then subtract seasonal mean and divide by seasonal standard deviation. Otherwise,
if FALSE, just subtract seasonal mean.}

}

\details{
See McLeod (2012) and Hipel and McLeod (1994) for further details and 
illustrative examples.
}

\value{
When searchQ is TRUE, a list with two components is produced. 
The first component 'dspar' is the matrix whose rows are c(Fm, Fs, p, IC),
where Fm and Fs are the number of Fourier components used for the mean and sd,
p=AR order selected and IC is the value of the information criterion
The second component is the deseasonalized time series.
When searchQ is FALSE, just the deasonalized time series is returned.
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
#Simple example.
out <- ds(nottem)
#from the table below we see that 2 Fourier components are used for the seasonal means
# and 0 components for the seasonal standard deviations.
out$dispar
#check that the series is deasonalized using the cumulative periodogram test
cpgram(out$z)
#
#As a check, compute deaseasonalized time series using full transformation.
#Then monthly means should be close to 0 and monthly sd close to 1.0.
#But not exact due to harmonic regression errors.
z <- ds(nottem, Fm=6, Fs=6, type="monthly")$z
apply(matrix(z, ncol=12, byrow=TRUE), MARGIN=2, mean )
apply(matrix(z, ncol=12, byrow=TRUE), MARGIN=2, sd )
}

\keyword{ ts }
