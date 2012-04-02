\name{getds}
\alias{getds}

\title{
get deseasonalized time series
}
\description{
This is a utility function. Most users should use the \code{ds}.

}
\usage{
getds(z, s, Fm = 6, Fs = 6, ic = c("BIC", "AIC"), lag.max = 20, standardizeQ=TRUE)
}

\arguments{

  \item{z}{original series}

  \item{s}{seasonal period either s=12 or s=365.25}

  \item{Fm}{Number of Fourier components for seasonal mean. If Fm=0, then only the overall mean of series is used.}

  \item{Fs}{Number of harmonics for seasonal standard deviations.If Fs=0, only overall standard deviation is used.}

  \item{ic}{"BIC" or "AIC"}

  \item{lag.max}{Number of lags used to fit AR}

  \item{standardizeQ}{If TRUE, divide by seasonal standard deviation. Otherwise, only use seasonal mean correction.}
}

\details{
The series is deseasonalized by subtracting the seasonal means
and dividing by the seasonal standard deviations.
If Fm=0, the overall mean is used and if Fs=0, the overall standard
deviation is used.
If standardizeQ is FALSE, the series is not divided by the standard
deviation and only the mean or seasonal mean correction is done.
In addition, the best AR model is determined for
the deaseasonalized series according to the BIC or AIC criterion.
This criterion may be used to select the best deseasonalization.
}

\value{
list with two components: 'dspar' and 'z'.
dspar: vector of length 4 containing Fm, Fs, p, IC-value.
z: deseasonalized series
}

\references{
K. W. Hipel and A. I. McLeod (1994). 
Time Series Modelling of Water Resources and Environmental Systems.
Elsevier.
}

\author{
A. I. McLeod
}


\seealso{
\code{\link{ds}}
}

\examples{
z <- getds(log(Saugeen), s=12, Fm = 5, Fs = 4, ic = "AIC", lag.max = 20)$z
acf(z)
}

\keyword{ ts }
