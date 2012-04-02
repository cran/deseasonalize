\name{LondonMaxTemp}
\alias{LondonMaxTemp}
\docType{data}

\title{Maximum daily temperature (deg F), London, Ontario. January 1, 1979 to December 31, 2005.}


\description{Station: London A  Latitude 43.03 (deg N), Longitude (81.43 deg W), Elevation 278.0 m. }

\usage{data(LondonMaxTemp)}

\format{
  The format is:
 num [1:9862] 9.3 -3.2 -13.4 -11.1 -11.3 -9.3 -8.3 -9.1 -11.6 -10 ...
}

\details{Daily values in vector of length 9862. Complete record.}

\source{from Canadian government website.}

\references{
L. M. King, A. I. McLeod and S. P. Simonovic1, (2012, submitted),
Simulation of historical temperatures using a multisite, multivariate K-nearest neighbour block resampling algorithm with perturbation 
}

\examples{
out <- ds(LondonMaxTemp, type="daily", Fm=2, Fs=0, searchQ=FALSE)
out
acf(out$z)
}

\keyword{datasets}
