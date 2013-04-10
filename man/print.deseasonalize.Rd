\name{print.deseasonalize}
\alias{print.deseasonalize}
\title{ Print Method for "deseasonalize" Object }
\description{
A terse summary is given.
}
\usage{
\method{print}{deseasonalize}(x, ...)
}
\arguments{
  \item{x}{ object of class "deseasonalize" }
  \item{...}{ optional arguments }
}
\value{
A terse summary is displayed
}

\author{ A.I. McLeod}

\seealso{ 
\code{\link{summary.deseasonalize}}
 }
\examples{
ds(nottem, Fm=6, Fs=6, type="monthly", searchQ=FALSE)
}
\keyword{ ts }
