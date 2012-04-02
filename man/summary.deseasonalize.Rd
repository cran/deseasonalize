\name{summary.deseasonalize}
\alias{summary.deseasonalize}
\title{ Summary Method for "deseasonalize" Object }
\description{
summary for "deseasonalize" object.
}
\usage{
\method{summary}{deseasonalize}(object, ...)
}
\arguments{
  \item{object}{ "deseasonalize" object }
  \item{...}{ optional arguments }
}
\value{
A printed summary is given
}
\author{ A.I. McLeod }


\seealso{ 
\code{\link{print.deseasonalize}}, 
\code{\link{ds}} 
}

\examples{
out <- ds(nottem, Fm=6, Fs=6)
summary(out)
}

\keyword{ ts }
