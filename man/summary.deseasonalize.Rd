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
#Example 1: to save time only try 2 components
out <- ds(nottem, Fm=2, Fs=2, type="monthly")
summary(out)

#Example 2
\dontrun{
out <- ds(nottem, Fm=6, Fs=6, type="monthly")
summary(out)
}
}

\keyword{ ts }
