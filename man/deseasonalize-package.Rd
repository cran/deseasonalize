\name{deseasonalize-package}
\alias{deseasonalize-package}
\docType{package}

\title{
Optimal deseasonalization for geophysical time series using AR fitting
}

\description{
Deseasonalize daily or monthly time series.
An harmonic regression is fit to the data to estimate the
seasonal means and standard deviations.
The number of terms in the harmonic regression may be
determined using the BIC or generalized AIC.
}

\details{
\tabular{ll}{
Package: \tab deseasonalize\cr
Type: \tab Package\cr
Version: \tab 1.33\cr
Date: \tab 2012-12-15\cr
License: \tab GPL (>= 2.10)\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}

The only function is \code{\link{ds}}. 

For how to use the function \code{\link{ds}}, see the examples provided with the datasets
\code{\link{Saugeen}},
 and
\code{\link{SaugeenDay}}.

A dynamic time series plot for the Saugeen daily riverflow is available
in the subdirectory /inst/doc.

}

\author{
A. I. McLeod and Hyukjun Gweon
 <aimcleod@uwo.ca>
}

\references{

K. W. Hipel and A. I. McLeod (1994). 
Time Series Modelling of Water Resources and Environmental Systems.
Elsevier.

McLeod, A.I. and Zhang, Y. (2008b).
Improved Subset Autoregression: With R Package.
Journal of Statistical Software.


}


\keyword{ package }

