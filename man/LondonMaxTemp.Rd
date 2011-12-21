\name{LondonMaxTemp}
\alias{LondonMaxTemp}
\docType{data}
\title{
Maximum daily temperature (deg F), London, Ontario. January 1, 1979 to December 31, 2005.
}
\description{
Station: London A  Latitude 43.03 (deg N), Longitude (81.43 deg W), Elevation 278.0 m. 
}
\usage{data(LondonMaxTemp)}

\format{
  The format is:
 num [1:9862] 9.3 -3.2 -13.4 -11.1 -11.3 -9.3 -8.3 -9.1 -11.6 -10 ...
}
\details{
Daily values in vector of length 9862. Complete record.
}
\source{
from Canadian government website.
}
\examples{
data(LondonMaxTemp)
z<-LondonMaxTemp
maxFm <- 2
maxFs <- 2
#
#Using BIC
m <- matrix(numeric(4*((maxFm+1)*maxFs)), ncol=4)
colnames(m) <- c("Fm", "Fs", "p", "BIC")
i <- 0
for (iFm in 1:maxFm)
    for (iFs in 0:maxFs) {
        i <- i+1
        m[i,] <- ds(z, Fm=iFm, Fs=iFs, ic="BIC")$out
        }
rownames(m) <- rep(" ", nrow(m))
ind<-which.min(m[,4])
rownames(m)[ind]<-"*"
m
#
#Using AIC
m <- matrix(numeric(4*((maxFm+1)*maxFs)), ncol=4)
colnames(m) <- c("Fm", "Fs", "p", "AIC")
i <- 0
for (iFm in 1:maxFm)
    for (iFs in 0:maxFs) {
        i <- i+1
        m[i,] <- ds(z, Fm=iFm, Fs=iFs, ic="AIC")$out
        }
rownames(m) <- rep(" ", nrow(m))
ind<-which.min(m[,4])
rownames(m)[ind]<-"*"
m
}

\keyword{datasets}
