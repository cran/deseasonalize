#Source: EGLondonMaxTemp.R
require("FitAR")
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
