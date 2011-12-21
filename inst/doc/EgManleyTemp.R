#Source: EGManleyTemp.R
require("FitAR")
z<-ManleyTemp
maxFm <- 6
maxFs <- 6
#
#Using BIC.  Optimal is Fm=1, Fs=0
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
#Using AIC. optimal is Fm=2, Fs=0
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



