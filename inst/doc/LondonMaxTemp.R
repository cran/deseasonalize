#Source: LondonMaxTemp.R
require("FitAR")
z<-LondonMaxTemp
maxFm <- 3
maxFs <- 3
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
pl<-round(100*exp((m[ind,4]-m[,4])/2),1)/100
m<-cbind(m, pl)
colnames(m) <- c("Fm", "Fs", "p", "BIC", "Plausibility")
tb<-matrix(m[,5], nrow=maxFs+1)
rownames(tb)<-0:maxFs
colnames(tb)<-1:maxFm
mB <- m
mB
tbB <- tb
tb
#produce latex table
require("xtable")
xtb<-xtable(tb, caption="Plausibiliy", digits=1)
print(xtb, file="d:/r/2011/deseasonalize/LondonMaxTempBIC.tex")
#
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
pl<-round(100*exp((m[ind,4]-m[,4])/2),1)/100
m<-cbind(m, pl)
colnames(m) <- c("Fm", "Fs", "p", "AIC", "Plausibility")
tb<-matrix(m[,5], nrow=maxFs+1)
rownames(tb)<-0:maxFs
colnames(tb)<-1:maxFm
mB <- m
mB
tbB <- tb
tb
#produce latex table
require("xtable")
xtb<-xtable(tb, caption="Plausibiliy", digits=1)
print(xtb, file="d:/r/2011/deseasonalize/LondonMaxTempAIC.tex")
#
