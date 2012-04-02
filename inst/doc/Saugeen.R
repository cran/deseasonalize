#Source: Saugeen.R
require("FitAR")
z <- Saugeen
z <- log(z)
maxFm <- 6
maxFs <- 6
#
#Using BIC
m <- matrix(numeric(4*((maxFm+1)*maxFs)), ncol=4)
colnames(m) <- c("Fm", "Fs", "p", "BIC")
i <- 0
for (iFm in 1:maxFm)
    for (iFs in 0:maxFs) {
        i <- i+1
        m[i,] <- ds(z, Fm=iFm, Fs=iFs, ic="BIC", type="monthly")$out
        }
rownames(m) <- rep(" ", nrow(m))
ind<-which.min(m[,4])
rownames(m)[ind]<-"*"
pl<-round(100*exp((m[ind,4]-m[,4])/2),1)/100
m<-cbind(m, pl)
colnames(m) <- c("Fm", "Fs", "p", "BIC", "Plausibility")
tb<-matrix(m[,5], nrow=7)
dimnames(tb)<-list(list(0:maxFs), list(1:maxFm))
rownames(tb)<-0:maxFs
colnames(tb)<-1:maxFm
mB <- m
mB
tbB <- tb
#produce latex table
require("xtable")
xtb<-xtable(tb, caption="Plausibiliy", digits=1)
print(xtb, file="d:/r/2011/deseasonalize/SaugeenBIC.tex")
#
#Using AIC
m <- matrix(numeric(4*((maxFm+1)*maxFs)), ncol=4)
i <- 0
for (iFm in 1:maxFm)
    for (iFs in 0:maxFs) {
        i <- i+1
        m[i,] <- ds(z, Fm=iFm, Fs=iFs, ic="AIC", type="monthly")$out
        }
rownames(m) <- rep(" ", nrow(m))
ind<-which.min(m[,4])
rownames(m)[ind]<-"*"
pl<-round(100*exp((m[ind,4]-m[,4])/2),1)/100
m<-cbind(m, pl)
colnames(m) <- c("Fm", "Fs", "p", "AIC", "Plausibility")
tb<-matrix(m[,5], nrow=7)
dimnames(tb)<-list(list(0:maxFs), list(1:maxFm))
rownames(tb)<-0:maxFs
colnames(tb)<-1:maxFm
mA<-m
tbA <- tb
mA
#produce latex table
require("xtable")
xtb<-xtable(tb, caption="Plausibiliy", digits=1)
print(xtb, file="d:/r/2011/deseasonalize/SaugeenAIC.tex")
#
#Examine deseasonalized series using Fm=5; Fs=4
dsz <- ds(z, Fm=5, Fs=4, type="monthly")$z
graphics.off()
layout(matrix(c(1,2), ncol=1, nrow=2))
boxplot(matrix(dsz, ncol=12, byrow=TRUE),main="deseasonalized")
boxplot(matrix(z, ncol=12, byrow=TRUE),main="original series")
#
apply(matrix(dsz, ncol=12, byrow=TRUE), MARGIN=2, function(x) mean(x^2) )
#
#complete deseasonalization
#as a check the sd should be close to 1 but due to numerical
# errors not exactly equal to 1
dsz <- ds(z, Fm=6, Fs=6, type="monthly")$z
apply(matrix(dsz, ncol=12, byrow=TRUE), MARGIN=2, sd )
