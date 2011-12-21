ds <-
function(z, Fm=1, Fs=0, ic=c("BIC","AIC"), type=c("daily", "monthly")){
    type <- match.arg(type)
    ic <- match.arg(ic)
    if (type=="daily") s<-365.25 else s<-12
    nz <- length(z)
    if (nz < s) stop("error: need at least one year (366 days or 12 months)")
    if (Fm <= 0) stop("error: need Fm > 0")
    if (max(Fm,Fs) > s/2) stop("error: Fm or Fs setting too large")
    xt <- 1:nz
    X <- matrix(rep(1,(2*Fm+1)*nz), ncol=2*Fm+1)
    jj <- 2
    for (j in 1:Fm){
        x <- j*(2*pi/s)*xt
        X[,jj] <- sin(x)
        X[,jj+1] <- cos(x)
        jj <- jj+2
        }
    zds <- zdm <- resid(lm.fit(x=X, y=z))
    estsd <- rep(sd(zds), nz)
    J2 <- 0
    if (Fs > 0) {
        zsq <- zdm^2
        X <- matrix(rep(1,(2*Fs+1)*nz), ncol=2*Fs+1)
        jj <- 2
        for (j in 1:Fs){
            x <- j*(2*pi/s)*xt
            X[,jj] <- sin(x)
            X[,jj+1] <- cos(x)
            jj <- jj+2
            }
        estsd <- sqrt(fitted(lm.fit(x=X, y=zsq)))
    }
    J2 <- -sum(log(estsd))
    zds <- zdm/estsd
    ans <- SelectModel(zds, lag.max = 20, Criterion = ic, Best=2)
    pHat <- ans[1,1]
    if (ic == "BIC") parPenalty <- log(nz)*2*(Fm+Fs) else
        parPenalty <- 4*(Fm+Fs)
    BestBIC <- ans[1,2] - J2 + parPenalty
    out<-c(Fm, Fs, pHat, BestBIC)
    if (ic == "BIC") names(out) <- c("Fm", "Fs", "p", "BIC") else
        names(out) <- c("Fm", "Fs", "p", "AIC")
    list(out=out, z=zds)
}
