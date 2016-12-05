library(tikzDevice)
library(MASS)
source('a_functions.R')

##################################################################"
### Haar basis
x <- seq(from=0, to=.99, length=100)

mother <- function(x){
  (2*(x<.5) -1)*(x>=0)*(x<1) 
}

wavelet <- function(x,i,j){
  k<-i
  if(i==0) k <- 1
  return(2^(k/2)*mother(2^i*x - j))
}

tikz('wavelet_haar00.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(1,1))
plot(x,wavelet(x,0,0), ylab='$\\psi_{0,0}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
dev.off()
tools::texi2dvi('wavelet_haar00.tex',pdf=T)

tikz('wavelet_haar.tex', standAlone = TRUE, width=6, height=4)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(2,3))
plot(x,wavelet(x,-1,0), ylab='$\\psi_{-1,0}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
plot(x,wavelet(x,0,0), ylab='$\\psi_{0,0}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
plot(x,wavelet(x,1,0), ylab='$\\psi_{1,0}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
plot(x,wavelet(x,1,1), ylab='$\\psi_{1,1}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
plot(x,wavelet(x,2,0), ylab='$\\psi_{2,0}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
plot(x,wavelet(x,2,1), ylab='$\\psi_{2,1}$',col=darkBlue, type='l', lwd = 2,ylim=c(-1.1,1.1))
dev.off()
tools::texi2dvi('wavelet_haar.tex',pdf=T)

##################################################################"
### Linear regression
x <- seq(from=0, to=.99, length=100)

X <-  1:8*1/8 - 1/16
F <- sin(5*pi*X) + 2*X

H <- function(x){
  cbind(wavelet(x,-1,0),wavelet(x,0,0),wavelet(x,1,0),wavelet(x,1,1),wavelet(x,2,0),wavelet(x,2,1),wavelet(x,2,2),wavelet(x,2,3))
}

beta <- solve(t(H(X))%*%H(X)) %*% t(H(X)) %*% F
m <- H(x) %*% beta

tikz('wavelet_linreg.tex', standAlone = TRUE, width=6, height=4)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(1,1))
plot(x,m,ylab='lin. reg. with Haar basis',col=darkBlue, type='l', lwd = 2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi('wavelet_linreg.tex',pdf=T)

## cut above threshold
norm <- 2^(-c(0,0,1,1,2,2,2,2))
betacut <- beta

betacut[abs(betacut)<.1] <- 0
mcut <- H(x) %*% betacut

tikz('wavelet_cut.tex', standAlone = TRUE, width=6, height=4)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(1,1))
plot(x,mcut,ylab='wavelets with threshold',col=darkBlue, type='l', lwd = 2,ylim=range(F))
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi('wavelet_cut.tex',pdf=T)

##################################################################"
### Meyer
x <- seq(from=-5, to=5, length=501)

meyer <- function(x){
  z <- (sin(2*pi*x) - sin(pi*x))/pi/x
  z[is.nan(z)] <- 1
  return(z)
}

tikz('wavelet_meyer00.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(1,1))
plot(x,meyer(x), ylab='Meyer wavelet',col=darkBlue, type='l', lwd = 2)
dev.off()
tools::texi2dvi('wavelet_meyer00.tex',pdf=T)

##################################################################"
### Mexican hat
x <- seq(from=-5, to=5, length=501)

mexhat <- function(x,sig=1){
  2/(sqrt(3*sig)*pi^0.25)*(1-x^2/sig^2) *exp(-.5*x^2/sig^2)
}

tikz('wavelet_mexhat00.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2,mfrow=c(1,1))
plot(x,mexhat(x), ylab='Ricker wavelet',col=darkBlue, type='l', lwd = 2)
dev.off()
tools::texi2dvi('wavelet_mexhat00.tex',pdf=T)
