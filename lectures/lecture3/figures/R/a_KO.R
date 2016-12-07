library(tikzDevice)
library(MASS)
source('../../functions.R')

#################################
## no trend
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(seq(from=0.05, to=0.95, length=n))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

GP <- GPR(x,X,F,kGauss,c(1,.1))

tikz('trend_pb.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
#lines(x,sin(2*pi*x) + sin(4*pi*x))
dev.off()
tools::texi2dvi('trend_pb.tex',pdf=T)

#################################
## no trend
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(seq(from=0.05, to=0.95, length=n))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

GP <- GPR(x,X,F,kGauss,c(1,.1))

tikz('trend_pb.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
#lines(x,sin(2*pi*x) + sin(4*pi*x))
dev.off()
tools::texi2dvi('trend_pb.tex',pdf=T)

#################################
## known trend
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(seq(from=0.05, to=0.95, length=n))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

## constant
trend <- function(x){
  0*x+10
}
GP <- GPRtrend(x,X,F,trend,kGauss,c(1,.1))

tikz('trend_knowncst.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
lines(x,t(x),lty=2)
dev.off()
tools::texi2dvi('trend_knowncst.tex',pdf=T)

## linear
trend <- function(x){
  -2*x+11
}
GP <- GPRtrend(x,X,F,trend,kGauss,c(1,.1))

tikz('trend_knownlin.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
lines(x,t(x),lty=2)
dev.off()
tools::texi2dvi('trend_knownlin.tex',pdf=T)

#################################
## basic ordinary kriging
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(seq(from=0.05, to=0.95, length=n))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

tikz('trend_dataordinary.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(X, F, pch=4, cex=1,lwd=3,xlab="$x$",ylab="$F$",xlim=c(-.25,1.25),ylim=c(8.5,12.5))
#lines(x,t(x),lty=2)
dev.off()
tools::texi2dvi('trend_dataordinary.tex',pdf=T)

tikz('trend_basicordinary.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(X, F, pch=4, cex=1,lwd=3,xlab="$x$",ylab="$F$",xlim=c(-.25,1.25),ylim=c(8.5,12.5))
lines(x,0*x+mean(F),lty=2)
dev.off()
tools::texi2dvi('trend_basicordinary.tex',pdf=T)

#################################
## problem basic ordinary kriging
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(c(seq(from=0.10, to=0.20, length=n-1),0.95))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

tikz('trend_pbbasicordinary.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(X, F, pch=4, cex=1,lwd=3,xlab="$x$",ylab="$F$",xlim=c(-.25,1.25),ylim=c(8.5,12.5))
lines(x,0*x+mean(F),lty=2)
dev.off()
tools::texi2dvi('trend_pbbasicordinary.tex',pdf=T)

## Actual estimation
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(c(seq(from=0.10, to=0.20, length=n-1),0.95))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

mu <- GLSo(X,F,trend,kGauss,c(1,0.1))


tikz('trend_estimordinary.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(X, F, pch=4, cex=1,lwd=3,xlab="$x$",ylab="$F$",xlim=c(-.25,1.25),ylim=c(8.5,12.5))
lines(x,0*x+mu,lty=2)
dev.off()
tools::texi2dvi('trend_estimordinary.tex',pdf=T)
