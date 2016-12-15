
library(tikzDevice)
library(gpR)
library(MASS)
#source("fonctionsKrigeage.R")

#######################################################
## large number of observations
set.seed(8)
ng <- 101
n <- 20
x <- matrix(seq(from=0, to=1, length=ng))
ind <- matrix(round(runif(n)*99+1))

K <- kMat52(x,x,c(1,.2))

Y <- mvrnorm(1,rep(0,ng),K)
# Y <- Y - mean(Y)

Z <- Y[ind]
Z <- pnorm(Z,0,0.25)
Z <- (runif(n)<Z)*2-1

fileName <- 'classif_data.tex'
tikz(fileName, standAlone = TRUE, width=8, height=4)
par(mar=c(2.5,3.,1.5,1.5))
plot(x,Y,xlab="",ylab="",type='n',lty=3,ylim=c(-1.3,1.3),lwd=2,col="blue")
# lines(x,pnorm(Y,0,0.25),lwd=2,lty=2,col="blue")
points(x[ind],Z)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)


fileName <- 'classif_GP.tex'
tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(2.5,3.,1.5,1.5))
plot(x,Y,xlab="",ylab="",type='l',lty=3,ylim=c(-1.3,1.3),lwd=2,col="blue")
# lines(x,pnorm(Y,0,0.25),lwd=2,lty=2,col="blue")
# points(x[ind],Z)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)


fileName <- 'classif_phiGP.tex'
tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(2.5,3.,1.5,1.5))
plot(x,Y,xlab="",ylab="",type='n',lty=3,ylim=c(-1.3,1.3),lwd=2,col="blue")
lines(x,pnorm(Y,0,0.25),lwd=2,lty=2,col="blue")
# points(x[ind],Z)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)


fileName <- 'classif_datasmall.tex'
tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(2.5,3.,1.5,1.5))
plot(x,Y,xlab="",ylab="",type='n',lty=3,ylim=c(-1.3,1.3),lwd=2,col="blue")
# lines(x,pnorm(Y,0,0.25),lwd=2,lty=2,col="blue")
points(x[ind],Z)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

