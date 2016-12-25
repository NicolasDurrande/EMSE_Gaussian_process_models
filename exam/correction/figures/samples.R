## Lab Session 1 - Gaussian process regression 
library(MASS)
library(colorspace)
library(tikzDevice)

par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1,cex.lab=1.5)

####################
## Question 1

k0 <- function(x,y,param=1){
  param[1]*matrix(1,length(x),length(y))
}

k1 <- function(x,y,param=1){
  param[1]*outer(x,y)
}

k2 <- function(x,y,param=1){
  param[1]*outer(x^2,y^2)
}

kGauss <- function(x,y,param=c(1,.2)){
  param[1]*exp(-.5*outer(x,y,'-')^2/param[2]^2)
}

####################
## plot samples
x <- seq(-1,1,0.02)

sigma2 <- 1
theta <- 0.2

plotsamples <- function(fileName,Z,ylab){
  tikz(fileName, standAlone = TRUE, width=4, height=4)
  par(mar=c(4.5,5,1.5,1.5),cex.axis=1.5,cex.lab=2)
  matplot(x,t(Z),type='l',lty=1,lwd=1.5,ylim=c(-3,3),
         col=diverge_hcl(nrow(Z)),xlab='x',ylab=ylab)             
  dev.off()
  tools::texi2dvi(fileName,pdf=T,clean=TRUE)
  file.remove(fileName)
}

kxx <- k0(x,x,sigma2)
Z <- mvrnorm(5,0*x,kxx)
plotsamples('samplesZ0.tex',Z,'$Z_0(x)$')

kxx <- k1(x,x,sigma2)
Z <- mvrnorm(5,0*x,kxx)
plotsamples('samplesZ1.tex',Z,'$Z_1(x)$')

kxx <- k2(x,x,sigma2)
Z <- mvrnorm(5,0*x,kxx)
plotsamples('samplesZ2.tex',Z,'$Z_2(x)$')

kxx <- kGauss(x,x,c(sigma2,theta))
Z <- mvrnorm(5,0*x,kxx)
plotsamples('samplesZ3.tex',Z,'$Z_3(x)$')









####################
## Question 5
ftest <- function(x){
  x + sin(4*pi*x)
}

X <- seq(0.1,0.9,length=6)
Y <- ftest(X)

plot(X,Y,pch=4,lwd=3,xlab='',ylab='', xlim=c(0,1), ylim=c(-1,2))
lines(x,ftest(x),lty=2,col='red')

####################
## Question 6

GPRmean <- function(x,X,Y,kern,param){
  kxX <- kern(x,X,param)
  kXX_1 <- solve(kern(X,X,param))
  m <- kxX %*% kXX_1 %*% Y
  return(m)
}

GPRcov <- function(x,X,kern,param){
  kxX <- kern(x,X,param)
  kXX_1 <- solve(kern(X,X,param))
  K <- kern(x,x,param) - kxX %*% kXX_1 %*% t(kxX)
  return(K)
}

####################
## Question 7
x <- seq(-5,5,0.01)
kern <- kMat52
param <- c(1,.2)
mp <- GPRmean(x,X,Y,kern,param)
Cp <- GPRcov(x,X,kern,param)
vp <- pmax(diag(Cp),0)
  
plot(X,Y,pch=4,lwd=3,xlab='',ylab='', xlim=range(x), ylim=c(-2,3))
lines(x,ftest(x),col='red',lty=2)

lines(x,mp,col="#204a87",lwd=2)
lines(x,mp-2*sqrt(vp),col="#204a87",lwd=.5,lty=1)
lines(x,mp+2*sqrt(vp),col="#204a87",lwd=.5,lty=1)

####################
## Question 9

Z <- mvrnorm(20,mp,Cp)
matplot(x,t(Z),type='l',lty=1,lwd=1.5,col=sample(diverge_hcl(10)),xlab='x',ylab='Z(x)|Z(X)=Y', ylim=c(-2,3))             
points(X,Y,pch=4,lwd=3)

####################
## Question 10
x <- seq(0,1,0.002)

kern <- kGauss
param <- c(1,.265)

mp <- GPRmean(x,X,Y,kern,param)
Cp <- GPRcov(x,X,kern,param)
vp <- pmax(diag(Cp),0)

plot(X,Y,pch=4,lwd=3,xlab='',ylab='', xlim=range(x), ylim=c(-2,3))
lines(x,ftest(x),col='red')

lines(x,mp,col="#204a87",lwd=2)
lines(x,mp+2*sqrt(vp),col="#204a87",lwd=1.5,lty=3)
lines(x,mp-2*sqrt(vp),col="#204a87",lwd=1.5,lty=3)

errors <- mp-ftest(x)
RMSE <- sqrt(mean(errors^2)) 
RMSE

hist(errors/sqrt(vp),freq=FALSE,xlim=c(-2,2))
y <- seq(-2,2,0.01)
lines(y,dnorm(y))
