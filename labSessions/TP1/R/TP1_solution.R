## Lab Session 1 - Gaussian process regression 
library(MASS)
library(colorspace)
library(rgl)

par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1,cex.lab=1.5)

####################
## Question 1

kBrown <- function(x,y,param=1){
  param[1]*outer(x,y,"pmin")
}

kExp <- function(x,y,param=c(1,.2)){
  param[1]*exp(-abs(outer(x,y,'-'))/param[2])
}

kGauss <- function(x,y,param=c(1,.2)){
  param[1]*exp(-.5*outer(x,y,'-')^2/param[2]^2)
}

kMat32 <- function(x,y,param=c(1,.2)){
  d <- sqrt(3)*abs(outer(x/param[2],y/param[2],'-'))
  return(param[1]*(1 + d)*exp(-d))
}

kMat52 <- function(x,y,param=c(1,.2)){
  d <- sqrt(5)*abs(outer(x/param[2],y/param[2],'-'))
  return(param[1]*(1 + d +1/3*d^2)*exp(-d))
}

kWhite <- function(x,y,param=1){
  d <- outer(x,y,'-')
  return(param[1]*(abs(d)<0.00001))
}

####################
## Question 2-3:

## plot covariance matrix
x <- seq(0,1,0.01)
Kmat <- kBrown(x,x)
image(x,x,Kmat[length(x):1,])

## Question 2-3
x <- seq(0,1,0.002)
y <- 0.7
sigma2 <- 1
theta <- 0.2
kern <- kBrown

par(mfrow=c(1,2))
plot(x,kern(x,y,c(sigma2,theta)),type='l',lwd=2,ylab=paste0('k(x,',y,')'))
lines(c(y,y),c(0,sigma2),lty=2)

kxx <- kern(x,x,c(sigma2,theta))
Z <- mvrnorm(10,0*x,kxx)
matplot(x,t(Z),type='l',lty=1,lwd=1.5,col=sample(diverge_hcl(10)),xlab='x',ylab='Z(x)')             
par(mfrow=c(1,1))

####################
## Question 4
x <- seq(0,1,0.01)
ind1 <- 21

sigma2 <- 0.5
theta <- 0.2
kxx <- kGauss(x,x,c(sigma2,theta))
Z <- mvrnorm(1000,0*x,kxx)
matplot(x,t(Z),type='l',lty=1,lwd=1.5,col=sample(diverge_hcl(10)),xlab='x',ylab='Z(x)')             
lines(c(x[ind1],x[ind1]),c(-5,5),lty=2,lwd=2)

# Distribution of Z(x1) 
x1 <- x[ind1]
lines(c(x1,x1),c(-4,4),lty=2,lwd=2)

hist(Z[,ind1],30,freq=FALSE,xlab='Z(x1)',xlim=c(-4,4),ylim=1.2*c(0,1/(sqrt(2*pi*sigma2))))
lines(seq(-4,4,0.1),dnorm(seq(-4,4,0.1),0,sqrt(sigma2)))

# joint distribution of (Z(x1),Z(x2))
ind2 <- 82
x2 <- x[ind2]

par(mfrow=c(1,2))
matplot(x,t(Z)[,1:50],type='l',lty=1,lwd=1.5,col=sample(diverge_hcl(10)),xlab='x',ylab='Z(x)')             
lines(c(x1,x1),c(-4,4),lty=2,lwd=2)
lines(c(x2,x2),c(-4,4),lty=2,lwd=2)

plot(Z[,ind1],Z[,ind2],pch=16,col=rgb(32/255,74/255,135/255,.5),xlab='Z(x1)',ylab='Z(x2)')
par(mfrow=c(1,1))

# joint distribution of (Z(x1),Z(x2),Z(x3))
ind3 <- 30
#pairs(Z[,c(ind1,ind2,ind3)],pch=16,col=rgb(32/255,74/255,135/255,.25))

plot3d(Z[,ind1],Z[,ind2],Z[,ind3],xlab='Z(x1)',ylab='Z(x2)',zlab='Z(x3)',type='p',radius=0.1,col=rgb(32/255,74/255,135/255),alpha=0.6)

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
