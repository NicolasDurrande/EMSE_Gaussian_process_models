library(tikzDevice)
library(MASS)
library(colorspace)
source("a_functions.R")

mycol <- function(){
  diverge_hcl(50)[round(runif(1,0.51,50.49))]
}

###############################################
### brown path
n <- 500
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x
K <- kBrown(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(50,m,K)

tikz('GPR_simBrown.tex', standAlone = TRUE, width=5, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z$')
for (i in 2:30) {
  lines(x, y[i,], col=mycol())
}
dev.off()
tools::texi2dvi('GPR_simBrown.tex',pdf=T,clean=TRUE)
file.remove('GPR_simBrown.tex')

###############################################
### Gauss path
n <- 100
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x
K <- kGauss(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(100,m,K)

tikz('GPR_simGauss.tex', standAlone = TRUE, width=5, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z$')
for (i in 2:100) {
  lines(x, y[i,], col=diverge_hcl(100)[101-i]) #col=diverge_hcl(100)[i]
}
dev.off()
tools::texi2dvi('GPR_simGauss.tex',pdf=T,clean=TRUE)
file.remove('GPR_simGauss.tex')


###############################################
###############################################
### Observations

ftest <- function(x) sin(2*pi*x) + x

n <- 101
x <- matrix(seq(from=0, to=1, length=n))
y <- ftest(x)

X <- matrix(seq(0.1,0.9,length.out=6))
Y <- ftest(X)

tikz('GPR_obs.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(X, Y, col='black', pch=4 ,ylim=c(-0.5,1.5),xlab='$X$',ylab='$f(X)$',lwd=4)
dev.off()
tools::texi2dvi('GPR_obs.tex',pdf=T,clean=TRUE)
file.remove('GPR_obs.tex')

tikz('GPR_obs_small.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(X, Y, col='black', pch=4 ,ylim=c(-0.5,1.5),xlab='$X$',ylab='$f(X)$',lwd=4)
dev.off()
tools::texi2dvi('GPR_obs_small.tex',pdf=T,clean=TRUE)
file.remove('GPR_obs_small.tex')


###############################################
### Gauss prior
m <- 0*x
K <- kGauss(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)
y <- mvrnorm(100,m,K)

tikz('GPR_GaussPrior.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z$')
for (i in 2:100) {
  lines(x, y[i,], col=mycol())
}
dev.off()
tools::texi2dvi('GPR_GaussPrior.tex',pdf=T,clean=TRUE)
file.remove('GPR_GaussPrior.tex')


###############################################
### Gauss posterior
pred <- GPR(x,X,Y,kGauss)

y <- mvrnorm(100,pred$mean,pred$cov)

tikz('GPR_GaussPosterior.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(5,5,.75,.75),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z(x)|Z(X)=F$')
for (i in 2:30) {
  lines(x, y[i,], col=mycol())
}
points(X, Y, col='black', pch=4,lwd=3)
dev.off()
tools::texi2dvi('GPR_GaussPosterior.tex',pdf=T,clean=TRUE)
file.remove('GPR_GaussPosterior.tex')

###############################################
### confidence intervals

tikz('GPR_GaussGPR.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(5,5,.75,.75),cex.axis=1.5,cex.lab=2)
plotGPR(x, pred, xlab='$x$',ylab='$Z(x)|Z(X)=F$')
points(X, Y, col='black', pch=4,lwd=3)
dev.off()
tools::texi2dvi('GPR_GaussGPR.tex',pdf=T,clean=TRUE)
file.remove('GPR_GaussGPR.tex')







#################################
## 1d
x <- seq(-5,5,length=101)
f <- dnorm(x,0,1)

tikz('MVN_dens1.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5))
plot(x,f,type='l',lwd=2,col=darkBlue,ylab="density",cex.axis=2,cex.lab=2)
dev.off()
tools::texi2dvi('MVN_dens1.tex',pdf=T)

#################################
## dim d
multdens <- function(x,m,K){
  d <- length(m)
  xc <- matrix(x-m,ncol=1)
  return(1/sqrt((2*pi)^d*det(K)) * exp(-.5*t(xc)%*%solve(K)%*%xc))
}

m <- c(0,0)
K <- matrix(c(3,2,2,3),2)
g <- seq(-5,5,length=31)
G <- as.matrix(expand.grid(g,g))
F <- rep(0,dim(G)[1])
for(i in 1:dim(G)[1]){
  F[i] <- multdens(G[i,],m,K)
}

tikz('MVN_dens2.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(.5,2.1,.5,1.5))
persp(g,g,matrix(F,length(g)),xlab="$x_1$",ylab="$x_2$",zlab="density",cex.axis=2,cex.lab=2,theta = 20, phi = 25)
dev.off()
tools::texi2dvi('MVN_dens2.tex',pdf=T)

#################################
## samples
K <- matrix(c(1,2,2,7),2)
Y <- mvrnorm(700,c(0,2),K)
tikz('MVN_gaussvec1.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5))
plot(Y,xlab='$Y_1$',ylab='$Y_2$',asp=1,col=rgb(0,0,0,.5),cex.axis=2,cex.lab=2)
dev.off()
tools::texi2dvi('MVN_gaussvec1.tex',pdf=T)

K <- matrix(c(1,0,0,1),2)
Y <- mvrnorm(1000,c(0,0),K)
tikz('MVN_gaussvec2.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5))
plot(Y,xlab='$Y_1$',ylab='$Y_2$',asp=1,col=rgb(0,0,0,.5),cex.axis=2,cex.lab=2)
dev.off()
tools::texi2dvi('MVN_gaussvec2.tex',pdf=T)

K <- matrix(c(4,-2,-2,1.5),2)
Y <- mvrnorm(1500,c(0,0),K)
for(i in 1:1500){
  if(runif(1)>.7 ) Y[i,1] <- -Y[i,1] 
}
tikz('MVN_gaussvec3.tex', standAlone = TRUE, width=5,height=5)
par(mar=c(4.5,5.1,1.5,1.5))
plot(Y,xlab='$Y_1$',ylab='$Y_2$',asp=1,col=rgb(0,0,0,.5),cex.axis=2,cex.lab=2)
dev.off()
tools::texi2dvi('MVN_gaussvec3.tex',pdf=T)



##################################################################"
### plot kernel 
x <- seq(from=-5, to=5, length=201)
K <- kMat52(x,x,c(1,50))

tikz('MVN_kern150.tex', standAlone = TRUE, width=5,height=5)
par(mar=c(4.5,5.1,1.5,1.5))
plot(x,K[,100],type='l',ylab='k(x,0)',cex.axis=1.5,cex.lab=2,ylim=c(0,3))
dev.off()
tools::texi2dvi('MVN_kern150.tex',pdf=T)

m <- 0*x
Z <- mvrnorm(200,m,K)

tikz('MVN_traj150.tex', standAlone = TRUE, width=5,height=5)
plot(x,Z[1,],ylim=c(-6,6),type='l',ylab="samples of Z",cex.axis=1.5,cex.lab=2)
for(i in 2:100){
  lines(x,Z[i,],col=i)
}
dev.off()
tools::texi2dvi('MVN_traj150.tex',pdf=T)


