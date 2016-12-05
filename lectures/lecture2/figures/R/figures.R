library(DiceKriging)
library(MASS)
library(tikzDevice)
library(colorspace)


source("a_newfunctions.R")
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
getwd()


####################################################################
# Prior samples for Exponential kernel (can also try matern5_2", "matern3_2")
X <- seq(0.1,0.9,length.out=6)
Y <- sin(2*pi*X) + X

x <- seq(0,1,0.002)
m <- km(design=data.frame(x=X), response=Y, covtype="exp",coef.trend=0,coef.cov=.5,coef.var=1,nugget=1e-8)
Z <- t(simulate(m, nsim = 10, newdata = data.frame(x = x), cond = FALSE))

tikz('Fig2-sim-exp.tex', standAlone = TRUE, width=5, height=4)
par(mar=c(.5,.5,2.5,.5),cex.axis=1.5,cex.lab=2)
matplot(x,Z,type='l',lty=1.5,lwd=1,col=sample(diverge_hcl(10)),xlab='',ylab='', xaxt='n',yaxt='n',main='\\huge Exponential kernel:')             
dev.off()
tools::texi2dvi('Fig2-sim-exp.tex',pdf=T,clean=TRUE)
file.remove('Fig2-sim-exp.tex')


####################################################################
# Prior samples for Gaussian kernel
x <- seq(0,1,0.02)
m <- km(design=data.frame(x=X), response=Y, covtype="gauss",coef.trend=0,coef.cov=.2,coef.var=1,nugget=1e-8)
Z <- t(simulate(m, nsim = 10, newdata = data.frame(x = x), cond = FALSE))

tikz('Fig2-sim-rbf.tex', standAlone = TRUE, width=5, height=4)
par(mar=c(.5,.5,2.5,.5),cex.axis=1.5,cex.lab=2)
matplot(x,Z,type='l',lty=1.5,col=sample(diverge_hcl(10)),xlab='',ylab='', xaxt='n',yaxt='n',main='\\huge Gaussian kernel:')             
dev.off()
tools::texi2dvi('Fig2-sim-rbf.tex',pdf=T,clean=TRUE)
file.remove('Fig2-sim-rbf.tex')

####################################################################
# interpolating kriging models
x <- seq(0,1,0.02)
m <- km(design=data.frame(x=X), response=Y, covtype="gauss",coef.trend=0,coef.cov=.2,coef.var=1)
pred <- predict(m,newdata=data.frame(x),type="SK",cov.compute =TRUE)

tikz('Fig2-GP-rbf.tex', standAlone = TRUE, width=5, height=4)
par(mar=c(.5,.5,2.5,.5),cex.axis=1.5,cex.lab=2)
plot(X,Y,pch=4,lwd=3,xlab='',ylab='', xaxt='n',yaxt='n',ylim=c(-1,2),main='\\huge Gaussian kernel:')
plotGPR(x,pred,add=TRUE,ylim=c(-1,2))
dev.off()
tools::texi2dvi('Fig2-GP-rbf.tex',pdf=T,clean=TRUE)
file.remove('Fig2-GP-rbf.tex')

####################################################################
# interpolating kriging models
x <- seq(0,1,0.002)
m <- km(design=data.frame(x=X), response=Y, covtype="exp",coef.trend=0,coef.cov=.5,coef.var=1)
pred <- predict(m,newdata=data.frame(x),type="SK",cov.compute =TRUE)

tikz('Fig2-GP-exp.tex', standAlone = TRUE, width=5, height=4)
par(mar=c(.5,.5,2.5,.5),cex.axis=1.5,cex.lab=2)
plot(X,Y,pch=4,lwd=3,xlab='',ylab='', xaxt='n',yaxt='n',ylim=c(-1,2),main='\\huge Exponential kernel:')
plotGPR(x,pred,add=TRUE,ylim=c(-1,2))
dev.off()
tools::texi2dvi('Fig2-GP-exp.tex',pdf=T,clean=TRUE)
file.remove('Fig2-GP-exp.tex')



####################################################################
##################################################
## Noisy observations
X <- seq(0.1,0.9,length.out=30)
Y <- sin(2*pi*X) + X + rnorm(length(X),0,0.1)

tikz('noisyObs.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(X,Y,pch=4,lwd=3,xlab='x',ylab='f(X)',ylim=c(-1,2))
dev.off()
tools::texi2dvi('noisyObs.tex',pdf=T,clean=TRUE)
file.remove('noisyObs.tex')


##################################################
## kriging model with noise
m <- km(design=data.frame(x=X), response=Y, covtype="gauss",noise.var=rep(0.01,length(X)),coef.trend=0,coef.cov=.2,coef.var=1)
pred <- predict(m,newdata=data.frame(x),type="SK",cov.compute =TRUE)

tikz('noisyGPR.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,pred,ylim=c(-1,2))
points(X,Y,pch=4,lwd=3)
dev.off()
tools::texi2dvi('noisyGPR.tex',pdf=T,clean=TRUE)
file.remove('noisyGPR.tex')


