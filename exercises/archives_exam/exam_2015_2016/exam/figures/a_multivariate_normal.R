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
m <- 0*x + 2
K <- kBrown(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(50,m,K)

tikz('GPR_simBrown.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_1$')
for (i in 2:30) {
  lines(x, y[i,], col=diverge_hcl(30)[31-i])
}
dev.off()
tools::texi2dvi('GPR_simBrown.tex',pdf=T,clean=TRUE)
file.remove('GPR_simBrown.tex')

###############################################
### Gauss path
n <- 100
x <- matrix(seq(from=0, to=1, length=n))
m <- -2*x + 2
K <- kGauss(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(100,m,K)

tikz('GPR_simGauss.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_2$')
for (i in 2:30) {
  lines(x, y[i,], col=diverge_hcl(30)[31-i]) #col=diverge_hcl(100)[i]
}
dev.off()
tools::texi2dvi('GPR_simGauss.tex',pdf=T,clean=TRUE)
file.remove('GPR_simGauss.tex')

###############################################
### Mat path
n <- 300
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x
K <- kMat32(x,x)
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(100,m,K)

tikz('GPR_simMat.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_3$')
for (i in 2:100) {
  lines(x, y[i,], col=diverge_hcl(100)[101-i]) #col=diverge_hcl(100)[i]
}
dev.off()
tools::texi2dvi('GPR_simMat.tex',pdf=T,clean=TRUE)
file.remove('GPR_simMat.tex')

###############################################
### exp path
n <- 500
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x - 10
K <- kExp(x,x,c(4,0.5))
low95 <- m - 1.96*sqrt(diag(K))
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(50,m,K)

tikz('GPR_simExp.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_4$')
for (i in 2:30) {
  lines(x, y[i,], col=diverge_hcl(30)[31-i])
}
dev.off()
tools::texi2dvi('GPR_simExp.tex',pdf=T,clean=TRUE)
file.remove('GPR_simExp.tex')



###################################################################
###################################################################

###############################################
### Mat52 path
n <- 100
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x
K <- kMat52(x,x,c(1,0.1))
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(100,m,K)

tikz('GPR_simMat52_1_01.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_5$')
for (i in 2:50) {
  lines(x, y[i,], col=diverge_hcl(50)[51-i]) #col=diverge_hcl(100)[i]
}
dev.off()
tools::texi2dvi('GPR_simMat52_1_01.tex',pdf=T,clean=TRUE)
file.remove('GPR_simMat52_1_01.tex')



###############################################
### Mat52 path
n <- 20
x <- matrix(seq(from=0, to=1, length=n))
m <- 0*x
K <- kMat52(x,x,c(100,10))
low95 <- m - 1.96*sqrt(diag(K)) 
upp95 <- m + 1.96*sqrt(diag(K)) 
prior <- list(mean=m,cov=K,lower95=low95,upper95=upp95)

y <- mvrnorm(100,m,K)

tikz('GPR_simMat52_100_10.tex', standAlone = TRUE, width=7, height=5)
#plotGPR(x,prior)
par(mar=c(5,5,.5,.5),cex.axis=1.5,cex.lab=2)
plot(x, y[1,], col=mycol(), type='l',ylim=range(y),xlab='$x$',ylab='$Z_6$')
for (i in 2:50) {
  lines(x, y[i,], col=diverge_hcl(50)[51-i]) #col=diverge_hcl(100)[i]
}
dev.off()
tools::texi2dvi('GPR_simMat52_100_10.tex',pdf=T,clean=TRUE)
file.remove('GPR_simMat52_100_10.tex')













