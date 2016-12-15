library(tikzDevice)
library(MASS)
source('aa_functions.R')

##################################################################"
### traj

##
x <- matrix(seq(from=0, to=1, length=201),ncol=1)
K <- kMat32(x,x,c(1,.2)) 
Z1 <- mvrnorm(5,rep(0,length(x)),K)

tikz('traj_1.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
matplot(x,t(Z1),lty=1, lwd= 2, col=tangocol, type='l',ylim=c(-1,1)*max(abs(Z1)),ylab='$Z_1$')
dev.off()
tools::texi2dvi('traj_1.tex',pdf=T)

##
x <- matrix(seq(from=0, to=1, length=301),ncol=1)
K <- kGauss(x,x,c(1,.2)) + kWhite(x,x,c(.01))
Z2 <- mvrnorm(5,rep(0,length(x)),K)

tikz('traj_2.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
matplot(x,t(Z2),lty=1, lwd= 2, col=tangocol, type='l',ylim=c(-1,1)*max(abs(Z2)),ylab='$Z_2$')
dev.off()
tools::texi2dvi('traj_2.tex',pdf=T)

##
x <- matrix(seq(from=0, to=1, length=401),ncol=1)
K <- kBrown(x,x,c(1,.2)) 
Z3 <- mvrnorm(5,rep(0,length(x)),K)

tikz('traj_3.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
matplot(x,t(Z3),lty=1, lwd= 2, col=tangocol, type='l',ylim=c(-1,1)*max(abs(Z3)),ylab='$Z_3$')
dev.off()
tools::texi2dvi('traj_3.tex',pdf=T)

##
x <- matrix(seq(from=-1, to=1, length=3),ncol=1)
K <- kConst(x,x,.05) + kLinear(x,x,1)
Z4 <- mvrnorm(15,rep(0,length(x)),K)

tikz('traj_4.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
matplot(x,t(Z4),lty=1, lwd= 2, col=tangocol, type='l',ylim=c(-1,1)*max(abs(Z4)),ylab='$Z_4$')
dev.off()
tools::texi2dvi('traj_4.tex',pdf=T)


##################################################################"
### model 2D
library(rgl)
ng <- 41
g <- seq(0,1,length=ng)
G <- expand.grid(g,g)

X <- expand.grid(seq(0.05,.95,length=3), seq(0.05,.95,length=3))
F <- as.matrix(branin(X))
trend <- function(x){
  matrix(mean(F),dim(x)[1],1)
}

##################################
## modele 1
GP <- GPRtrend(G,X,F,trend,kGauss,c(.15*var(F),.05,.5))

persp3d(g,g,matrix(GP[[1]],ng),color=tangocol[5],xlab='x1',ylab='x2',zlab='m1(x)',zlim=range(GP[[3]],GP[[4]]),aspect=1,cex=2)
#surface3d(g,g,matrix(branin(G),ng),color=tangocol[3])
surface3d(g,g,matrix(GP[[3]],ng),color=tangocol[8],alpha=0.4,aspect=1)
surface3d(g,g,matrix(GP[[4]],ng),color=tangocol[8],alpha=0.4,aspect=1)
points3d(X[,1],X[,2],F,size=5)
points3d(X[,1],X[,2],0*F+min(GP[[3]]),size=5)
snapshot3d('model1.png')

## test
nt <- 6
Xt <- expand.grid(seq(1/nt/2,1-1/nt/2,length=nt), seq(1/nt/2,1-1/nt/2,length=nt))
Ft <- as.matrix(branin(Xt))

GPt <- GPRtrend(Xt,X,F,trend,kGauss,c(.15*var(F),.05,.5))

K <- matrix(GPt[[2]],nt^2)
P <- eigen(K)$vectors
D <- eigen(K)$values
D[D<0] <- Inf
res <- P%*%diag(1/sqrt(D))%*%t(P) %*% (branin(Xt)-GPt[[1]])

Q2(branin(Xt),GPt[[1]])

tikz('res_1.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
hist(res,10,freq=FALSE,main='model 1',xlab='standardised residuals',ylim=range(dnorm(-50:50/15)))
lines(-50:50/15,dnorm(-50:50/15))
dev.off()
tools::texi2dvi('res_1.tex',pdf=T)

##################################
## modele 2
GP <- GPRtrend(G,X,F,trend,kGauss,c(1*var(F),.24,.24))

persp3d(g,g,matrix(GP[[1]],ng),color=tangocol[5],xlab='x1',ylab='x2',zlab='m2(x)',zlim=range(GP[[3]],GP[[4]]),aspect=1)
#surface3d(g,g,matrix(branin(G),ng),color=tangocol[3])
surface3d(g,g,matrix(GP[[3]],ng),color=tangocol[8],alpha=0.4)
surface3d(g,g,matrix(GP[[4]],ng),color=tangocol[8],alpha=0.4)
points3d(X[,1],X[,2],F,size=5)
points3d(X[,1],X[,2],0*F+min(GP[[3]]),size=5)
snapshot3d('model2.png')

## test
nt <- 6
Xt <- expand.grid(seq(1/nt/2,1-1/nt/2,length=nt), seq(1/nt/2,1-1/nt/2,length=nt))
Ft <- as.matrix(branin(Xt))

GPt <- GPRtrend(Xt,X,F,trend,kGauss,c(1*var(F),.24,.24))

K <- matrix(GPt[[2]],nt^2)#+diag(rep(100,nt^2))
P <- Re(eigen(K/100)$vectors)
D <- Re(eigen(K)$values)
D[D<0] <- Inf
res <- P%*%diag(1/sqrt(D))%*%t(P) %*% (branin(Xt)-GPt[[1]])

Q2(branin(Xt),GPt[[1]])

tikz('res_2.tex', standAlone = TRUE, width=5, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
hist(res,10,freq=FALSE,main='model 2',xlab='standardised residuals')
lines(-50:50/15,dnorm(-50:50/15))
dev.off()
tools::texi2dvi('res_2.tex',pdf=T)

#########################################################
## RKHS
#brownien pair
kBrownSym <- function(x,y,param){
  param[1]*1/2*(1 - pmax(1-outer(x,y,'pmin'),outer(x,y,'pmax'))) 
} 

#brownien impair
kBrownAsym <- function(x,y,param){
  param[1]*(kBrown(x,y,param) - 1/2*(1 - pmax(1-outer(x,y,'pmin'),outer(x,y,'pmax'))))
} 

x <- matrix(seq(from=0, to=1, length=401),ncol=1)
plot(seq())
