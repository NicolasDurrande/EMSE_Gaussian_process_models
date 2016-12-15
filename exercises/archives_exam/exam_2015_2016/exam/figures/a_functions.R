
lightblue <- rgb(114/255,159/255,207/255,.3) ; darkblue <- rgb(32/255,74/255,135/255,1) ; darkbluetr <- rgb(32/255,74/255,135/255,.3)
darkPurple <- "#5c3566" ; darkBlue <- "#204a87" ; darkGreen <- "#4e9a06" ; darkChocolate <- "#8f5902" ; darkRed  <- "#a40000" ; darkOrange <- "#ce5c00" ; darkButter <- "#c4a000"

## kernels

dist <- function(x,y,theta){
  dist2 <- matrix(0,dim(x)[1],dim(y)[1])
  for(i in 1:dim(x)[2]){
    dist2 <- dist2 + (outer(x[,i],y[,i],"-")/theta[i])^2
  }
  return(sqrt(dist2))
}

kBrown <- function(x,y,param=NULL){
  if(is.null(param)) param <-1
  param*outer(c(x),c(y),"pmin")
}

kExp <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  param[1]*exp(-dist(x,y,param[-1]))
}

kGauss <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  param[1]*exp(-.5*dist(x,y,param[-1])^2)
}

kMat32 <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  d <- sqrt(3)*dist(x,y,param[-1])
  return(param[1]*(1 + d)*exp(-d))
}

kMat52 <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  d <- sqrt(5)*dist(x,y,param[-1])
  return(param[1]*(1 + d +1/3*d^2)*exp(-d))
}

kWhite <- function(x,y,param=NULL){
  if(is.null(param)) param <- 1
  d <- dist(x,y,rep(1,dim(x)[2]))
  return(param*(d==0))
}

k0 <- function(x,y,param=NULL){
  if(is.null(param)) param = c(1,.2)         
  nx <- length(x)
  x <- matrix(x,nx,length(y))
  y <- matrix(y,nx,length(y),byrow=T)
  z <- exp(-(x-y)^2/2/param[2]^2)
  int1 <- sqrt(2*pi)*param[2] * (pnorm(1,x,param[2]) - pnorm(0,x,param[2]))
  int2 <- sqrt(2*pi)*param[2] * (pnorm(1,y,param[2]) - pnorm(0,y,param[2]))
  intint <- param[2]^2 * (sqrt(2*pi)/param[2]*(2*pnorm(1/param[2])-1) + 2*exp(-1/param[2]^2/2) - 2)
  return(param[1]*(z-int1*int2/intint))
}

dkGauss <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  x <- as.vector(x)
  y <- as.vector(y)
  d <- outer(x,y,'-')
  return(-param[1]/param[2]^2*d*exp(-.5*d^2/param[2]^2) )
}

ddkGauss <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  x <- as.vector(x)
  y <- as.vector(y)
  d <- outer(x,y,'-')
  return(param[1]/param[2]^2*(1-d^2/param[2]^2)*exp(-.5*d^2/param[2]^2) )
}


## GPR
GPR <- function(x,X,F,kern,param=NULL,kernNoise=kWhite,paramNoise=0){
  kxX <- kern(x,X,param)
  kXX_1 <- solve(kern(X,X,param) + kernNoise(X,X,paramNoise))
  m <- kxX %*% kXX_1 %*% F
  K <- kern(x,x,param) - kxX %*% kXX_1 %*% t(kxX)
  upp95 <- m + 1.96*sqrt(pmax(0,diag(K)))
  low95 <- m - 1.96*sqrt(pmax(0,diag(K)))
  return(list(mean=m,cov=K,lower95=low95,upper95=upp95))
}

GPRtrend <- function(x,X,F,trend,kern,param,kernNoise=kWhite,paramNoise=0){
  m <- trend(x) + kern(x,X,param)%*%solve(kern(X,X,param))%*%(F-trend(X))
  K <- kern(x,x,param) - kern(x,X,param)%*%solve(kern(X,X,param))%*%kern(X,x,param)  
  upp95 <- m + 1.96*sqrt(pmax(0,diag(K)))
  low95 <- m - 1.96*sqrt(pmax(0,diag(K)))
  return(list(m,K,low95,upp95))
}

plotGPR <- function(x,pred,xlab="",ylab="",ylim=NULL,add=FALSE){
  m <- pred$mean
  low95 <- pred$lower95
  upp95 <- pred$upper95
  if(is.null(ylim)) ylim <- range(low95-0.5,upp95+0.5)
  par(mar=c(4.5,5.1,1.5,1.5))
  if(!add){
    plot(x, m, type="n", xlab=xlab,ylab=ylab, ylim=ylim, cex.axis=1.5,cex.lab=2)
  }
  #plot(x, m, type="n", xlab="$x$",ylab=ylab, ylim=range(7.5,13.5),cex.axis=1.5,cex.lab=2)
  polygon(c(x,rev(x)),c(upp95,rev(low95)),border=NA,col=lightblue)
  lines(x,m,col=darkblue,lwd=3)
  lines(x,low95,col=darkbluetr)  
  lines(x,upp95,col=darkbluetr)  
}

GLSo <- function(X,F,trend,kern,param,kernNoise=kWhite,paramNoise=0){
  one <- 0*F+1
  K_1 <- solve(kern(X,X,param)+kernNoise(X,X,paramNoise))
  tr <- t(one)%*%K_1 %*% F / t(one)%*%K_1 %*% one
  return( tr[1,1])
}


