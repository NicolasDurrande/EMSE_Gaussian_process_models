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

## Actual KO
n <- 5
x <- as.matrix(seq(from=-.5, to=1.5, length=201))
X <- as.matrix(c(seq(from=0.10, to=0.20, length=n-1),0.95))
F <- sin(2*pi*X) + sin(4*pi*X) + 10

mu <- GLSo(X,F,trend,kGauss,c(1,0.1))

trend <- function(x){
  0*x+mu
}

GP <- GPRtrend(x,X,F,trend,kGauss,c(1,.1))
K_1 <- solve(kGauss(X,X,c(1,.1)))
tmp <- rep(1,length(x)) - kGauss(x,X,c(1,.1)) %*% K_1 %*% matrix(rep(1,n),ncol=1)
predvar <- pmax(0,diag(matrix(GP[[2]],length(x))))
GP[[3]] <- GP[[1]] - 1.96 * sqrt(predvar + diag(tmp %*% t(tmp)) / sum(K_1) )
GP[[4]] <- GP[[1]] + 1.96 * sqrt(predvar + diag(tmp %*% t(tmp)) / sum(K_1) )

tikz('trend_ko.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
lines(x,trend(x),lty=2)
dev.off()
tools::texi2dvi('trend_ko.tex',pdf=T)

## comp with bad options
GP <- GPR(x,X,F,kGauss,c(1,.1))

tikz('trend_badks.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi('trend_badks.tex',pdf=T)

# KS variance on KO model
GP <- GPRtrend(x,X,F,trend,kGauss,c(1,.1))

tikz('trend_badko.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"$Z(x)|Z(X)=F$")
points(X, F, pch=4, cex=1,lwd=3)
lines(x,trend(x),lty=2)
dev.off()
tools::texi2dvi('trend_badko.tex',pdf=T)

#######################################################
## Universal kriging
n <- 201
x <- seq(from=0, to=1, length=n)
trend <- c(1,5)
nugget <- 0.00000001   
formula <- ~x
covtype <- "gauss"
coef.cov <- c(theta <- 0.1)
sigma <- 1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=2, newdata=NULL)

N <- 7
design <- 1:N/(N+1)/2
design <- x[round(design*199+0.49)+1]
response <- y[1,round(design*199+0.49)+1]

tikz('trend_data.tex', standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(design,response, pch=4, cex=1,lwd=3,ylim=c(0,12),xlim=range(x))
dev.off()
tools::texi2dvi('trend_data.tex',pdf=T)

## KU
modelku <- km(formula=~x, design=data.frame(x=design), response=response, covtype=covtype,coef.trend=trend,coef.cov=coef.cov, coef.var=sigma^2)
pku <- predict(modelku, newdata=x, type="UK")

GP <- list(pku$mean,0,pku$lower95,pku$upper95)

tikz('trend_ku.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"")
points(design,response, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi('trend_ku.tex',pdf=T)

## ks known trend

modelku <- km(formula=~x, design=data.frame(x=design), response=response, covtype=covtype,coef.trend=trend,coef.cov=coef.cov, coef.var=sigma^2)
pku <- predict(modelku, newdata=x, type="SK")

GP <- list(pku$mean,0,pku$lower95,pku$upper95)

tikz('trend_kstrend.tex', standAlone = TRUE, width=8, height=5)
plotGPR(x,GP,"")
points(design,response, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi('trend_kstrend.tex',pdf=T)


