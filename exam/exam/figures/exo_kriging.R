library(DiceKriging)
library(tikzDevice)
library(MASS)
source("a_newfunctions.R")

##################################################################"
### Modeles de krigeage
m <- 101
x <- matrix(seq(from=0, to=1, length=m))

n <- 5
X <- matrix(seq(from=0.1, to=0.5, length=n))
F <- matrix(c(0.5, 0, 1.5, 3, 2))

##
theta <- 0.3
sigma <- 3
trend <- c(0)

m1 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="matern5_2", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,noise.var=rep(0.2,n))
p1 <- predict(m1, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

m1 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="matern5_2", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,nugget=0.2)

fileName <- 'exo_kriging_1.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p1)
title("model C",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

##
theta <- 0.1
sigma <- 3
trend <- c(0,6)

m2 <- km(~., design=data.frame(x=X), response=data.frame(y=F), 
            covtype="gauss", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,nugget=0)
p2 <- predict(m2, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

fileName <- 'exo_kriging_2.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p2)
title("model D",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

##
theta <- 0.02
sigma <- 2
trend <- c(0)

m3 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="gauss", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,nugget=0)
p3 <- predict(m3, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

fileName <- 'exo_kriging_3.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p3)
title("model A",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)


##
theta <- 0.3
sigma <- 3
trend <- c(0)

m4 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="exp", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,nugget=0)
p4 <- predict(m4, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

fileName <- 'exo_kriging_4.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p4)
title("model E",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

##
theta <- 0.2
sigma <- 1
trend <- c(2)

m5 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="matern3_2", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,noise.var=rep(0.1,5))
p5 <- predict(m5, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

fileName <- 'exo_kriging_5.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p5)
title("model B",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

##
theta <- 0.2
sigma <- 3
trend <- c(0)

m6 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="matern5_2", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma^2,nugget=0)
p6 <- predict(m6, newdata=data.frame(x=x), type="SK",cov.compute=TRUE)

fileName <- 'exo_kriging_6.tex'
tikz(fileName, standAlone = TRUE, width=5, height=5)
par(mar=c(2.5,3.,3.5,1.5))
plotGPR(x,p6)
title("model F",cex.main=2)
points(X, F, pch=4, cex=1,lwd=3)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)


m1
m2
m3
m4






