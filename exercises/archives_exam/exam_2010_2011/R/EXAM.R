######## EXAM #########

library(DiceKriging)

###############################################
### FIG 1

rm(list=ls(all=TRUE))
library(DiceKriging)
library(MASS)
library(rgl)

d <- 2; n <- 40
s <- seq(0,1, length=n)

covtype <- "gauss"
sigma <- 1

coef.cov <- c(0.3,100)
CS <- covStruct.create(covtype, d, coef.cov, coef.var = sigma, nugget = NULL)
X <- expand.grid(s,s)
C1 <- covMatrix(as.matrix(X),CS)[[1]]


covtype <- "exp"
sigma <- 1

coef.cov <- c(100,0.4)
CS <- covStruct.create(covtype, d, coef.cov, coef.var = sigma, nugget = NULL)
X <- expand.grid(s,s)
C2 <- covMatrix(as.matrix(X),CS)[[1]]

C <- C1*C2

Y <- mvrnorm(3,rep(0,n^2),C)

i <- 0
i <- i+1
persp3d(s, s,matrix(Y[i,],n,n),xlim=c(0,1),ylim=c(0,1),zlim=c(-2,2),col="wheat",xlab="x1",ylab="x2",zlab="z",theta = 45,phi=30,ticktype = "detailed",nticks=2)
lines3d(s,0*s,matrix(Y[i,],n,n)[,1],col="red",lwd=4)
lines3d(0*s,s,matrix(Y[i,],n,n)[1,],col="blue",lwd=4)



###############################################
### FIG 2
n <- 201
x <- seq(from=0, to=1, length=n)
nugget <- 0.00001   
covtype <- "matern3_2"
coef.cov <- c(theta <- 0.1)
sigma <- 1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=1, newdata=NULL)

par(mfrow=c(1,3))

design <- x[1:10*10]
response <- y[1:10*10]


## 5 ku
trend <- c(0,0)   
formula <- ~1+x+I(x^2)
plot(design,response, ylim=c(-10,10),xlim=c(0,1))

modelku <- km(formula=formula, design=data.frame(x=design), response=response, covtype=covtype,coef.cov=coef.cov, coef.var=sigma^2)
pku <- predict(modelku, newdata=x, type="UK")
lines(x, pku$mean, lwd=1,col="red")
lines(x, pku$lower95, lwd=1,lty=2)
lines(x, pku$upper95, lwd=1,lty=2)
title("(a)")

## 3 ks
trend <- c(0)   
formula <- ~1

plot(design,response, ylim=c(-3,3),xlim=c(0,1))

modelks <- km(formula=formula, design=data.frame(x=design), response=response, covtype=covtype,coef.trend=trend,coef.cov=coef.cov, coef.var=sigma^2)
pks <- predict(modelks, newdata=x, type="SK")
lines(x, pks$mean, lwd=1,col="red")
lines(x, pks$lower95, lwd=1,lty=2)
lines(x, pks$upper95, lwd=1,lty=2)
title("(b)")

## 4 ko
trend <- c(0)   
formula <- ~1
plot(design,response, ylim=c(-3,3),xlim=c(0,1))

modelko <- km(formula=~1, design=data.frame(x=design), response=response, covtype=covtype,coef.cov=coef.cov,coef.trend=trend, coef.var=sigma^2)
pko <- predict(modelko, newdata=x, type="UK")
lines(x, pko$mean, lwd=1,col="red")
lines(x, pko$lower95, lwd=1,lty=2)
lines(x, pko$upper95, lwd=1,lty=2)
title("(c)")

par(mfrow=c(1,1))

mean(response)


