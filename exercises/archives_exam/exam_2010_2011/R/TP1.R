######## Cours 1 #########

library(DiceKriging)

###############################################
### FIG 0a

n <- 40
x <- seq(from=0, to=1, length=n)

y <- 5+2*x+rnorm(n)/8

plot(x,y)
lines(c(0,1),c(5,7),col="red")
#lines(x,y,lty=2)

###############################################
### FIG 0b

n <- 15
x <- seq(from=0, to=1, length=n)

y <- 5+2*x+cos(8*x)/8

plot(x,y)
lines(c(0,1),c(5,7),col="red")
#lines(x,y,lty=2)

###############################################
### FIG 1
n <- 200
x <- seq(from=0, to=1, length=n)

covtype <- "exp"
coef.cov <- c(theta <- 1)
sigma <- 1.5
trend <- c(intercept <- 0)
nugget <- 0.00000001   
formula <- ~1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=5, newdata=NULL)

plot(x, y[1,], type="l", col=1, ylab="y", ylim=range(y))

for (i in 2:5) {
	lines(x, y[i,], col=i)
}



###############################################
### FIG 2
n <- 200
x <- seq(from=0, to=1, length=n)

covtype <- "gauss"
coef.cov <- c(theta <- 0.2)
sigma <- 1.5
trend <- c(intercept <- 0)
nugget <- 0.00000001   
formula <- ~1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=5, newdata=NULL)

plot(x, y[1,], type="l", col=1, ylab="y", ylim=range(y))

for (i in 2:5) {
	lines(x, y[i,], col=i)
}

###############################################
### FIG 3
n <- 200
x <- seq(from=0, to=1, length=n)

covtype <- "matern3_2"
coef.cov <- c(theta <- 0.3/sqrt(3))
sigma <- 1.5
trend <- c(intercept <- 0)
nugget <- 0.00000001   
formula <- ~1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=5, newdata=NULL)

plot(x, y[1,], type="l", col=1, ylab="y", ylim=range(y))

for (i in 2:5) {
	lines(x, y[i,], col=i)
}

###############################################
### FIG 4
covtype <- c("exp", "matern3_2", "matern5_2", "gauss")

d <- 1
n <- 500
x <- seq(from=0, to=3, length=n)

par(mfrow=c(1,2))
plot(c(-x,x), rep(0,2*n), type="l", ylim=c(0,1), xlab="distance", ylab="covariance")

param <- 1
sigma2 <- 1

for (i in 1:length(covtype)) {
	covStruct <- covStruct.create(covtype=covtype[i], d=d, 
                                      coef.cov=param, coef.var=sigma2)
	y <- covMat1Mat2(as.matrix(x), as.matrix(0), covStruct)
	lines(c(-x[501-1:500],x), c(y[501-1:500],y), col=i, lty=i)
	}
legend(x=1.3, y=1, legend=covtype, col=1:length(covtype), 
       lty=1:length(covtype), cex=0.8)

plot(x, rep(0,n), type="l", ylim=c(-2.2, 2.2), xlab="input, x", 
     ylab="output, f(x)",col="white")
for (i in 1:length(covtype)) {
	model <- km(~1, design=data.frame(x=x), response=rep(0,n), covtype=covtype[i], 
		    coef.trend=0, coef.cov=param, coef.var=sigma2, nugget=1e-8)
	y <- simulate(model)
	lines(x, y, col=i)
}

################################################################"""
##############################################################"""



##################################################################"


### FIG 7
n <- 5
m <- 101
design <- seq(from=0, to=1, length=n)
response <- c(0.5, 0, 1.5, 3, 2)

covtype <- "gauss"
coef.cov <- 0.2
sigma <- 1.5

trend <- c(intercept <- 0)
model <- km(formula=~1, design=data.frame(x=design), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma^2)

newdata <- seq(from=0, to=1, length=m)
nsim <- 40
y <- simulate(model, nsim=nsim, newdata=newdata, cond=TRUE, nugget.sim=1e-5)

p <- predict(model, newdata=newdata, type="SK")

## 1
plot(design, response, pch=19, cex=0.5, col="black",ylim=range(y),xlim=range(newdata))
points(newdata[41], p$mean[41], pch=19, cex=0.5, col="red")

## 2
points(newdata[41], p$lower95[41], cex=0.5)
points(newdata[41], p$upper95[41], cex=0.5)

## 3
lines(newdata, p$mean, lwd=1,col="red")
lines(newdata, p$lower95, lwd=1,lty=2)
lines(newdata, p$upper95, lwd=1,lty=2)

### FIG 5
n <- 200
x <- seq(from=0, to=1, length=n)

covtype <- "gauss"
coef.cov <- c(theta <- 0.2)
sigma <- 1.5
trend <- c(intercept <- 0)
nugget <- 0.00000001   
formula <- ~1

model <- km(formula, design=data.frame(x=x), response=rep(0,n), covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2, nugget=nugget)
y <- simulate(model, nsim=50, newdata=NULL)

plot(x, y[1,], type="l", col=1, ylab="y", ylim=range(y))

for (i in 2:50) {
	lines(x, y[i,], col=i)
}


### FIG 6
n <- 5
m <- 101
design <- seq(from=0, to=1, length=n)
response <- c(0.5, 0, 1.5, 3, 2)

covtype <- "gauss"
coef.cov <- 0.2
sigma <- 1.5

trend <- c(intercept <- 0)
model <- km(formula=~1, design=data.frame(x=design), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma^2)

newdata <- seq(from=0, to=1, length=m)
nsim <- 40
y <- simulate(model, nsim=nsim, newdata=newdata, cond=TRUE, nugget.sim=1e-5)

## 1
plot(design, response, pch=19, cex=0.5, col="black",xlim=c(0,1),ylim=c(-1,4))

#plot(newdata, y[1,], col=1,type="l",ylim=c(-1,4))

## 2
for (i in 1:nsim) {
	lines(newdata, y[i,], col=i)
}

###############################################
#############################################"""
# Fin cours 1

### FIG 7bis

par(mfrow=c(1,1))
n <- 2
m <- 101
design <- c(0.1,0.2)
response <- c(0.5, 1)

covtype <- "gauss"
coef.cov <- 0.2
sigma <- 1.5

trend <- c(intercept <- 0)
model <- km(formula=~1, design=data.frame(x=design), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma^2)

newdata <- seq(from=0, to=1, length=m)
p <- predict(model, newdata=newdata, type="SK")

plot(design, response, pch=19, cex=0.5, col="black",ylim=c(-4,4),xlim=range(newdata))
lines(newdata, p$mean, lwd=2,col="red")
lines(newdata, p$lower95, lwd=2,lty=2)
lines(newdata, p$upper95, lwd=2,lty=2)

### FIG 8
n <- 5
m <- 101
design <- seq(from=0, to=1, length=n)
response <- c(0.5, 0, 1.5, 3, 2)

covtype <- "gauss"
coef.cov <- 0.2
sigma <- 1.5

trend <- c(intercept <- 0)
model <- km(formula=~1, design=data.frame(x=design), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma^2)

newdata <- seq(from=0, to=1, length=m)
nsim <- 40
y <- simulate(model, nsim=nsim, newdata=newdata, cond=TRUE, nugget.sim=1e-5)

p <- predict(model, newdata=newdata, type="SK")

plot(design, response,pch=19, cex=1, col="black",ylim=range(y),xlim=range(newdata))
lines(newdata, p$mean, lwd=2,col="red")
lines(newdata, p$lower95, lwd=2,lty=2)
lines(newdata, p$upper95, lwd=2,lty=2)



### FIG 9
n <- 5
m <- 101
design <- seq(from=0, to=1, length=n)
response <- c(0.5, 0, 1.5, 3, 2)
newdata <- seq(from=0, to=1, length=m)

covtype <- "gauss"
coef.cov <- 0.1
sigma <- 1.5

kk <- matrix(0,m,n)
for(i in 1:n){
kk[,i] <- dnorm(newdata-design[i], mean = 0, sd = coef.cov , log = FALSE)/3
}

plot(design, response, pch=19, cex=1, col="red",ylim=range(y),xlim=range(newdata))
lines(newdata,kk[,1],lty=1,lwd=2)
lines(newdata,kk[,2],lty=2,lwd=2)
lines(newdata,kk[,3],lty=3,lwd=2)
lines(newdata,kk[,4],lty=4,lwd=2)
lines(newdata,kk[,5],lty=5,lwd=2)


#######################################################################
## Inf du noyau sur les modèles de krigeage

### FIG 10
n <- 5
m <- 101
design <- seq(from=0, to=1, length=n)
response <- c(0.5, 0, 1.5, 3, 2)
trend <- c(intercept <- 0)
newdata <- seq(from=0, to=1, length=m)

## 1
covtype <- "gauss"
coef.cov <- 0.2
sigma <- 1.5

model <- km(formula=~1, design=data.frame(x=design), response=response, covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2)
y <- simulate(model, nsim=20, newdata=newdata, cond=TRUE, nugget.sim=1e-5)
p <- predict(model, newdata=newdata, type="SK")

# a
plot(design, response, pch=19, cex=0.5, col="black",ylim=range(y),xlim=range(newdata))
lines(newdata, p$mean, lwd=2,col="red")
lines(newdata, p$lower95, lwd=2,lty=2)
lines(newdata, p$upper95, lwd=2,lty=2)
title("Noyau gaussien")

for (i in 1:20) {
	lines(newdata, y[i,], col=i,lty=1)
}


## 2
covtype <- "exp"
coef.cov <- 0.2
sigma <- 1.5

model <- km(formula=~1, design=data.frame(x=design), response=response, covtype=covtype, coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2)
y <- simulate(model, nsim=20, newdata=newdata, cond=TRUE, nugget.sim=1e-5)
p <- predict(model, newdata=newdata, type="SK")

# a
plot(design, response, pch=19, cex=0.5, col="black",ylim=range(y),xlim=range(newdata))
lines(newdata, p$mean, lwd=2,col="red")
lines(newdata, p$lower95, lwd=2,lty=2)
lines(newdata, p$upper95, lwd=2,lty=2)
title("Noyau exponentiel")

for (i in 1:20) {
	lines(newdata, y[i,], col=i,lty=1)
}



###################################################"
## Cas multidim

### FIG 11

d <- 2; n <- 16
fact.design <- expand.grid(seq(0,1,length=3), seq(0,1,length=3))
fact.design <- data.frame(fact.design); names(fact.design)<-c("x1", "x2")
branin.resp <- data.frame(branin(fact.design)); names(branin.resp) <- "y" 

m1 <- km(~1, design=fact.design, response=branin.resp, covtype="gauss")

# predicting at testdata points
testdata <- expand.grid(s <- seq(0,1, length=15), s)
predicted.values.model1 <- predict(m1, testdata, "UK")

library(rgl)

## 1
open3d()
persp3d(s,s,matrix(branin(testdata)$Var1,15,15),xlim=c(0,1),ylim=c(0,1),zlim=c(0,300),col="green",xlab="x",ylab="y",zlab="f")
branin

## 2
open3d()
plot3d(fact.design$x1,fact.design$x2, branin.resp$y,xlim=c(0,1),ylim=c(0,1),zlim=c(0,300),cex=3,xlab="x",ylab="y",zlab="m et v")
surface3d(s,s,matrix(predicted.values.model1$mean,15,15),alpha=1,col="red")
surface3d(s,s,matrix(predicted.values.model1$upper95,15,15),alpha=0.5,col="lightblue")
surface3d(s,s,matrix(predicted.values.model1$lower95,15,15),alpha=0.5,col="lightblue")

## 3
open3d()
persp3d(s,s,matrix(branin(testdata)$Var1,15,15),xlim=c(0,1),ylim=c(0,1),zlim=c(0,300),col="green",xlab="x",ylab="y",zlab="f et m")
surface3d(s,s,matrix(predicted.values.model1$mean,15,15),alpha=1,col="red")
points3d(fact.design$x1,fact.design$x2, branin.resp$y)





