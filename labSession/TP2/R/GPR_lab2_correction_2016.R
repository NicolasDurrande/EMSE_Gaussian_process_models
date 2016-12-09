library(DiceKriging)
library(rgl)
library(MASS)

######################################
######################################
## Part 1 
######################################
######################################

## plot of the test function
n.grid <- 50
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)

persp3d(x.grid, y.grid, matrix(response.grid, n.grid, n.grid),col="red",xlab="x1",ylab="x2",zlab="f(x)")

###############################################
#
# building models
#
###############################################

###############
## Choice of a Design of Experiment X

N <- 4^2   # nb points in the design

x_grid <- seq(0.05,0.95,length=sqrt(N))

X <- expand.grid(X1=x_grid, X2=x_grid) #+ matrix(runif(N,-0.1,0.1),ncol=2)
Y <- apply(X, 1, branin)

plot3d(X[,1],X[,2],Y,cex=24,xlab="X1",ylab="X2",zlab="Y")

###############
## Test various models

m1 <- km(~1, design=data.frame(X), response=data.frame(Y),
	     covtype="gauss",iso=TRUE)

m2 <- km(Y~X1+X2+X1*X2, design=data.frame(X), response=data.frame(Y),
	     covtype="gauss",iso=TRUE)

m3 <- km(Y~I((X1+X2-1)^2)-1, design=data.frame(X), response=data.frame(Y), 
	     covtype="gauss",iso=TRUE)

# models details can be obtained with
print(m3)

# predictions
n.grid <- 50
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
names(design.grid) <- c('X1','X2')
names(response.grid) <- 'Y'
predicted.values.model1 <- predict(m1, design.grid, "UK")
predicted.values.model2 <- predict(m2, design.grid, "UK")
predicted.values.model3 <- predict(m3, design.grid, "UK")

# plots
preds = list(predicted.values.model1,predicted.values.model2,predicted.values.model3)
for(i in 1:3){
	open3d()
	persp3d(x.grid, y.grid, matrix(preds[[i]]$mean, n.grid, n.grid),col="blue4",xlab="x1",ylab="x2",zlab=paste0("m",i))
	surface3d(x.grid, y.grid, matrix(response.grid, n.grid, n.grid),col="red",alpha=0.5)
	surface3d(x.grid, y.grid, matrix(preds[[i]]$lower95, n.grid, n.grid),col="lightblue",alpha=0.8)
	surface3d(x.grid, y.grid, matrix(preds[[i]]$upper95, n.grid, n.grid),col="lightblue",alpha=0.8)
	points3d(X[,1],X[,2],Y,cex=100)
}

###############################################
#
# models validation
#
###############################################

## make a test set
NT <- 1000
XT <- matrix(runif(2*NT),NT,2)
Br <- apply(XT, 1, branin)

M1 <- predict(m1, data.frame(XT), "UK")$mean
MSE1 <- mean((M1 - Br)^2)

M2 <- predict(m2, data.frame(XT), "UK")$mean
MSE2 <- mean((M2 - Br)^2)

M3 <- predict(m3, data.frame(XT), "UK")$mean
MSE3 <- mean((M3 - Br)^2)

round(c(MSE1,MSE2,MSE3),2)
# the third model is the best

###############################################
# testing residuals distribution

model <- m3

NT <- 100
XT <- matrix(runif(2*NT),NT,2)
Br <- apply(XT, 1, branin)
	
M <- predict(model, data.frame(X1=XT[,1], X2=XT[,2]), "UK")
SR <- (M$mean-Br)/M$sd ## standardised residuals

hist(SR,20,probability=TRUE,col="grey",main="standardised residuals")
xg <- seq(min(SR),max(SR),0.05)
lines(xg,dnorm(xg), col = "red", lwd = 2)


###############################################
# testing residuals covariances

model <- m3

SR <- matrix(0,100,2)
for(i in 1:100){
	NT <- 2
	XT <- matrix(runif(2*NT),NT,2)
	Br <- apply(XT, 1, branin)
	
	M <- predict(model, data.frame(X1=XT[,1], X2=XT[,2]), "UK", cov.compute =TRUE)
	C1 = solve(t(chol(M$cov)))
	SR[i,] <- C1 %*% (M$mean-Br)
}
plot(SR)

###############################################
#
# Computation of the mean
#
###############################################
NT <- 10000
XT <- matrix(runif(2*NT),NT,2)

#vraie
MV <- apply(XT, 1, branin)
mean(MV)

# Plan exp
mean(Y)

# Modèles
M13 <- predict(m3, data.frame(XT), "UK")$mean
mean(M13)

###############################################
# Loi de l'intégrale

NT <- 1000
XT <- matrix(runif(2*NT),NT,2)

MM <- predict(m3, data.frame(XT), "UK",cov.compute=TRUE)

SC <- mvrnorm(100,MM$mean,MM$cov)

var(rowMeans(SC))



###############################################
###############################################
###############################################
# Partie 2 : dataIRSN5D
###############################################
###############################################
###############################################
library(DiceEval)

data(testIRSN5D)

dataIRSN5D <- testIRSN5D
s <- 0.90

X <- dataIRSN5D[,1:5]
keff <- dataIRSN5D[,6]

M <- km(keff~1, design=data.frame(X), response=data.frame(keff), covtype="gauss")

plot.km(M)

#XT <- maximinLHS(1000,5)
XT <- data.frame(matrix(runif(100000),20000,5))
names(XT) <- names(X)
P <- predict(M, XT, "UK",cov.compute=F)

# sans prendre en compte les variances
mean(P$mean>s)

# borne max
mean(P$upper95>s)
max(P$upper95)


