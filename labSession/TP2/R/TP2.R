library(DiceKriging)
library(lhs)
library(rgl)
library(MASS)

######################################
N <- 15

X <- maximinLHS(N,2)

Y <- matrix(N,1)

for(i in 1:N){
Y[i] <- branin(X[i,])
}

m1 <- km(~1, design=data.frame(X), response=data.frame(Y), covtype="gauss")

m2 <- km(Y~X1+X2+X1*X2, design=data.frame(X), response=data.frame(Y), covtype="gauss")

m3 <- km(Y~I((X1+X2-1)^2), design=data.frame(X), response=data.frame(Y), covtype="gauss")


# graphics 
n.grid <- 50
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
names(design.grid) <- c('X1','X2')
names(response.grid) <- 'Y'
predicted.values.model1 <- predict(m1, design.grid, "UK")
predicted.values.model2 <- predict(m2, design.grid, "UK")
predicted.values.model3 <- predict(m3, design.grid, "UK")


# Graph 1
persp3d(x.grid, y.grid, matrix(response.grid, n.grid, n.grid),col="red")

surface3d(x.grid, y.grid, matrix(predicted.values.model1$mean, n.grid, n.grid),col="lightblue")
surface3d(x.grid, y.grid, matrix(predicted.values.model2$mean, n.grid, n.grid),col="lightblue")
surface3d(x.grid, y.grid, matrix(predicted.values.model3$mean, n.grid, n.grid),col="lightblue")
points3d(X[,1],X[,2],Y,cex=24)

# Graph 2
persp3d(x.grid, y.grid, matrix(predicted.values.model3$mean, n.grid, n.grid),col="blue")
surface3d(x.grid, y.grid, matrix(predicted.values.model3$lower95, n.grid, n.grid),col="lightblue")
surface3d(x.grid, y.grid, matrix(predicted.values.model3$upper95, n.grid, n.grid),col="lightblue")
points3d(X[,1],X[,2],Y,cex=100)

###############################################
#
# test des modèles
#
###############################################
NT <- 1000
XT <- matrix(runif(2*NT),NT,2)
Br <- apply(XT, 1, branin)

M1 <- predict(m1, data.frame(XT), "UK")$mean
V1 <- predict(m1, data.frame(XT), "UK",cov.compute =TRUE)$cov
C1 = chol(V1)
RCR <- C1 %*% (M1-Br)

M2 <- predict(m2, data.frame(XT), "UK")$mean
V2 <- predict(m2, data.frame(XT), "UK")$sd
RCR <- (M2-Br)/V2

M3 <- predict(m3, data.frame(XT), "UK")$mean
V3 <- predict(m3, data.frame(XT), "UK")$sd
RCR <- (M3-Br)/V3


hist(RCR,20,probability=TRUE)
lines((0:100)/10-5,exp(-((0:100)/10-5)^2/2)/(sqrt(2*pi)), col = "red", lwd = 2)

qqnorm(RCR)
lines(c(-3,3),c(-3,3), col = "red", lwd = 2)

###############################################
# test residus

NT <- 10
XT <- matrix(runif(2*NT),NT,2)
Br <- apply(XT, 1, branin)

M1 <- predict(m1, data.frame(XT), "UK")$mean
V1 <- predict(m1, data.frame(XT), "UK",cov.compute =TRUE)$cov
C1 = chol(V1)
RCR <- C1 %*% (M1-Br)

M2 <- predict(m2, data.frame(XT), "UK")$mean
V2 <- predict(m2, data.frame(XT), "UK",cov.compute =TRUE)$cov
C2 = chol(V2)
RCR <- C2 %*% (M2-Br)

M3 <- predict(m3, data.frame(XT), "UK")$mean
V3 <- predict(m3, data.frame(XT), "UK",cov.compute =TRUE)$cov
C3 = chol(V3)
RCR <- C3 %*% (M3-Br)


hist(RCR,20,probability=TRUE)
lines((0:100)/10-5,exp(-((0:100)/10-5)^2/2)/(sqrt(2*pi)), col = "red", lwd = 2)

qqnorm(RCR)
lines(c(-3,3),c(-3,3), col = "red", lwd = 2)


###############################################
#
# Calcul de ma moyenne
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
M11 <- predict(m1, data.frame(XT), "UK")$mean
mean(M11)
var(M11)

M12 <- predict(m2, data.frame(XT), "UK")$mean
mean(M12)
var(M12)

M13 <- predict(m3, data.frame(XT), "UK")$mean
mean(M13)
var(M13)

###############################################
# Loi de l'intégrale

NT <- 3000
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


