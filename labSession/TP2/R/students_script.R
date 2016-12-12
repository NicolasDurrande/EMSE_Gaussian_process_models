library(DiceKriging)
library(rgl)
library(MASS)

######################################
######################################
## Partie 2 : fonction de Branin
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

