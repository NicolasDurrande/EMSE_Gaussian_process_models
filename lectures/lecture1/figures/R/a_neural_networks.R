library(tikzDevice)
library(MASS)
library(rgl)
source('a_functions.R')

x <- seq(from=-5, to=5, length=101)
Xg = as.matrix(expand.grid(seq(0,1,0.05),seq(0,1,0.05)))

##################################################################
sigmoid <- function(x){
  1/(1+exp(-x))
}

treeSigmoid = function(x,W,B){
  n = nrow(x)
  S1 = x
  for(i in 2:length(W)){
    nbL = nrow(W[[i]])
    S2 = matrix(0,n,nbL)
    for(j in 1:nbL){
      S2[,j] = sigmoid(S1 %*% W[[i]][j,]+B[[i]][j])[,1]
    }
    S1 = S2
  }
  return(S2)
}

vec2list <- function(x,nlayer){
  j <- 1
  w = list(rep(1,nlayer[1]))
  b = list(rep(0,nlayer[1]))
  for(i in 2:length(nlayer)){
    j2 <- j-1+nlayer[i]*nlayer[i-1]
    j3 <- j2+nlayer[i]
    w = c(w,list(matrix(x[j:j2],nlayer[i])))
    b = c(b,list(matrix(x[(j2+1):j3])))
    j <- j3+1
  } 
  return(list(w,b))
}

dimParam <- function(nblayer){
  dim <- 0
  for(i in 2:length(nlayer)){
    dim <- dim + nlayer[i]*nlayer[i-1] + nlayer[i]
  } 
  return(dim)
}

RSS <- function(x){
  WB <- vec2list(x,nlayer)
  sum((treeSigmoid(Xtrain,WB[[1]],WB[[2]])-Ytrain)^2)
}

##################################################################"
### sigmoide


# tikz('neuralnet_sigmoid.tex', standAlone = TRUE, width=5, height=4)
par(mar=c(4.5,5.1,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(x,sigmoid(x), ylab='sigmoid function',col=darkBlue, type='l', lwd = 2)
# dev.off()
# tools::texi2dvi('neuralnet_sigmoid.tex',pdf=T)


##################################################################"
### example of function:

nlayer <- c(2,4,5,1)
dim <- dimParam(nlayer)
WB = vec2list(runif(dim,-5,5),nlayer)

W = WB[[1]]
B = WB[[2]]

Y = treeSigmoid(Xg,W,B)

persp3d(seq(0,1,0.05),seq(0,1,0.05),matrix(Y,length(seq(0,1,0.05))),col='red',xlab='x1', ylab='x2', zlab='f(x)')
#rgl.snapshot('neuralnetwork.png')

###############################
## fit

Xtrain <- matrix(runif(15*2),ncol=2)
Ytrain <- treeSigmoid(Xtrain,W,B)

mod <- optim(rep(0,43),RSS,control = list(maxit=15000))
WBmod <- vec2list(mod$par,nlayer)
  
predmod <- treeSigmoid(Xg,WBmod[[1]],WBmod[[2]])

persp3d(seq(0,1,0.05),seq(0,1,0.05),matrix(Y,length(seq(0,1,0.05))),col='red',xlab='x1', ylab='x2', zlab='f(x)')
surface3d(seq(0,1,0.05),seq(0,1,0.05),matrix(predmod,length(seq(0,1,0.05))),col='lightblue',xlab='x1', ylab='x2', zlab='f(x)')
spheres3d(Xtrain[,1],Xtrain[,2],Ytrain,radius =0.02)

#rgl.snapshot('neuralnetworkfit.png')

