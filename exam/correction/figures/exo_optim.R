library(DiceKriging)
library(DiceOptim)
library(tikzDevice)

##################################################################"
### Modeles de krigeage

X <- matrix(c(-0.2,0.2))
F <- matrix(c(-0.2,0.2))

theta <- 0.2
sigma2 <- 1
trend <- c(0)

m1 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="exp", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma2)
p1 <- predict(m1, newdata=data.frame(x=0), type="SK",cov.compute=TRUE)

m2 <- km(~1, design=data.frame(x=X), response=data.frame(y=F), 
            covtype="gauss", coef.trend=trend, coef.cov=theta, 
            coef.var=sigma2)
p2 <- predict(m2, newdata=data.frame(x=0), type="SK",cov.compute=TRUE)

EI(data.frame(x=0),m1,type="SK")

EI(data.frame(x=0),m2,type="SK")

u = (min(m1@y) - p1$mean)/(p1$sd)
p1$sd * (u * pnorm(u) + dnorm(u))



m <- 201
x <- seq(from=-1.5, to=1.5, length=m)


fileName <- 'exo_optim.tex'
tikz(fileName, standAlone = TRUE, width=10, height=6)
par(mar=c(2.5,3.,3.5,1.5),cex.axis=1.5,cex.lab=2)
plot(x,pnorm(x),type="l",col="#204a87",ylim=c(0,1),lwd=2,xlab="",ylab="")
lines(x,dnorm(x),col="#a40000",lwd=2)
legend('topleft',c('$\\Phi$ : Standard Gaussian cdf ','$\\phi$ : Standard Gaussian pdf '),col=c("#204a87","#a40000"), lwd=2 ,cex=2)
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)
