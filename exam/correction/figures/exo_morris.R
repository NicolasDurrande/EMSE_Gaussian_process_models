
library(sensitivity)
library(tikzDevice)
 
########################################################
######################################################## 
poly2.fun <- function(X, b12, b11, b22){
  X[,1]  + b12*X[,1]*X[,2] + b11*X[,1]^2 + b22*X[,2]^2
}

m <- morris(model = poly2.fun, factors = 2, r = 20, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf = -0.5, bsup = 0.5, 
            b12 = 0, b11 = 1, b22 = 0)

fileName <- 'exo_morris1.tex'
# tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(4.5,4.5,3.5,1.5))
plot(m, xlim = c(0, 3), ylim = c(0, 2), cex = 2, main = "A")
abline(h = 0, lty = 2); abline(v = 0, lty = 2)
#dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
#file.remove(fileName)

# \node[text=drawColor,anchor=base,inner sep=0pt, outer sep=0pt, scale=  1.50] at (162.54,263.94) {\bfseries A};

# \node[text=drawColor,anchor=base west,inner sep=0pt, outer sep=0pt, scale=  1.50] at (156.62,  8.40) {$\mu^*$};

# \node[text=drawColor,rotate= 90.00,anchor=base west,inner sep=0pt, outer sep=0pt, scale=  1.50] at ( 15.60,148.57) {$\sigma$};


########################################################
########################################################


poly2.fun <- function(X, b12, b11, b22){
  X[,1] + 2* X[,2] + b12*X[,1]*X[,2] + b11*X[,1]^2 + b22*X[,2]^2
}

m <- morris(model = poly2.fun, factors = 2, r = 20, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf = -0.5, bsup = 0.5, 
            b12 = 2, b11 = 0, b22 = 0)

fileName <- 'exo_morris2.tex'
# tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(4.5,4.5,3.5,1.5))
plot(m, xlim = c(0, 3), ylim = c(0, 2), cex = 2, main = "B")
abline(h = 0, lty = 2); abline(v = 0, lty = 2)
# dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
# file.remove(fileName)  

########################################################
########################################################


poly2.fun <- function(X, b12, b11, b22){
  X[,1] - 2*X[,2] + b12*X[,1]*X[,2] + b11*X[,1]^2 + b22*X[,2]^2
}

m <- morris(model = poly2.fun, factors = 2, r = 20, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf = -0.5, bsup = 0.5, 
            b12 = 0, b11 = 2, b22 = 0)

fileName <- 'exo_morris3.tex'
# tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(4.5,4.5,3.5,1.5))
plot(m, xlim = c(0, 3), ylim = c(0, 2), cex = 2, main = "C")
abline(h = 0, lty = 2); abline(v = 0, lty = 2)
# dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
# file.remove(fileName)

########################################################
########################################################

# poly2.fun <- function(X) X[,1]^2
#  
# m <- morris(model = poly2.fun, factors = 2, r = 20, 
#             design = list(type = "oat", 
#                           levels = 5, 
#                           grid.jump = 1), 
#             binf = -0.5, bsup = 0.5)
# 
# plot(m, xlim = c(0, 3), ylim = c(0, 2), cex = 2, main = "D")
# abline(h = 0, lty = 2); abline(v = 0, lty = 2)

########################################################
########################################################

poly2.fun <- function(X) X[,1]^2 + X[,2]^2

m <- morris(model = poly2.fun, factors = 2, r = 20, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf = -0.5, bsup = 0.5)

fileName <- 'exo_morris4.tex'
# tikz(fileName, standAlone = TRUE, width=4, height=4)
par(mar=c(4.5,4.5,3.5,1.5))
plot(m, xlim = c(0, 3), ylim = c(0, 2), cex = 2, main = "D")
abline(h = 0, lty = 2); abline(v = 0, lty = 2)
# dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
# file.remove(fileName)

## tikz
