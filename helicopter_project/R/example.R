source("doe2pdf.R")

# example
Ww_bounds = c(20, 50)
Wl_bounds = c(30, 75)
Tl_bounds = c(50, 80)
alpha_bounds = c(-25, 25)
LB <- c(Ww_bounds[1], Wl_bounds[1], Tl_bounds[1], alpha_bounds[1])
UB <- c(Ww_bounds[2], Wl_bounds[2], Tl_bounds[2], alpha_bounds[2])

# DoE
n <- 10
X <- matrix(runif(n*4), nrow=n)
X <- t(apply(X, 1, function(x) return(LB + (UB-LB)*x)))

# Make pdf
drawHelicopters(X=X, groupName="HeureuxCopter")