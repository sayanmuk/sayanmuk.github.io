install.packages("plgp")
install.packages("mvtnorm")
install.packages("lhs")
library(plgp)
library(mvtnorm)
library(lhs)


n <- 100
X <- matrix(seq(0, 10, length=n), ncol=1)
D <- distance(X)

eps <- sqrt(.Machine$double.eps) 
Sigma <- exp(-D) + diag(eps, n) 

Y <- rmvnorm(1, sigma=Sigma)
plot(X, Y, type="l")

Y <- rmvnorm(1, sigma=Sigma)
plot(X, Y, type="l")

Y <- rmvnorm(1, sigma=Sigma)
plot(X, Y, type="l")

c(exp(-1^2), exp(-4^2))


Y <- rmvnorm(3, sigma=Sigma)
matplot(X, t(Y), type="l", ylab="Y")


n <- 8
X <- matrix(seq(0, 2*pi, length=n), ncol=1)
y <- sin(X)
D <- distance(X) 
Sigma <- exp(-D) + diag(eps, ncol(D))

XX <- matrix(seq(-0.5, 2*pi + 0.5, length=100), ncol=1)
DXX <- distance(XX)
SXX <- exp(-DXX) + diag(eps, ncol(DXX))

DX <- distance(XX, X)
SX <- exp(-DX) 

Si <- solve(Sigma)
mup <- SX %*% Si %*% y
Sigmap <- SXX - SX %*% Si %*% t(SX)

YY <- rmvnorm(100, mup, Sigmap)

q1 <- mup + qnorm(0.05, 0, sqrt(diag(Sigmap)))
q2 <- mup + qnorm(0.95, 0, sqrt(diag(Sigmap)))

matplot(XX, t(YY), type="l", col="gray", lty=1, xlab="x", ylab="y")
points(X, y, pch=20, cex=2)
lines(XX, sin(XX), col="blue")
lines(XX, mup, lwd=2)
lines(XX, q1, lwd=2, lty=2, col=2)
lines(XX, q2, lwd=2, lty=2, col=2)




nx <- 20
x <- seq(0, 2, length=nx)
X <- expand.grid(x, x)

D <- distance(X)
Sigma <- exp(-D) + diag(eps, nrow(X))

Y <- rmvnorm(2, sigma=Sigma)

par(mfrow=c(1,2)) 
persp(x, x, matrix(Y[1,], ncol=nx), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")
persp(x, x, matrix(Y[2,], ncol=nx), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")

X <- randomLHS(40, 2)
X[,1] <- (X[,1] - 0.5)*6 + 1
X[,2] <- (X[,2] - 0.5)*6 + 1
y <- X[,1]*exp(-X[,1]^2 - X[,2]^2)

xx <- seq(-2, 4, length=40)
XX <- expand.grid(xx, xx)

D <- distance(X)
Sigma <- exp(-D)

DXX <- distance(XX)
SXX <- exp(-DXX) + diag(eps, ncol(DXX))
DX <- distance(XX, X)
SX <- exp(-DX)

Si <- solve(Sigma)
mup <- SX %*% Si %*% y
Sigmap <- SXX - SX %*% Si %*% t(SX)

sdp <- sqrt(diag(Sigmap))

par(mfrow=c(1,2))
cols <- heat.colors(128)
image(xx, xx, matrix(mup, ncol=length(xx)), xlab="x1", ylab="x2", col=cols)
points(X[,1], X[,2])
image(xx, xx, matrix(sdp, ncol=length(xx)), xlab="x1", ylab="x2", col=cols)
points(X[,1], X[,2])

persp(xx, xx, matrix(mup, ncol=40), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")
