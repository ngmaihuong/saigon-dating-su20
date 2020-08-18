library(flexclust)

distEuclidean <- function(x, centers)
{
  z <- matrix(0, nrow=nrow(x), ncol=nrow(centers))
  for(k in 1:nrow(centers)){
    z[,k] <- sqrt( colSums((t(x) - centers[k,])^2) )
  }
  z 
}

kmFam <- kccaFamily(dist = distEuclidean, cent = colMeans)

x <- matrix(rnorm(1000), ncol = 2)
cl1 <- kcca(x, 10, family = kmFam)
cl1
image(cl1)


distJaccard <- function(x, centers)
{
  nx <- nrow(x)
  nc <- nrow(centers)
  xc <- x %*% t(centers)
  denominator <-
    matrix(rowSums(x), nrow=nx, ncol=nc) +
    matrix(rowSums(centers), nrow=nx, ncol=nc, byrow=TRUE) - xc
  return(1 - xc/denominator)
}

centOptim01 <- function(x, dist)
{
  foo <- function(p)
    sum(dist(x, matrix(p, nrow=1)))
  optim(colMeans(x), foo, lower=0, upper=1, method="L-BFGS-B")$par
}

jaccFam <- kccaFamily(dist = distJaccard, cent = centOptim01)
ejacFam <- kccaFamily(dist = distJaccard, cent = colMeans)

cl2 <- kcca(x, k=10, family = ejacFam)
cl2
image(cl2)

