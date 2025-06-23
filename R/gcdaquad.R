gcdaquad <-
function(X,y,gr1,gr2,alpha1=0.5,alpha2=0.5) {
  tol <- 1e-5
  n <- nrow(X) ; g <- ncol(X)
  y <- as.numeric(as.factor(y))
  sigma1 <- solve(graph.laplacian(gr1)+diag(1,g,g))
  sigma2 <- solve(graph.laplacian(gr2)+diag(1,g,g))
  yfac <- as.factor(y) ; nlev <- nlevels(yfac)
  prior <- as.vector(table(y))/n

  group.means <- tapply(X,list(rep(yfac,g),col(X)),mean)
  f1 <- sd(X - group.means[yfac,  ])
  scaling <- diag(1/f1,,g)
  Xw <- 1/sqrt(n-nlev)*(X - group.means[yfac,  ]) %*% scaling
  Sw1 <- alpha1*t(Xw[y==1,])%*%Xw[y==1,]+(1-alpha1)*(sigma1)
  Sw2 <- alpha2*t(Xw[y==2,])%*%Xw[y==2,]+(1-alpha2)*(sigma2)
  Sw <- Sw1 + Sw2
  Sw.s <- svd(Sw, nu = 0)
  rank <- sum(Sw.s$d > tol^2)
  if(rank == 0) stop("rank = 0: variables are numerically const")
  if(rank < g) warning("variables are collinear")
  scaling <- scaling %*% Sw.s$v[,1:rank]%*%diag(sqrt(1/Sw.s$d[1:rank]),,rank)
  xbar <- colSums(prior %*% group.means)
  fac <- 1/(nlev - 1)
  Xb <- sqrt((n * prior)*fac) * scale(group.means, center=xbar, scale=FALSE) %*% scaling
  Xb.s <- svd(Xb, nu = 0)
  rank <- sum(Xb.s$d > tol * Xb.s$d[1])
  if(rank == 0) stop("group means are numerically identical")
  scaling <- scaling %*% Xb.s$v[, 1:rank]
  list(prior = prior, means = group.means,
                   scaling = scaling,  svd = Sw.s$d[1:rank], Sw1= Sw1, Sw2=Sw2, N = n)
}

