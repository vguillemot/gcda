gcdalin <-
function(X,y,gr,alpha=0.5) {
  tol <- 1e-5
  n <- nrow(X) ; g <- ncol(X)
  sigma <- solve(graph.laplacian(gr)+diag(1,g,g))
  y <- as.numeric(as.factor(y))
  yfac <- as.factor(y) ; nlev <- nlevels(yfac) 
  prior <- as.vector(table(y))/n
  
  group.means <- tapply(X,list(rep(yfac,g),col(X)),mean)
  f1 <- sd(X - group.means[yfac,  ])
  scaling <- diag(1/f1,,g)
  # adjust to "unbiased" scaling of covariance matrix 
  Xw <- 1/sqrt(n-nlev)*(X - group.means[yfac,  ]) %*% scaling 
  Sw <- alpha*t(Xw)%*%Xw+(1-alpha)*(sigma)
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
                   scaling = scaling,  svd = Sw.s$d[1:rank], Sw= Sw, N = n)
}

