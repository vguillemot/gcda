\name{cv.gcda}
\alias{cv.gcda}
\title{Wrapper function for the cross validation}
\description{Cross validation algorithm used to determine
the regularization parameters of the gCDA method.}
\usage{
cv.gcda(X, y, gr = NULL, gr1 = NULL, gr2 = NULL, Alpha = seq(0, 1, l = 4), Alpha1 = seq(0, 1, l = 4), Alpha2 = seq(0, 1, l = 4), type = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ An \eqn{n \times p}{n x p} matrix representing the two class learning dataset. }
  \item{y}{ A vector of 1 and 2 (of length n) describing the class of each individual for the learning set.}
  \item{gr}{ A graph object (package igraph), linear gCDA.}
  \item{gr1}{The graph object (package igraph) for class 1, quadratic gCDA.}
  \item{gr2}{The graph object (package igraph) for class 2, quadratic gCDA.}
  \item{Alpha}{ Vector of floats between 0 and 1 tuning the integration of the graph into the linear gCDA.}
  \item{Alpha1}{Vector of floats between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{Alpha2}{Vector of floats between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{type}{String, either equal to "linear" or "quadratic".}
}
\value{
The output of the function is a list containing the number of errors (er) and the optimal values selected for
the parameters (alphaopt - only one value - for the linear version or alphasopt - a vector of two values - 
for the quadratic version).
}
\author{
Vincent Guillemot
}
\examples{
# rm(list=ls())
# require(gcda)

## An example to show a comparison between linear and quadratic gCDA

p <- 10 ; n1 <- 50 ; n2 <- 50 ; n <- n1+n2
# Use the igraph package to simulate random graphs
truegraph1 <- erdos.renyi.game(p, 3/p)
truegraph2 <- erdos.renyi.game(p, 3/p)
truegraph <- graph.union(truegraph1,truegraph2)

mu <- rep(0,p) ; mu2 <- 0.10*rnorm(p)
sigma  <- solve(graph.laplacian(truegraph)+diag(1,p,p))
sigma1 <- solve(graph.laplacian(truegraph1)+diag(1,p,p))
sigma2 <- solve(graph.laplacian(truegraph2)+diag(1,p,p))
# dataset x, there are two classes, each is modelled by a Gaussian
# p-variate random variable
x <- rbind(rmvnorm(n=n1,mu,sigma1),rmvnorm(n=n2,mu2,sigma2))
y <- rep(1:2,c(n1,n2))

# test dataset with 10 times more individuals than the original dataset
xtest <- rbind(rmvnorm(n=10*n1,mu,sigma1),rmvnorm(n=10*n2,mu2,sigma2))
ytest <- rep(1:2,10*c(n1,n2))

# Cross validation - linear gCDA
cvlin  <- cv.gcda(x,y,gr=truegraph,Alpha=seq(0,1,length=5),type="linear")
reslin <- gcda(x,y,xtest,ytest,gr=truegraph,alpha=cvlin$alphaopt,type="linear")
cat("error rate: ",reslin$err/length(ytest),"\n")

# Cross validation - quadratic gCDA
cvquad <- cv.gcda(x,y,gr1=truegraph1,gr2=truegraph2,Alpha1=seq(0,1,length=5),Alpha2=seq(0,1,length=5),type="quadratic")
al <- cvquad$alphasopt
resquad <- gcda(x,y,xtest,ytest,gr1=truegraph1,gr2=truegraph2,alpha1=al[1],alpha2=al[2],type="quadratic")
cat("error rate: ",resquad$err/length(ytest),"\n")

}
\keyword{methods}
\keyword{multivariate}
