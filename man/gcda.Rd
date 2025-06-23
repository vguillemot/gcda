\name{gcda}
\alias{gcda}
\title{
gCDA
}
\description{
Wrapper method for the graph Constrained Discriminant Analysis.
Model : 
\deqn{\hat \Sigma = \alpha S + (1-\alpha)(L_G + I)^{-1}}{hat Sigma = alpha S + (1-alpha)(L_G + I)^{-1}}
}
\usage{
gcda(X, y, Xnew, ynew, gr=NULL, gr1 = NULL, gr2 = NULL, alpha = 0.5, alpha1 = 0.5, alpha2 = 0.5, type = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ An \eqn{n \times p}{n x p} matrix representing the two class learning dataset. }
  \item{y}{ A vector of 1 and 2 (of length n) describing the class of each individual for the learning set.}
  \item{Xnew}{ An \eqn{n^{new} \times p}{n^{new} x p} matrix representing the two class learning dataset.}
  \item{ynew}{A vector of 1 and 2 (of length \eqn{n^{new}}) describing the class of each individual for the learning set.}
  \item{gr}{ A graph object (package igraph), linear gCDA.}
  \item{gr1}{The graph object (package igraph) for class 1, quadratic gCDA.}
  \item{gr2}{The graph object (package igraph) for class 2, quadratic gCDA.}
  \item{alpha}{ Float between 0 and 1 tuning the integration of the graph into the linear gCDA.}
  \item{alpha1}{Float between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{alpha2}{Float between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{type}{String, either equal to "linear" or "quadratic".}
}
\value{
The output of the function is a list containing the number of errors (err) and the classes predicted (yhat).
}
\references{
Vincent Guillemot, Arthur Tenenhaus, Laurent Le Brusquet, Vincent Frouin. 
Graph Constrained Discriminant Analysis: a new method for the integration 
of a graph into a classification process. (submitted)

}
\author{
Vincent Guillemot
}
\examples{
# rm(list=ls())
# require(gcda)

## An example to show a comparison between linear and quadratic gCDA

p <- 200 ; n1 <- 50 ; n2 <- 50 ; n <- n1+n2
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

# Small values for the parameters (close to 0)
res1 <- gcda(x,y,xtest,ytest,gr=truegraph,alpha=0.01,type="linear")
cat("error rate: ",res1$err/length(ytest),"\n")
res2 <- gcda(x,y,xtest,ytest,gr1=truegraph1,gr2=truegraph2,alpha1=0.01,alpha2=0.01,type="quadratic")
cat("error rate: ",res2$err/length(ytest),"\n")

# High values for the paraneters (close to 1)
res3 <- gcda(x,y,xtest,ytest,gr=truegraph,alpha=0.9,type="linear")
cat("error rate: ",res3$err/length(ytest),"\n")
res4 <- gcda(x,y,xtest,ytest,gr1=truegraph1,gr2=truegraph2,alpha1=0.9,alpha2=0.9,type="quadratic")
cat("error rate: ",res4$err/length(ytest),"\n")

}
\keyword{methods}
\keyword{multivariate}
