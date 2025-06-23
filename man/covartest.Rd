\name{covartest}
\alias{covartest}
\title{Test used to compare the covariance matrices of several samples.}
\description{This test is further detailed in the given reference and, to our knowledge,
not implemented in R. It is particularly well adapted to datasets with more variables than
samples.}
\usage{
covartest(Xlist, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xlist}{A list containing the sample whose covariance matrices are to be compared.}
  \item{verbose}{A boolean indicating wether to print out some text outputs. Default to FALSE.}
}
\value{
The returned values are classical for such a test: 
\itemize{
  \item{tnmstar}{The statistics on which the test is based,}
  \item{pval}{The associated p-value.}
}
}
\references{Schott JR: A test for the equality of covariance matrices when the dimension 
is large relative to the sample sizes. Computational Statistics & Data Analysis. 
2007, 51:6535-6542.}
\author{Vincent Guillemot}
\examples{
# rm(list=ls())
# require(gcda)

## An example on simulated data

p <- 200 ; n1 <- 50 ; n2 <- 50 ; n <- n1+n2
# Use the igraph package to simulate random graphs
truegraph1 <- erdos.renyi.game(p, 3/p)
truegraph2 <- erdos.renyi.game(p, 3/p)
truegraph <- graph.union(truegraph1,truegraph2)

mu <- rep(0,p) ; mu2 <- 0.10*rnorm(p)
sigma  <- solve(graph.laplacian(truegraph)+diag(1,p,p))
sigma1 <- solve(graph.laplacian(truegraph1)+diag(1,p,p))
sigma2 <- solve(graph.laplacian(truegraph2)+diag(1,p,p))
# first case : sigma1 != sigma2
xlist <- list(rmvnorm(n=n1,mu,sigma1),rmvnorm(n=n2,mu2,sigma2))
covartest(xlist)

# second case : sigma1 = sigma2 = sigma
xlist <- list(rmvnorm(n=n1,mu,sigma),rmvnorm(n=n2,mu2,sigma))
covartest(xlist)

}
\keyword{multivariate}
