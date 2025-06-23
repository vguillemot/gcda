\name{gcda-package}
\alias{gcda-package}
\docType{package}
\title{
gCDA
}
\description{
The gCDA algorithm allows to integrate one or several graphs into a 
binary classification procedure.
}
\details{
There are two versions of the algorithm:
 \itemize{
   \item the first one is the linear gCDA, for which thee is only one 
   graph for both classes,
   \item the second one is the quadratic gCDA, for which there is one
   graph for each class.}
The given examples are based only on simulated data, whose model is quickly described here: \code{\link{gcda}}.
}
\author{
Vincent Guillemot
}
\references{
 \itemize{
   \item the function lda from the package MASS served as a basis to implement
         the linear and quadratic versions of gCDA,
   \item the function pseudoinverse from package corpcor is used in gCDA to compute
         inverse covariance matrices when needed.
   \item }
}
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
# Example with the linear version of gCDA

p <- 200 ; n1 <- 50 ; n2 <- 50 ; n <- n1+n2
# Use the igraph package to simulate random graphs
truegraph <- erdos.renyi.game(p, 3/p)

mu <- rep(0,p) ; mu2 <- 0.10*rnorm(p)
sigma  <- solve(graph.laplacian(truegraph)+diag(1,p,p))
# dataset x, there are two classes, each is modelled by a Gaussian
# p-variate random variable
x <- rbind(rmvnorm(n=n1,mu,sigma),rmvnorm(n=n2,mu2,sigma))
y <- rep(1:2,c(n1,n2))

# test dataset with 10 times more individuals than the original dataset
xtest <- rbind(rmvnorm(n=10*n1,mu,sigma),rmvnorm(n=10*n2,mu2,sigma))
ytest <- rep(1:2,10*c(n1,n2))

# Small value for the parameter (close to 0)
res1 <- gcda(x,y,xtest,ytest,gr=truegraph,alpha=0.01,type="linear")
cat("error rate: ",res1$err/length(ytest),"\n")
# High value for the paraneters (close to 1)
res2 <- gcda(x,y,xtest,ytest,gr=truegraph,alpha=0.90,type="linear")
cat("error rate: ",res2$err/length(ytest),"\n")



}
