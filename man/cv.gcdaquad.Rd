\name{cv.gcdaquad}
\alias{cv.gcdaquad}
\title{Internal function}
\description{Cross validation, quadratic version}
\usage{
cv.gcdaquad(X, y, gr1, gr2, Alpha1, Alpha2, verbose = FALSE)
}
\arguments{
  \item{X}{ An \eqn{n \times p}{n x p} matrix representing the two class learning dataset. }
  \item{y}{ A vector of 1 and 2 (of length n) describing the class of each individual for the learning set.}
  \item{gr1}{The graph object (package igraph) for class 1, quadratic gCDA.}
  \item{gr2}{The graph object (package igraph) for class 2, quadratic gCDA.}
  \item{Alpha1}{Vector of floats between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{Alpha2}{Vector of floats between 0 and 1 tuning the integration of the graph into the quadratic gCDA.}
  \item{verbose}{verbose function, default to FALSE}
}
\keyword{internal}
