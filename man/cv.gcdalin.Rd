\name{cv.gcdalin}
\alias{cv.gcdalin}
\title{Internal function}
\description{Cross validation, linear version}
\usage{
cv.gcdalin(X, y, gr, Alpha, verbose = FALSE)
}
\arguments{
  \item{X}{ An \eqn{n \times p}{n x p} matrix representing the two class learning dataset. }
  \item{y}{ A vector of 1 and 2 (of length n) describing the class of each individual for the learning set.}
  \item{gr}{ A graph object (package igraph), linear gCDA.}
  \item{Alpha}{ Vector of floats between 0 and 1 tuning the integration of the graph into the linear gCDA.}
  \item{verbose}{verbose function, default to FALSE}
}
\keyword{internal}