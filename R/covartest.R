covartest <-
function(Xlist,verbose=FALSE) {
    if (verbose) print(sapply(Xlist,rownames))
    # Vector of degrees of freedom
    ngs <- sapply(Xlist,nrow)-1

    # Number of classes
    g <- length(Xlist)
    mm <- sapply(Xlist,ncol)
    if (min(mm) != max(mm)) cat("Error: number of variables different from one dataset to another")

    # Number of variables
    m <- min(mm)
    n <- sum(ngs)

    # Matrix containing (i,j) when i < j, (i = 1,...,g and j = i,...,g)
    indmat <- which(upper.tri(matrix(0,g,g)),arr.ind=TRUE)

    # List of the sample covariances
    Slist <- lapply(Xlist, cov )

    # Total sample covariance matrix
    S <- matrix(0,m,m)
    for (ii in 1:length(Slist)) S <- S + ngs[ii]*Slist[[ii]]
    S <- 1/n * S

    # Vector of eta
    etas <- (ngs+2)*(ngs-1)

    # Statistic tnm
    ttnm  <- apply(indmat, 1, function(x) Alpha(Slist[[ x[1] ]], Slist[[ x[2] ]]) - Beta(ngs[x[1]],etas[x[1]],Slist[[ x[1] ]])  - Beta(ngs[x[2]],etas[x[2]],Slist[[ x[2] ]]) )
    tnm <- sum(ttnm)

    # Variance thetahatsq (theta hat square)
    a <-  n**2*( 1/((n+2)*(n-1)) ) * (tr(S%*%S) - (1/n)*(tr(S))**2)
    thetahatsq1 <- sum( apply(indmat,1,function(x) ( ( ngs[x[1]]+ngs[x[2]] )/( ngs[x[1]]*ngs[x[2]] ) )**2) )
    thetahatsq2 <- (g-1)*(g-2)*sum(1/ngs**2)
    thetahatsq  <- 4*a**2*( thetahatsq1 + thetahatsq2 )

    # Statistic tnmstar
    tnmstar <- tnm/( sqrt(thetahatsq) )
    pval <- 1-pnorm(tnmstar,0,1)

    list(tnmstar = tnmstar , pval=pval)
}

