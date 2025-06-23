cv.gcdalin <-
function(X,y,gr,Alpha,verbose=FALSE) {
    n <- length(y)
    nfold <- min(table(y),10)
    folds <- split(sample(1:n), rep(1:nfold, length = n))
    cv.err <- matrix(NA,nfold,length(Alpha))
    for (k in 1:nfold) {
        if (verbose) cat("Fold", k, ":")
        index <- folds[[k]]
        for (i in 1:length(Alpha)) {
          cv.yhat <- predictgcdalin(X[-index, ], y[-index], X[index,],y[index], gr, Alpha[i])$yhat
          cv.err[k,i] <- sum( cv.yhat  != as.numeric(as.factor(y[index])))
          #cat(Alpha[i],", ",cv.err[k,i],"\n")
        }
    }
    #print(cv.err)
    res <- apply(cv.err,2,mean)
    list(alphaopt = Alpha[which.min(res)], er = res)
}

