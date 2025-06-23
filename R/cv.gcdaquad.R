cv.gcdaquad <-
function(X,y,gr1,gr2,Alpha1,Alpha2,verbose=FALSE) {
    n <- length(y)
    nfold <- min(table(y),10)
    folds <- split(sample(1:n), rep(1:nfold, length = n))
    if(length(Alpha1)!=length(Alpha2)) print("Attention, length(Alpha1)!=length(Alpha2)")
    indices <- which(is.na(matrix(NA,length(Alpha1),length(Alpha2))),arr.ind=TRUE)
    #print(indices)
    params <-  cbind(Alpha1[indices[,1]],Alpha2[indices[,2]])#[sample(length(Alpha1)),]
    #print(params)
    cv.err <- matrix(NA,nfold,nrow(params))
    for (k in 1:nfold) {
        if (verbose) cat("Fold", k, ":")
        index <- folds[[k]]
        for (i in 1:nrow(params)) {
          cv.yhat <- predictgcdaquad(X[-index, ], y[-index], X[index,],y[index], gr1, gr2, params[i,1], params[i,2])$yhat
          #print(cbind(yhat,as.numeric(as.factor(y[index]))))
          cv.err[k,i] <- sum( cv.yhat  != as.numeric(as.factor(y[index])))
          #cat(params[i,],", ",cv.err[k,i],"\n")
        }
    }
    #print(cv.err)
    res <- apply(cv.err,2,mean)
#    print( params[which.min(res),])
    list(alphasopt = params[which.min(res),], er = res)
}

