predictgcdalin <-
function(X,y,Xnew,ynew,gr,alpha){
  obj <-  gcdalin(X,y,gr,alpha)
  nlev <- length(obj$prior)
  prior <- obj$prior
  scaling <- obj$scaling

  group.means <- obj$means
  Sw <- obj$Sw
  S <- t(scaling) %*% (Sw) %*% scaling
  dd <-  t(apply(Xnew,1,function(x){ sapply(1:nlev, function(group) { x <- (x - group.means[group,])%*%(scaling); -t(x)%*%pseudoinverse(S)%*%(x)  })}))
  yhat <- max.col(dd)
  #print(rbind(as.numeric(as.factor(ynew)),yhat))
  
  error <- sum(as.numeric(as.factor(ynew)) != yhat)

  list(error=error,yhat=yhat)
}

